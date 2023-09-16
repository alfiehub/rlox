use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use anyhow::Result;

use crate::{
    ast::{Declaration, Expression, Statement},
    lox_type::{LoxType, LoxTypeError},
    token::{Token, TokenType},
};

#[derive(Debug)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Option<LoxType>>,
}

struct EnvironmentError(String);
impl std::fmt::Display for EnvironmentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

macro_rules! environment_bail {
    ($msg:expr) => {
        return Err(EnvironmentError($msg.to_string()))
    };
}

impl From<EnvironmentError> for InterpreterError {
    fn from(value: EnvironmentError) -> Self {
        Self::GenericError(value.0)
    }
}

impl Environment {
    fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: None,
            values: HashMap::new(),
        }))
    }

    fn get(&self, key: &str) -> Result<Option<LoxType>, EnvironmentError> {
        match self.values.get(key) {
            Some(value) => match value {
                Some(value) => Ok(Some(value.clone())),
                None => environment_bail!(format!("Uninitialized variable '{}'.", key)),
            },
            None => match &self.parent {
                Some(parent) => parent.borrow().get(key),
                None => environment_bail!(format!("Undefined variable '{}'.", key)),
            },
        }
    }

    fn create(&mut self, key: String, value: Option<LoxType>) {
        self.values.insert(key, value);
    }

    fn assign(&mut self, key: String, value: Option<LoxType>) -> Result<(), EnvironmentError> {
        if self.values.contains_key(&key) {
            self.values.insert(key, value);
            Ok(())
        } else {
            match &self.parent {
                Some(parent) => parent.borrow_mut().assign(key, value),

                None => environment_bail!("Undefined variable '{key}'."),
            }
        }
    }
}

pub struct Interpreter<T: std::io::Write = std::io::Stdout> {
    writer: T,
    environment: Rc<RefCell<Environment>>,
}

#[derive(Debug, thiserror::Error)]
pub enum InterpreterError {
    #[error("Return is used for flow control")]
    Return(LoxType),
    #[error("Something unexpected.")]
    Unexpected(String),
    #[error("Uninitialized variable")]
    UninitializedVariable(String),
    #[error("Generic error")]
    GenericError(String),
}

macro_rules! generic_bail {
    ($msg:expr) => {
        return Err(InterpreterError::GenericError($msg))
    };
}

impl From<LoxTypeError> for InterpreterError {
    fn from(value: LoxTypeError) -> Self {
        Self::GenericError(value.0)
    }
}

impl<T: std::io::Write> From<T> for Interpreter<T> {
    fn from(value: T) -> Self {
        Self::new_from_writer(value)
    }
}

type InterpreterResult<T> = Result<T, InterpreterError>;

impl Interpreter<std::io::Stdout> {
    pub fn new() -> Self {
        std::io::stdout().into()
    }
}

impl<T: std::io::Write> Interpreter<T> {
    pub fn new_from_writer(writer: T) -> Self {
        let environment = Environment::new();
        environment.borrow_mut().create(
            "clock".to_string(),
            Some(LoxType::NativeFunction {
                name: "clock".to_string(),
                arity: 0,
                func: |_| {
                    SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .unwrap()
                        .as_secs_f64()
                        .into()
                },
            }),
        );
        let mut interpreter = Self { environment, writer };
        interpreter.nest_environment();
        interpreter
    }

    fn nest_environment(&mut self) {
        let child_environment = Environment::new();
        child_environment.borrow_mut().parent = Some(self.environment.clone());
        self.environment = child_environment
    }

    fn unnest_environment(&mut self) {
        if let Some(parent_environment) = &self.environment.clone().borrow().parent {
            self.environment = parent_environment.clone();
        } else {
            panic!("No parent environment to unnest to.")
        }
    }

    pub fn interpret(&mut self, declarations: &[Declaration]) -> InterpreterResult<()> {
        for decl in declarations {
            self.evaluate_declaration(decl)?;
        }
        Ok(())
    }

    fn evaluate_declaration(&mut self, decl: &Declaration) -> InterpreterResult<LoxType> {
        match decl {
            Declaration::Variable(identifier, expr_option) => {
                if let TokenType::Identifier(identifier) = &identifier.0.token_type {
                    let value = expr_option
                        .as_ref()
                        .map(|expr| self.evaluate_expression(expr))
                        .transpose()?;
                    self.environment
                        .borrow_mut()
                        .create(identifier.clone(), value);
                } else {
                    return Err(InterpreterError::Unexpected(format!(
                        "Expected identifier, got {:?}",
                        identifier
                    )));
                }
                Ok(LoxType::Nil)
            }
            Declaration::Statement(stmt) => self.evaluate_statement(stmt),
        }
    }

    fn evaluate_statement(&mut self, stmt: &Statement) -> InterpreterResult<LoxType> {
        match stmt {
            Statement::Expression(expr) => {
                self.evaluate_expression(expr)?;
            }
            Statement::Print(expr) => {
                let output = self.evaluate_expression(expr)?;
                write!(self.writer, "{}\n", output).unwrap();
            }
            Statement::Block(declarations) => {
                self.nest_environment();
                let mut i = 0;
                let return_value = loop {
                    let decl = &declarations[i];
                    match self.evaluate_declaration(decl) {
                        Err(e) => match e {
                            InterpreterError::Return(value) => break value.clone(),
                            _ => return Err(e),
                        },
                        _ => {}
                    }
                    i += 1;
                    if i == declarations.len() {
                        break LoxType::Nil;
                    }
                };
                self.unnest_environment();
                return Ok(return_value);
            }
            Statement::If(condition, then_branch, else_branch) => {
                if self.evaluate_expression(condition)?.is_truthy() {
                    self.evaluate_statement(then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.evaluate_statement(else_branch)?;
                }
            }
            Statement::While(condition, statement) => {
                while self.evaluate_expression(condition)?.is_truthy() {
                    self.evaluate_statement(statement)?;
                }
            }
            Statement::Function(identifier, _, _) => {
                if let TokenType::Identifier(identifier) = &identifier.0.token_type {
                    self.environment.borrow_mut().create(
                        identifier.clone(),
                        Some(LoxType::Function(stmt.clone(), self.environment.clone())),
                    );
                }
            }
            Statement::Return(expr) => {
                if let Some(return_expr) = expr {
                    return Err(InterpreterError::Return(
                        self.evaluate_expression(return_expr)?,
                    ));
                } else {
                    return Err(InterpreterError::Return(LoxType::Nil));
                }
            }
        }
        Ok(LoxType::Nil)
    }

    fn evaluate_expression(&mut self, expr: &Expression) -> InterpreterResult<LoxType> {
        Ok(match expr {
            Expression::Literal(literal) => match &literal.0.token_type {
                TokenType::Number(n) => (*n).into(),
                TokenType::String(s) => LoxType::String(s.to_string()),
                TokenType::True => true.into(),
                TokenType::False => false.into(),
                TokenType::Nil => LoxType::Nil,
                TokenType::Identifier(identifier) => {
                    match self
                        .environment
                        .borrow()
                        .get(&identifier)
                        .map_err(|e| InterpreterError::GenericError(e.to_string()))?
                    {
                        Some(value) => value,
                        None => {
                            return Err(InterpreterError::UninitializedVariable(format!(
                                "Uninitialized variable '{}'",
                                identifier
                            )))
                        }
                    }
                }
                _ => return Err(InterpreterError::GenericError(format!("Invalid literal"))),
            },
            Expression::Grouping(expr) => self.evaluate_expression(expr)?,
            Expression::Unary(operator, expr) => {
                let right = self.evaluate_expression(expr)?;
                match operator.0.token_type {
                    TokenType::Bang => LoxType::Boolean(!right.is_truthy()),
                    TokenType::Minus => match right {
                        LoxType::Number(n) => LoxType::Number(-n),
                        _ => generic_bail!("Operand must be a number.".to_string()),
                    },
                    _ => generic_bail!("Invalid unary operator".to_string()),
                }
            }
            Expression::Binary(left, operator, right) => {
                let left = self.evaluate_expression(left)?;
                let right = self.evaluate_expression(right)?;

                match operator.0.token_type {
                    TokenType::Minus => (left - right)?,
                    TokenType::Slash => (left / right)?,
                    TokenType::Star => (left * right)?,
                    TokenType::Plus => (left + right)?,
                    TokenType::Greater => left.gt(&right)?.into(),
                    TokenType::GreaterEqual => left.gte(&right)?.into(),
                    TokenType::Less => left.lt(&right)?.into(),
                    TokenType::LessEqual => left.lte(&right)?.into(),
                    TokenType::BangEqual => left.neq(&right)?.into(),
                    TokenType::EqualEqual => left.eq(&right)?.into(),
                    _ => generic_bail!("Invalid binary operator.".to_string()),
                }
            }
            Expression::Assignment(identifier, expr) => {
                let value = self.evaluate_expression(expr)?;
                if let TokenType::Identifier(identifier) = &identifier.0.token_type {
                    self.environment
                        .borrow_mut()
                        .assign(identifier.clone(), Some(value))?;
                } else {
                    generic_bail!(format!("Expected identifier, got {:?}", identifier));
                }
                LoxType::Nil
            }
            Expression::Logical(left, operator, right) => {
                let left = self.evaluate_expression(left)?;
                match operator.0.token_type {
                    TokenType::Or => {
                        if left.is_truthy() {
                            left
                        } else {
                            self.evaluate_expression(right)?
                        }
                    }
                    TokenType::And => {
                        if !left.is_truthy() {
                            left
                        } else {
                            self.evaluate_expression(right)?
                        }
                    }
                    _ => generic_bail!("Invalid logical operator".to_string()),
                }
            }
            Expression::Call(expr, arguments) => {
                self.nest_environment();
                let function = self.evaluate_expression(expr)?;
                let return_value = match function {
                    LoxType::NativeFunction { name, arity, func } => {
                        if arity != arguments.len() {
                            generic_bail!(format!(
                                "Expected {} arguments for {} but got {}.",
                                arity,
                                name,
                                arguments.len()
                            ))
                        }
                        let argument_values: InterpreterResult<Vec<LoxType>> = arguments
                            .iter()
                            .map(|a| self.evaluate_expression(a))
                            .collect();
                        func(argument_values?)
                    }
                    LoxType::Function(func, environment) => {
                        let old_environment = self.environment.clone();
                        self.environment = environment;
                        let return_value = if let Statement::Function(_, identifiers, body) = func {
                            if arguments.len() != identifiers.len() {
                                generic_bail!(format!(
                                    "Expected {} arguments but got {}. {}",
                                    identifiers.len(),
                                    arguments.len(),
                                    expr
                                ))
                            }
                            for (arg, id) in arguments.iter().zip(identifiers.iter()) {
                                let arg_value = self.evaluate_expression(arg)?;
                                if let Token {
                                    token_type: TokenType::Identifier(identifier),
                                    ..
                                } = &id.0
                                {
                                    self.environment
                                        .borrow_mut()
                                        .create(identifier.to_string(), Some(arg_value));
                                } else {
                                    generic_bail!(
                                        "Identifier didnt contain correct TokenType.".to_string()
                                    )
                                }
                            }
                            self.evaluate_statement(&body)?
                        } else {
                            generic_bail!(
                                "Expected statement in LoxType::Function to be of type Funtion"
                                    .to_string()
                            )
                        };
                        self.environment = old_environment;
                        return_value
                    }
                    _ => generic_bail!("Expected call on function.".to_string()),
                };
                self.unnest_environment();
                return_value
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use crate::scanner::Scanner;

    use super::*;

    macro_rules! parse {
        ($program:expr) => {{
            let tokens = Scanner::scan($program).unwrap();
            let mut parser = Parser::new(tokens);
            parser.parse().unwrap()
        }};
    }

    #[test]
    fn test_evaluate_expressions() {
        let expressions = vec![
            "6 / (2 - 1) + 2",
            "1 + 2 * 3",
            "1 + 2 * 3 - 4 / 2",
            "1 + 2 * 3 - 4 / 2 + 1",
            "true == false",
            "true != false",
            "true == !false",
            "2 < 3",
            "3 <= 3",
            "\"hello\" == \"hello\"",
            "\"hello\" == \"world\"",
            "nil == nil",
            "\"hello\" + \" world\"",
        ];
        let results = vec![
            LoxType::Number(8.0),
            LoxType::Number(7.0),
            LoxType::Number(5.0),
            LoxType::Number(6.0),
            LoxType::Boolean(false),
            LoxType::Boolean(true),
            LoxType::Boolean(true),
            LoxType::Boolean(true),
            LoxType::Boolean(true),
            LoxType::Boolean(true),
            LoxType::Boolean(false),
            LoxType::Boolean(true),
            LoxType::String("hello world".to_string()),
        ];
        for (expression, expected_result) in expressions.iter().zip(results.iter()) {
            let tokens = Scanner::scan(expression).unwrap();
            let mut parser = Parser::new(tokens);
            let expr = parser.expression().unwrap();
            let result = Interpreter::new().evaluate_expression(&expr).unwrap();
            assert_eq!(
                result.eq(expected_result).unwrap(),
                true,
                "\"{}\" resulted in {}",
                expression,
                result
            );
        }
    }

    #[test]
    fn test_evaluate_statement() {
        let expressions = vec![
            "6 / (2 - 1) + 2;",
            "1 + 2 * 3;",
            "1 + 2 * 3 - 4 / 2;",
            "1 + 2 * 3 - 4 / 2 + 1;",
            "true == false;",
            "print true != false;",
            "print true == !false;",
            "print (2 < 3);",
            "print 3 <= 3;",
            "print \"hello\" == \"hello\";",
            "print \"hello\" == \"world\";",
        ];
        for expression in expressions.iter() {
            let tokens = Scanner::scan(expression).unwrap();
            let mut parser = Parser::new(tokens);
            let expr = parser.statement().unwrap();
            let result = Interpreter::new().evaluate_statement(&expr).unwrap();
            assert_eq!(
                result.eq(&LoxType::Nil).unwrap(),
                true,
                "\"{}\" resulted in {}",
                expression,
                result
            );
        }
    }

    #[test]
    fn test_function_multiple_arguments() {
        let program = parse!(
            "
            fun add(a, b, c) {
              print a + b + c;
            }

            add(1, 2, 3);
        "
        );
        Interpreter::new().interpret(&program).unwrap()
    }

    #[test]
    fn test_nested_function_environment() {
        let program = parse!(
            "
            fun makeCounter() {
              var i = 0;
              fun count() {
                i = i + 1;
                print i;
              }

              return count;
            }
            var i = 100;
            var counter = makeCounter();
            counter();
        "
        );
        let mut interpreter = Interpreter::new();
        interpreter.interpret(&program).unwrap();
        match interpreter.environment.clone().borrow().get("i") {
            Ok(Some(LoxType::Number(n))) => assert_eq!(n, 100.0),
            _ => panic!("No i in environment"),
        }
    }

    #[test]
    fn test_fib_for_loop() {
        let program = parse!(
            "
                var a = 0;
                var temp;

                for (var b = 1; a < 10000; b = temp + b) {
                  temp = a;
                  a = b;
                }
                print a;
            "
        );
        let mut output = Vec::new();
        Interpreter::new_from_writer(&mut output).interpret(&program).unwrap();
        assert_eq!(String::from_utf8(output).unwrap(), "10946\n".to_string());
    }

    #[test]
    fn test_static_scope() {
        // `a` referenced in showA should be static
        let program = parse!(
            "
            var a = \"global\";
            {
              fun showA() {
                print a;
              }

              showA();
              var a = \"block\";
              showA();
            }
            "
        );
        let mut output = Vec::new();
        Interpreter::new_from_writer(&mut output).interpret(&program).unwrap();
        assert_eq!(String::from_utf8(output).unwrap(), "global\nglobal\n".to_string());
    }
}
