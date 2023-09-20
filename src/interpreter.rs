use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use anyhow::Result;

use crate::{
    ast::{Expression, Identifier, IdentifierKey, Statement},
    lox_type::{LoxType, LoxTypeError},
    resolver::Resolver,
    token::{Token, TokenType},
    visitor::Visitor,
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

    fn ancestor(&self, distance: usize) -> Result<Rc<RefCell<Self>>, EnvironmentError> {
        if distance > 1 {
            if let Some(parent) = &self.parent {
                parent.borrow().ancestor(distance - 1)
            } else {
                environment_bail!("No parent.")
            }
        } else {
            self.parent
                .as_ref()
                .map(|e| e.clone())
                .ok_or_else(|| EnvironmentError("No parent.".to_string()))
        }
    }

    fn get_at_distance(
        &self,
        key: &str,
        distance: Option<usize>,
    ) -> Result<Option<LoxType>, EnvironmentError> {
        if let Some(distance) = distance {
            if distance <= 1 {
                self.get(key)
            } else {
                self.ancestor(distance - 1)?.borrow().get(key)
            }
        } else {
            self.get(key)
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

    fn assign_at_distance(
        &mut self,
        key: String,
        value: Option<LoxType>,
        distance: Option<usize>,
    ) -> Result<(), EnvironmentError> {
        if let Some(distance) = distance {
            if distance == 1 {
                self.assign(key, value)
            } else {
                self.ancestor(distance - 1)?.borrow_mut().assign(key, value)
            }
        } else {
            self.assign(key, value)
        }
    }
}

pub struct Interpreter<T: std::io::Write = std::io::Stdout> {
    writer: T,
    pub locals: HashMap<IdentifierKey, usize>,
    environment: Rc<RefCell<Environment>>,
}

#[derive(Default, Debug, thiserror::Error)]
pub enum InterpreterError {
    #[error("Return is used for flow control")]
    Return(LoxType),
    #[error("Something unexpected.")]
    Unexpected(String),
    #[error("Uninitialized variable")]
    UninitializedVariable(String),
    #[error("Generic error")]
    GenericError(String),
    #[error("Default")]
    #[default]
    Default,
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

impl<T: std::io::Write> Visitor<Result<LoxType, InterpreterError>> for Interpreter<T> {
    fn visit_statement(&mut self, stmt: Statement) -> Result<LoxType, InterpreterError> {
        match stmt {
            Statement::Expression(expr) => {
                self.visit_expression(expr)?;
                Ok(LoxType::default())
            }
            Statement::Print(expr) => {
                let output = self.visit_expression(expr)?;
                write!(self.writer, "{}\n", output).unwrap();
                Ok(LoxType::default())
            }
            Statement::If(cond, then_branch, else_branch) => {
                if self.visit_expression(cond)?.is_truthy() {
                    self.visit_statement(*then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.visit_statement(*else_branch)?;
                }
                Ok(LoxType::default())
            }
            Statement::Block(stmts) => {
                self.nest_environment();
                let mut i = 0;
                let return_value = loop {
                    let stmt = &stmts[i];
                    // TODO: avoid clone
                    match self.visit_statement(stmt.clone()) {
                        Err(e) => match e {
                            InterpreterError::Return(value) => break value.clone(),
                            _ => return Err(e),
                        },
                        _ => {}
                    }
                    i += 1;
                    if i == stmts.len() {
                        break LoxType::Nil;
                    }
                };
                self.unnest_environment();
                return Ok(return_value);
            }
            Statement::While(cond, stmt) => {
                // TODO: avoid cloning
                while self.visit_expression(cond.clone())?.is_truthy() {
                    self.visit_statement(*stmt.clone())?;
                }
                Ok(LoxType::Nil)
            }
            Statement::Function(ident, arg_idents, stmt) => {
                if let TokenType::Identifier(identifier) = &ident.0.token_type {
                    self.environment.borrow_mut().create(
                        identifier.clone(),
                        Some(LoxType::Function(
                            Statement::Function(ident, arg_idents, stmt.into()),
                            self.environment.clone(),
                        )),
                    );
                }
                Ok(LoxType::default())
            }
            Statement::Return(expr) => {
                if let Some(return_expr) = expr {
                    Err(InterpreterError::Return(
                        self.visit_expression(return_expr)?,
                    ))
                } else {
                    Err(InterpreterError::Return(LoxType::Nil))
                }
            }
            Statement::Variable(ident, expr) => {
                if let TokenType::Identifier(ident) = &ident.0.token_type {
                    let value = expr
                        .as_ref()
                        // TODO: avoid clone
                        .map(|expr| self.visit_expression(expr.clone()))
                        .transpose()?;
                    self.environment.borrow_mut().create(ident.clone(), value);
                } else {
                    return Err(InterpreterError::Unexpected(format!(
                        "Expected identifier, got {ident:?}"
                    )));
                }
                Ok(LoxType::Nil)
            }
        }
    }

    fn visit_expression(&mut self, expr: Expression) -> Result<LoxType, InterpreterError> {
        match expr {
            Expression::Unary(op, expr) => {
                let right = self.visit_expression(*expr)?;
                Ok(match op.0.token_type {
                    TokenType::Bang => LoxType::Boolean(!right.is_truthy()),
                    TokenType::Minus => match right {
                        LoxType::Number(n) => LoxType::Number(-n),
                        _ => generic_bail!("Operand must be a number.".to_string()),
                    },
                    _ => generic_bail!("Invalid unary operator".to_string()),
                })
            }
            Expression::Binary(left_expr, op, right_expr) => {
                let left = self.visit_expression(*left_expr)?;
                let right = self.visit_expression(*right_expr)?;

                Ok(match op.0.token_type {
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
                })
            }
            Expression::Logical(left_expr, op, right_expr) => {
                let left = self.visit_expression(*left_expr)?;
                let right = self.visit_expression(*right_expr)?;

                Ok(match op.0.token_type {
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
                })
            }
            Expression::Grouping(expr) => self.visit_expression(*expr),
            Expression::Literal(lit) => Ok(match &lit.0.token_type {
                TokenType::Number(n) => (*n).into(),
                TokenType::String(s) => LoxType::String(s.to_string()),
                TokenType::True => true.into(),
                TokenType::False => false.into(),
                TokenType::Nil => LoxType::Nil,
                _ => return Err(InterpreterError::GenericError(format!("Invalid literal"))),
            }),
            Expression::Assign(ident, expr) => {
                let value = self.visit_expression(*expr)?;
                if let TokenType::Identifier(identifier) = &ident.0.token_type {
                    self.environment.borrow_mut().assign_at_distance(
                        identifier.clone(),
                        Some(value),
                        self.locals.get(&ident.into()).map_or(None, |d| Some(*d)),
                    )?;
                } else {
                    generic_bail!(format!("Expected identifier, got {:?}", ident));
                }
                Ok(LoxType::default())
            }
            Expression::Call(expr, args) => {
                self.nest_environment();
                // tODO: avoid clone
                let function = self.visit_expression(*expr.clone())?;
                let return_value = match function {
                    LoxType::NativeFunction { name, arity, func } => {
                        if arity != args.len() {
                            generic_bail!(format!(
                                "Expected {} arguments for {} but got {}.",
                                arity,
                                name,
                                args.len()
                            ))
                        }
                        let argument_values: InterpreterResult<Vec<LoxType>> =
                            args.into_iter().map(|a| self.visit_expression(a)).collect();
                        func(argument_values?)
                    }
                    LoxType::Function(func, environment) => {
                        let old_environment = self.environment.clone();
                        self.environment = environment;
                        let return_value = if let Statement::Function(_, identifiers, body) = func {
                            if args.len() != identifiers.len() {
                                generic_bail!(format!(
                                    "Expected {} arguments but got {}. {}",
                                    identifiers.len(),
                                    args.len(),
                                    expr
                                ))
                            }
                            for (arg, id) in args.into_iter().zip(identifiers.iter()) {
                                let arg_value = self.visit_expression(arg)?;
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
                            self.visit_statement(*body)?
                        } else {
                            generic_bail!(
                        format!("Expected statement in LoxType::Function to be of type Function, got {func:?}")
                    )
                        };
                        self.environment = old_environment;
                        return_value
                    }
                    _ => generic_bail!("Expected call on function.".to_string()),
                };
                self.unnest_environment();
                Ok(return_value)
            }
            Expression::Variable(ident) => {
                match self
                    .environment
                    .borrow()
                    .get_at_distance(
                        &ident.to_string(),
                        self.locals.get(&(&ident).into()).map_or(None, |d| Some(*d)),
                    )
                    .map_err(|e| InterpreterError::GenericError(e.to_string()))?
                {
                    Some(value) => Ok(value),
                    None => {
                        return Err(InterpreterError::UninitializedVariable(format!(
                            "Uninitialized variable '{}'",
                            ident
                        )))
                    }
                }
            }
        }
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
        let mut interpreter = Self {
            locals: HashMap::new(),
            environment,
            writer,
        };
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

    pub fn resolve(&mut self, ident: Identifier, distance: usize) {
        self.locals.insert(ident.into(), distance);
    }

    pub fn interpret(&mut self, stmts: &[Statement]) -> InterpreterResult<()> {
        Resolver::new(self).resolve(stmts.to_vec());
        for stmt in stmts {
            // TODO: avoid clone
            self.visit_statement(stmt.clone())?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::parse;
    use crate::parser::Parser;
    use crate::scanner::Scanner;

    use super::*;

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
            let result = Interpreter::new().visit_expression(expr).unwrap();
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
            let stmt = parser.statement().unwrap();
            let result = Interpreter::new().visit_statement(stmt).unwrap();
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
        Interpreter::new_from_writer(&mut output)
            .interpret(&program)
            .unwrap();
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
              print a;
            }
            "
        );
        let mut output = Vec::new();
        Interpreter::new_from_writer(&mut output)
            .interpret(&program)
            .unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap(),
            "global\nglobal\nblock\n".to_string()
        );
    }
}
