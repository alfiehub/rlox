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

use super::{environment::EnvironmentError, Environment};

#[derive(Debug, thiserror::Error)]
pub enum InterpreterError {
    #[error("Return is used for flow control")]
    Return(LoxType),
    #[error("Something unexpected.")]
    Unexpected(String),
    #[error("Uninitialized variable")]
    UninitializedVariable(String),
    #[error("Invalid unary opator")]
    InvalidUnaryOperator,
    #[error("Invalid binary opator")]
    InvalidBinaryOperator,
    #[error("Generic error")]
    GenericError(String),
}

impl From<EnvironmentError> for InterpreterError {
    fn from(value: EnvironmentError) -> Self {
        Self::GenericError(value.0)
    }
}

impl From<LoxTypeError> for InterpreterError {
    fn from(value: LoxTypeError) -> Self {
        Self::GenericError(value.0)
    }
}

pub struct Interpreter<T: std::io::Write = std::io::Stdout> {
    writer: T,
    pub locals: HashMap<IdentifierKey, usize>,
    builtins: HashMap<String, LoxType>,
    environment: Rc<RefCell<Environment>>,
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
        let mut builtins = HashMap::new();
        builtins.insert(
            "clock".to_string(),
            LoxType::NativeFunction {
                name: "clock".to_string(),
                arity: 0,
                func: |_| {
                    SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .unwrap()
                        .as_secs_f64()
                        .into()
                },
            },
        );
        Self {
            locals: HashMap::new(),
            environment: Environment::new(),
            builtins,
            writer,
        }
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
        // TODO: handle err
        Resolver::new(self).resolve(stmts.to_vec()).unwrap();
        for stmt in stmts {
            // TODO: avoid clone
            self.visit_statement(stmt.clone())?;
        }
        Ok(())
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
                writeln!(self.writer, "{}", output).unwrap();
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
                    if let Err(e) = self.visit_statement(stmt.clone()) {
                        match e {
                            InterpreterError::Return(value) => break value.clone(),
                            _ => return Err(e),
                        }
                    }
                    i += 1;
                    if i == stmts.len() {
                        break LoxType::Nil;
                    }
                };
                self.unnest_environment();
                Ok(return_value)
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
                            Statement::Function(ident, arg_idents, stmt),
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
            Statement::Class(ident, _) => {
                self.environment.borrow_mut().create(
                    ident.to_string(),
                    Some(LoxType::Class {
                        name: ident.to_string(),
                        arity: 0,
                    }),
                );
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
                        _ => {
                            return Err(InterpreterError::Unexpected(
                                "Operand must be a number.".to_string(),
                            ))
                        }
                    },
                    _ => return Err(InterpreterError::InvalidUnaryOperator),
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
                    _ => return Err(InterpreterError::InvalidBinaryOperator),
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
                    _ => return Err(InterpreterError::InvalidBinaryOperator),
                })
            }
            Expression::Grouping(expr) => self.visit_expression(*expr),
            Expression::Literal(lit) => Ok(match &lit.0.token_type {
                TokenType::Number(n) => (*n).into(),
                TokenType::String(s) => LoxType::String(s.to_string()),
                TokenType::True => true.into(),
                TokenType::False => false.into(),
                TokenType::Nil => LoxType::Nil,
                _ => {
                    return Err(InterpreterError::GenericError(
                        "Invalid literal".to_string(),
                    ))
                }
            }),
            Expression::Assign(ident, expr) => {
                let value = self.visit_expression(*expr)?;
                if let TokenType::Identifier(identifier) = &ident.0.token_type {
                    self.environment.borrow_mut().assign_at_distance(
                        identifier.clone(),
                        Some(value),
                        self.locals.get(&ident.into()).copied(),
                    )?;
                } else {
                    return Err(InterpreterError::Unexpected(format!(
                        "Expected identifier, got {:?}",
                        ident
                    )));
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
                            return Err(InterpreterError::Unexpected(format!(
                                "Expected {} arguments for {} but got {}.",
                                arity,
                                name,
                                args.len()
                            )));
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
                                return Err(InterpreterError::Unexpected(format!(
                                    "Expected {} arguments but got {}. {}",
                                    identifiers.len(),
                                    args.len(),
                                    expr
                                )));
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
                                    return Err(InterpreterError::Unexpected(
                                        "Identifier didnt contain correct TokenType.".to_string(),
                                    ));
                                }
                            }
                            self.visit_statement(*body)?
                        } else {
                            return Err(InterpreterError::Unexpected(
                                    format!("Expected statement in LoxType::Function to be of type Function, got {func:?}")
                                    ));
                        };
                        self.environment = old_environment;
                        return_value
                    }
                    LoxType::Class { .. } => LoxType::ClassInstance {
                        class: function.into(),
                        properties: Rc::new(RefCell::new(HashMap::new())),
                    },
                    _ => {
                        return Err(InterpreterError::Unexpected(
                            "Expected call on function.".to_string(),
                        ))
                    }
                };
                self.unnest_environment();
                Ok(return_value)
            }
            Expression::Variable(ident) => {
                if let Some(builtin_fn) = self.builtins.get(&ident.to_string()) {
                    Ok(builtin_fn.clone())
                } else {
                    match self
                        .environment
                        .borrow()
                        .get_at_distance(
                            &ident.to_string(),
                            self.locals.get(&(&ident).into()).copied(),
                        )
                        .map_err(|e| InterpreterError::GenericError(e.to_string()))?
                    {
                        Some(value) => Ok(value),
                        None => Err(InterpreterError::UninitializedVariable(format!(
                            "Uninitialized variable '{}'",
                            ident
                        ))),
                    }
                }
            }
            Expression::Get(expr, ident) => {
                let object = self.visit_expression(*expr)?;
                if let LoxType::ClassInstance {
                    properties, class, ..
                } = object
                {
                    properties.borrow().get(&ident.to_string()).cloned().ok_or(
                        InterpreterError::GenericError(format!(
                            "No property '{ident}' on {class} instance."
                        )),
                    )
                } else {
                    Err(InterpreterError::GenericError(
                        "Only instances have properties.".to_string(),
                    ))
                }
            }
            Expression::Set(expr, ident, value) => {
                let object = self.visit_expression(*expr)?;
                if let LoxType::ClassInstance { properties, .. } = object {
                    let value = self.visit_expression(*value)?;
                    properties.borrow_mut().insert(ident.to_string(), value);
                    Ok(LoxType::Nil)
                } else {
                    Err(InterpreterError::GenericError(
                        "Only instances have properties.".to_string(),
                    ))
                }
            }
        }
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
            assert!(
                result.eq(expected_result).unwrap(),
                "\"{}\" resulted in {}",
                expression,
                result
            );
        }
    }

    #[test]
    fn test_evaluate_statement() {
        let expressions = [
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
            assert!(
                result.eq(&LoxType::Nil).unwrap(),
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

    #[test]
    fn test_class_print() {
        let program = parse!(
            "
            class MyFancyClass {
                fancyMethod() {
                    return 123;
                }
            }
            print MyFancyClass;
            var fancyInstance = MyFancyClass();
            print fancyInstance;
            fancyInstance.prop = 123;
            print fancyInstance.prop;
            "
        );
        let mut output = Vec::new();
        Interpreter::new_from_writer(&mut output)
            .interpret(&program)
            .unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap(),
            "MyFancyClass\nMyFancyClass instance\n123\n".to_string()
        );
    }
}
