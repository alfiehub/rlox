use std::{
    collections::HashMap,
    time::{SystemTime, UNIX_EPOCH},
};

use anyhow::{bail, Result};

use crate::{
    ast::{Declaration, Expression, Statement},
    lox_type::LoxType,
    token::{Token, TokenType},
};

#[derive(Debug)]
struct Environment {
    values: Vec<HashMap<String, Option<LoxType>>>,
}

#[derive(Debug, thiserror::Error)]
enum InterpreterError {
    #[error("Return is used for flow control")]
    Return(LoxType),
}

impl Environment {
    fn new() -> Self {
        Self {
            values: vec![HashMap::new()],
        }
    }

    fn nest(&mut self) {
        self.values.push(HashMap::new());
    }

    fn unnest(&mut self) {
        self.values.pop();
    }

    fn get(&self, key: &str) -> Result<Option<LoxType>> {
        // Loop backwards until value
        for scope in self.values.iter().rev() {
            if let Some(value) = scope.get(key) {
                return match value {
                    Some(value) => Ok(Some(value.clone())),
                    None => bail!("Uninitialized variable '{}'.", key),
                };
            }
        }
        bail!("Undefined variable '{}'.", key);
    }

    fn create(&mut self, key: String, value: Option<LoxType>) {
        self.values.last_mut().unwrap().insert(key, value);
    }

    fn assign(&mut self, key: String, value: Option<LoxType>) -> Result<()> {
        for scope in self.values.iter_mut().rev() {
            if scope.contains_key(&key) {
                scope.insert(key, value);
                return Ok(());
            }
        }
        bail!("Undefined variable '{}'.", key);
    }
}

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut environment = Environment::new();
        environment.create(
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
        environment.nest();
        Self { environment }
    }

    pub fn interpret(&mut self, declarations: &[Declaration]) -> Result<()> {
        for decl in declarations {
            self.evaluate_declaration(decl)?;
        }
        Ok(())
    }

    fn evaluate_declaration(&mut self, decl: &Declaration) -> Result<LoxType> {
        match decl {
            Declaration::Variable(identifier, expr_option) => {
                if let TokenType::Identifier(identifier) = &identifier.0.token_type {
                    let value = expr_option
                        .as_ref()
                        .map(|expr| self.evaluate_expression(expr))
                        .transpose()?;
                    self.environment.create(identifier.clone(), value);
                } else {
                    bail!("Expected identifier, got {:?}", identifier);
                }
                Ok(LoxType::Nil)
            }
            Declaration::Statement(stmt) => self.evaluate_statement(stmt),
        }
    }

    fn evaluate_statement(&mut self, stmt: &Statement) -> Result<LoxType> {
        match stmt {
            Statement::Expression(expr) => {
                self.evaluate_expression(expr)?;
            }
            Statement::Print(expr) => {
                println!("{}", self.evaluate_expression(expr)?);
            }
            Statement::Block(declarations) => {
                self.environment.nest();
                let mut i = 0;
                let return_value = loop {
                    let decl = &declarations[i];
                    match self.evaluate_declaration(decl) {
                        Err(e) => match e.downcast_ref::<InterpreterError>() {
                            Some(InterpreterError::Return(value)) => break value.clone(),
                            _ => bail!(e),
                        },
                        _ => {}
                    }
                    i += 1;
                    if i == declarations.len() {
                        break LoxType::Nil;
                    }
                };
                self.environment.unnest();
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
                    self.environment
                        .create(identifier.clone(), Some(LoxType::Function(stmt.clone())));
                }
            }
            Statement::Return(expr) => {
                if let Some(return_expr) = expr {
                    bail!(InterpreterError::Return(
                        self.evaluate_expression(return_expr)?
                    ))
                } else {
                    bail!(InterpreterError::Return(LoxType::Nil))
                }
            }
        }
        Ok(LoxType::Nil)
    }

    fn evaluate_expression(&mut self, expr: &Expression) -> Result<LoxType> {
        Ok(match expr {
            Expression::Literal(literal) => match &literal.0.token_type {
                TokenType::Number(n) => (*n).into(),
                TokenType::String(s) => LoxType::String(s.to_string()),
                TokenType::True => true.into(),
                TokenType::False => false.into(),
                TokenType::Nil => LoxType::Nil,
                TokenType::Identifier(identifier) => match self.environment.get(&identifier)? {
                    Some(value) => value,
                    None => bail!("Uninitialized variable '{}'", identifier),
                },
                _ => bail!("Invalid literal"),
            },
            Expression::Grouping(expr) => self.evaluate_expression(expr)?,
            Expression::Unary(operator, expr) => {
                let right = self.evaluate_expression(expr)?;
                match operator.0.token_type {
                    TokenType::Bang => LoxType::Boolean(!right.is_truthy()),
                    TokenType::Minus => match right {
                        LoxType::Number(n) => LoxType::Number(-n),
                        _ => bail!("Operand must be a number."),
                    },
                    _ => bail!("Invalid unary operator"),
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
                    _ => bail!("Invalid binary operator."),
                }
            }
            Expression::Assignment(identifier, expr) => {
                let value = self.evaluate_expression(expr)?;
                if let TokenType::Identifier(identifier) = &identifier.0.token_type {
                    self.environment.assign(identifier.clone(), Some(value))?;
                } else {
                    bail!("Expected identifier, got {:?}", identifier);
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
                    _ => bail!("Invalid logical operator"),
                }
            }
            Expression::Call(expr, arguments) => {
                self.environment.nest();
                let function = self.evaluate_expression(expr)?;
                let return_value = match function {
                    LoxType::NativeFunction { name, arity, func } => {
                        if arity != arguments.len() {
                            bail!(
                                "Expected {} arguments for {} but got {}.",
                                arity,
                                name,
                                arguments.len()
                            )
                        }
                        let argument_values: Result<Vec<LoxType>> = arguments
                            .iter()
                            .map(|a| self.evaluate_expression(a))
                            .collect();
                        func(argument_values?)
                    }
                    LoxType::Function(func) => {
                        if let Statement::Function(_, identifiers, body) = func {
                            if arguments.len() != identifiers.len() {
                                bail!(
                                    "Expected {} arguments but got {}. {}",
                                    identifiers.len(),
                                    arguments.len(),
                                    expr
                                )
                            }
                            for (arg, id) in arguments.iter().zip(identifiers.iter()) {
                                let arg_value = self.evaluate_expression(arg)?;
                                if let Token {
                                    token_type: TokenType::Identifier(identifier),
                                    ..
                                } = &id.0
                                {
                                    self.environment
                                        .create(identifier.to_string(), Some(arg_value));
                                } else {
                                    bail!("Identifier didnt contain correct TokenType.")
                                }
                            }
                            self.evaluate_statement(&body)?
                        } else {
                            bail!("Expected statement in LoxType::Function to be of type Funtion")
                        }
                    }
                    _ => bail!("Expected call on function."),
                };
                self.environment.unnest();
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
        let program = "
            fun add(a, b, c) {
              print a + b + c;
            }

            add(1, 2, 3);
        ";
        let tokens = Scanner::scan(program).unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        Interpreter::new().interpret(&program).unwrap()
    }
}
