use std::{fmt::Display, collections::HashMap};

use anyhow::{Result, bail};

use crate::{ast::{Expression, Statement, Declaration}, token::TokenType};

#[derive(Clone, Debug, PartialEq)]
enum LoxType {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl LoxType {
    fn is_truthy(&self) -> bool {
        match self {
            LoxType::Nil => false,
            LoxType::Boolean(b) => *b,
            _ => true,
        }
    }
}

impl Display for LoxType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxType::Number(n) => write!(f, "{}", n),
            LoxType::String(s) => write!(f, "{}", s),
            LoxType::Boolean(b) => write!(f, "{}", b),
            LoxType::Nil => write!(f, "nil"),
        }
    }
}

pub struct Interpreter {
    environment: HashMap<String, LoxType>
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: HashMap::new(),
        }
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
                let value = if let Some(expr) = expr_option {
                    self.evaluate_expression(expr)?
                } else {
                    LoxType::Nil
                };
                if let TokenType::Identifier(identifier) = &identifier.0 {
                    self.environment.insert(identifier.clone(), value);
                } else {
                    bail!("Expected identifier, got {:?}", identifier);
                }
                Ok(LoxType::Nil)
            },
            Declaration::Statement(stmt) => self.evaluate_statement(stmt),
        }
    }

    fn evaluate_statement(&mut self, stmt: &Statement) -> Result<LoxType> {
        let value = match stmt {
            Statement::Expression(expr) | Statement::Print(expr) => self.evaluate_expression(expr)?
        };
        if let Statement::Print(expr) = stmt {
            println!("{}", value);
        }
        Ok(LoxType::Nil)
    }

    fn evaluate_expression(&mut self, expr: &Expression) -> Result<LoxType> {
        match expr {
            // TODO: avoid cloning here?
            Expression::Literal(literal) => match literal.0.clone() {
                TokenType::Number(n) => Ok(LoxType::Number(n)),
                TokenType::String(s) => Ok(LoxType::String(s)),
                TokenType::True => Ok(LoxType::Boolean(true)),
                TokenType::False => Ok(LoxType::Boolean(false)),
                TokenType::Nil => Ok(LoxType::Nil),
                TokenType::Identifier(identifier) => {
                    if let Some(value) = self.environment.get(&identifier) {
                        Ok((*value).clone())
                    } else {
                        bail!("Undefined variable '{}'", identifier);
                    }
                },
                _ => bail!("Invalid literal"),
            },
            Expression::Grouping(expr) => self.evaluate_expression(expr),
            Expression::Unary(operator, expr) => {
                let right = self.evaluate_expression(expr)?;
                match operator.0 {
                    TokenType::Bang => Ok(LoxType::Boolean(!right.is_truthy())),
                    TokenType::Minus => match right {
                        LoxType::Number(n) => Ok(LoxType::Number(-n)),
                        _ => bail!("Operand must be a number."),
                    },
                    _ => bail!("Invalid unary operator"),
                }
            },
            Expression::Binary(left, operator, right) => {
                let left = self.evaluate_expression(left)?;
                let right = self.evaluate_expression(right)?;

                match (left, right) {
                    (LoxType::Number(left), LoxType::Number(right)) => match operator.0 {
                        TokenType::Minus => Ok(LoxType::Number(left - right)),
                        TokenType::Slash => Ok(LoxType::Number(left / right)),
                        TokenType::Star => Ok(LoxType::Number(left * right)),
                        TokenType::Plus => Ok(LoxType::Number(left + right)),
                        TokenType::Greater => Ok(LoxType::Boolean(left > right)),
                        TokenType::GreaterEqual => Ok(LoxType::Boolean(left >= right)),
                        TokenType::Less => Ok(LoxType::Boolean(left < right)),
                        TokenType::LessEqual => Ok(LoxType::Boolean(left <= right)),
                        TokenType::BangEqual => Ok(LoxType::Boolean(left != right)),
                        TokenType::EqualEqual => Ok(LoxType::Boolean(left == right)),
                        _ => bail!("Invalid binary operator number."),
                    },
                    (LoxType::String(left), LoxType::String(right)) => match operator.0 {
                        TokenType::Plus => Ok(LoxType::String(format!("{}{}", left, right))),
                        TokenType::EqualEqual => Ok(LoxType::Boolean(left == right)),
                        TokenType::BangEqual => Ok(LoxType::Boolean(left != right)),
                        _ => bail!("Invalid binary operator for String."),
                    },
                    (LoxType::Boolean(left), LoxType::Boolean(right)) => match operator.0 {
                        TokenType::EqualEqual => Ok(LoxType::Boolean(left == right)),
                        TokenType::BangEqual => Ok(LoxType::Boolean(left != right)),
                        _ => bail!("Invalid binary operator for Boolean."),
                    },
                    (LoxType::Nil, LoxType::Nil) => match operator.0 {
                        TokenType::EqualEqual => Ok(LoxType::Boolean(true)),
                        TokenType::BangEqual => Ok(LoxType::Boolean(false)),
                        _ => bail!("Invalid binary operator for Nil."),
                    },
                    // TODO: allow number == string
                    _ => bail!("Invalid binary operator."),
                }
            }
            _ => bail!("Not implemented"),
        }
    }
}

mod tests {

    use crate::scanner::Scanner;
    use crate::parser::Parser;

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
        ];
        for (expression, expected_result) in expressions.iter().zip(results.iter()) {
            let scanner = Scanner::new();
            let tokens = scanner.scan(expression).unwrap();
            let mut parser = Parser::new(tokens);
            let expr = parser.expression().unwrap();
            let result = Interpreter::new().evaluate_expression(&expr).unwrap();
            assert_eq!(result, *expected_result, "\"{}\" resulted in {}", expression, result);
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
            let scanner = Scanner::new();
            let tokens = scanner.scan(expression).unwrap();
            let mut parser = Parser::new(tokens);
            let expr = parser.statement().unwrap();
            let result = Interpreter::new().evaluate_statement(&expr).unwrap();
            assert_eq!(result, LoxType::Nil, "\"{}\" resulted in {}", expression, result);
        }
    }
}
