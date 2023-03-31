use anyhow::{Result, bail};

use crate::{ast::{Expression, Literal}, token::TokenType};

#[derive(Debug)]
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

struct Interpreter {}

impl Interpreter {
    fn interpret() {

    }

    fn evaluate_expression(expr: &Expression) -> Result<LoxType> {
        match expr {
            // TODO: avoid cloning here?
            Expression::Literal(literal) => match literal.0.clone() {
                TokenType::Number(n) => Ok(LoxType::Number(n)),
                TokenType::String(s) => Ok(LoxType::String(s)),
                TokenType::True => Ok(LoxType::Boolean(true)),
                TokenType::False => Ok(LoxType::Boolean(false)),
                TokenType::Nil => Ok(LoxType::Nil),
                _ => bail!("Invalid literal"),
            },
            Expression::Grouping(expr) => Self::evaluate_expression(expr),
            Expression::Unary(operator, expr) => {
                let right = Self::evaluate_expression(expr)?;
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
                let left = Self::evaluate_expression(left)?;
                let right = Self::evaluate_expression(right)?;

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
    fn test_evaluate_expression() {
        let code = "6 / (2 - 1) + 2".to_string();
        let scanner = Scanner::new();
        let tokens = scanner.scan(&code).unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.expression().unwrap();
        println!("Parsed expression {}", expr);
        let result = Interpreter::evaluate_expression(&expr).unwrap();
        println!("Computed result {:?}", result);
    }
}
