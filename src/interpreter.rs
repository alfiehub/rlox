use std::{collections::HashMap, fmt::Display};

use anyhow::{bail, Result};

use crate::{
    ast::{Declaration, Expression, Statement},
    token::TokenType,
};

#[derive(Debug, Clone)]
enum LoxType {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    Function(Statement),
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
            _ => todo!("Not implemented"),
        }
    }
}

#[derive(Debug)]
struct Environment {
    values: Vec<HashMap<String, Option<LoxType>>>,
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
        Self {
            environment: Environment::new(),
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
                if let TokenType::Identifier(identifier) = &identifier.0 {
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
            Statement::Print(expr) => println!("{}", self.evaluate_expression(expr)?),
            Statement::Block(declarations) => {
                self.environment.nest();
                for decl in declarations {
                    self.evaluate_declaration(decl)?;
                }
                self.environment.unnest();
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
                if let TokenType::Identifier(identifier) = &identifier.0 {
                    self.environment
                        .create(identifier.clone(), Some(LoxType::Function(stmt.clone())));
                }
            }
        };
        Ok(LoxType::Nil)
    }

    fn evaluate_expression(&mut self, expr: &Expression) -> Result<LoxType> {
        Ok(match expr {
            // TODO: avoid cloning here?
            Expression::Literal(literal) => match literal.0.clone() {
                TokenType::Number(n) => LoxType::Number(n),
                TokenType::String(s) => LoxType::String(s),
                TokenType::True => LoxType::Boolean(true),
                TokenType::False => LoxType::Boolean(false),
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
                match operator.0 {
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

                match (left, right) {
                    (LoxType::Number(left), LoxType::Number(right)) => match operator.0 {
                        TokenType::Minus => LoxType::Number(left - right),
                        TokenType::Slash => LoxType::Number(left / right),
                        TokenType::Star => LoxType::Number(left * right),
                        TokenType::Plus => LoxType::Number(left + right),
                        TokenType::Greater => LoxType::Boolean(left > right),
                        TokenType::GreaterEqual => LoxType::Boolean(left >= right),
                        TokenType::Less => LoxType::Boolean(left < right),
                        TokenType::LessEqual => LoxType::Boolean(left <= right),
                        TokenType::BangEqual => LoxType::Boolean(left != right),
                        TokenType::EqualEqual => LoxType::Boolean(left == right),
                        _ => bail!("Invalid binary operator number."),
                    },
                    (LoxType::String(left), LoxType::String(right)) => match operator.0 {
                        TokenType::Plus => LoxType::String(format!("{}{}", left, right)),
                        TokenType::EqualEqual => LoxType::Boolean(left == right),
                        TokenType::BangEqual => LoxType::Boolean(left != right),
                        _ => bail!("Invalid binary operator for String."),
                    },
                    (LoxType::Boolean(left), LoxType::Boolean(right)) => match operator.0 {
                        TokenType::EqualEqual => LoxType::Boolean(left == right),
                        TokenType::BangEqual => LoxType::Boolean(left != right),
                        _ => bail!("Invalid binary operator for Boolean."),
                    },
                    (LoxType::Nil, LoxType::Nil) => match operator.0 {
                        TokenType::EqualEqual => LoxType::Boolean(true),
                        TokenType::BangEqual => LoxType::Boolean(false),
                        _ => bail!("Invalid binary operator for Nil."),
                    },
                    // TODO: allow number == string
                    _ => bail!("Invalid binary operator."),
                }
            }
            Expression::Assignment(identifier, expr) => {
                let value = self.evaluate_expression(expr)?;
                if let TokenType::Identifier(identifier) = &identifier.0 {
                    self.environment.assign(identifier.clone(), Some(value))?;
                } else {
                    bail!("Expected identifier, got {:?}", identifier);
                }
                LoxType::Nil
            }
            Expression::Logical(left, operator, right) => {
                let left = self.evaluate_expression(left)?;
                match operator.0 {
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
                    LoxType::Function(func) => {
                        if let Statement::Function(_, identifiers, body) = func {
                            for (arg, id) in arguments.iter().zip(identifiers.iter()) {
                                let arg_value = self.evaluate_expression(arg)?;
                                self.environment.create(id.0.to_string(), Some(arg_value))
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
            _ => bail!("Not implemented"),
        })
    }
}

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
            let tokens = Scanner::scan(expression).unwrap();
            let mut parser = Parser::new(tokens);
            let expr = parser.expression().unwrap();
            let result = Interpreter::new().evaluate_expression(&expr).unwrap();
            // assert_eq!(
            //     result, *expected_result,
            //     "\"{}\" resulted in {}",
            //     expression, result
            // );
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
            // assert_eq!(
            //     result,
            //     LoxType::Nil,
            //     "\"{}\" resulted in {}",
            //     expression,
            //     result
            // );
        }
    }
}
