use anyhow::{bail, Result};

use crate::{
    ast::{Expression, Identifier, Literal, Operator, Statement, UnaryOperator},
    token::{Token, TokenType},
};

#[derive(Debug, thiserror::Error)]
enum ParserError {
    #[error("Unexpected token on line {line}. Found '{found}' instead of '{expected}'. {message}")]
    UnexpectedToken {
        line: usize,
        expected: TokenType,
        found: TokenType,
        message: String,
    },
    #[error("Unable to synchronize parser.")]
    Synchronize,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<()> {
        let token = self.peek();
        if token.token_type == token_type {
            self.advance();
            Ok(())
        } else {
            bail!(ParserError::UnexpectedToken {
                line: token.line,
                expected: token_type,
                found: token.token_type,
                message: message.to_string(),
            });
        }
    }

    pub fn expression(&mut self) -> Result<Expression> {
        self.assignment()
    }

    fn logic_or(&mut self) -> Result<Expression> {
        let expr = self.logic_and()?;
        let token = self.peek();
        if let TokenType::Or = token.token_type {
            self.advance();
            let right = self.logic_and()?;
            Ok(Expression::Logical(
                Box::new(expr),
                Operator(token),
                Box::new(right),
            ))
        } else {
            Ok(expr)
        }
    }

    fn logic_and(&mut self) -> Result<Expression> {
        let expr = self.equality()?;
        let token = self.peek();
        if let TokenType::And = token.token_type {
            self.advance();
            let right = self.equality()?;
            Ok(Expression::Logical(
                Box::new(expr),
                Operator(token),
                Box::new(right),
            ))
        } else {
            Ok(expr)
        }
    }

    pub fn assignment(&mut self) -> Result<Expression> {
        let expr = self.logic_or()?;
        let token = self.peek();
        match token.token_type {
            TokenType::Equal => {
                self.advance();
                let value = self.assignment()?;
                match expr {
                    Expression::Variable(ident) => {
                        return Ok(Expression::Assign(ident, Box::new(value)))
                    }
                    Expression::Get(object, ident) => {
                        return Ok(Expression::Set(object, ident, value.into()))
                    }
                    _ => {}
                }
                bail!("Invalid assignment target.");
            }
            _ => Ok(expr),
        }
    }

    fn primary(&mut self) -> Result<Expression> {
        let token = self.peek();
        let expr = match token.token_type {
            TokenType::False
            | TokenType::True
            | TokenType::Nil
            | TokenType::Number(_)
            | TokenType::String(_) => {
                self.advance();
                Expression::Literal(Literal(token))
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.consume(TokenType::RightParen, "Expected ')' after expression.")?;
                Expression::Grouping(Box::new(expr))
            }
            TokenType::Identifier(_) => {
                self.advance();
                Expression::Variable(Identifier(token))
            }
            _ => {
                bail!(
                    "Expected expression @ line: {}, found {:?}",
                    token.line,
                    token
                )
            }
        };
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression> {
        let token = self.peek();
        Ok(match token.token_type {
            TokenType::Bang | TokenType::Minus => {
                self.advance();
                let right = self.unary()?;
                Expression::Unary(UnaryOperator(token), Box::new(right))
            }
            _ => self.call()?,
        })
    }

    fn arguments(&mut self) -> Result<Vec<Expression>> {
        let mut expressions = vec![self.expression()?];
        while let TokenType::Comma = self.peek().token_type {
            if expressions.len() >= 255 {
                bail!("Cant have more than 255 argumets.")
            }
            self.advance();
            expressions.push(self.expression()?);
        }
        Ok(expressions)
    }

    fn call(&mut self) -> Result<Expression> {
        let mut expr = self.primary()?;
        loop {
            match self.peek().token_type {
                TokenType::LeftParen => {
                    self.advance();
                    let arguments = if TokenType::RightParen == self.peek().token_type {
                        vec![]
                    } else {
                        self.arguments()?
                    };
                    self.consume(TokenType::RightParen, "Expected ')' after arguments.")?;
                    expr = Expression::Call(expr.into(), arguments);
                }
                TokenType::Dot => {
                    self.advance();
                    expr = Expression::Get(expr.into(), self.identifier()?);
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression> {
        let mut expr = self.unary()?;
        while let TokenType::Slash | TokenType::Star = self.peek().token_type {
            self.advance();
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expression::Binary(Box::new(expr), Operator(operator), Box::new(right));
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression> {
        let mut expr = self.factor()?;
        while let TokenType::Minus | TokenType::Plus = self.peek().token_type {
            self.advance();
            let operator = self.previous();
            let right = self.factor()?;
            expr = Expression::Binary(Box::new(expr), Operator(operator), Box::new(right));
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression> {
        let mut expr = self.term()?;
        while let TokenType::Greater
        | TokenType::GreaterEqual
        | TokenType::Less
        | TokenType::LessEqual = self.peek().token_type
        {
            self.advance();
            let operator = self.previous();
            let right = self.term()?;
            expr = Expression::Binary(Box::new(expr), Operator(operator), Box::new(right));
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expression> {
        let mut expr = self.comparison()?;
        while let TokenType::BangEqual | TokenType::EqualEqual = self.peek().token_type {
            self.advance();
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expression::Binary(Box::new(expr), Operator(operator), Box::new(right));
        }
        Ok(expr)
    }

    pub fn statement(&mut self) -> Result<Statement> {
        let statement = match self.peek().token_type {
            TokenType::Return => {
                self.advance();
                let return_expr = if let TokenType::Semicolon = self.peek().token_type {
                    None
                } else {
                    Some(self.expression()?)
                };
                self.consume(TokenType::Semicolon, "Expected ';' after return.")?;
                Statement::Return(return_expr)
            }
            TokenType::Print => {
                self.advance();
                let expr = self.expression()?;
                self.consume(TokenType::Semicolon, "Expected ';' after expression.")?;
                Statement::Print(expr)
            }
            TokenType::LeftBrace => {
                self.advance();
                let mut statements = vec![];
                loop {
                    let stmt = self.declaration()?;
                    statements.push(stmt);
                    if self.is_at_end() {
                        break;
                    }
                    if self.peek().token_type == TokenType::RightBrace {
                        break;
                    }
                }
                self.consume(TokenType::RightBrace, "Expected '}' after block.")?;
                Statement::Block(statements)
            }
            TokenType::If => {
                self.advance();
                self.consume(TokenType::LeftParen, "Expected '(' after 'if'.")?;
                let condition = self.expression()?;
                self.consume(TokenType::RightParen, "Expected ')' after condition.")?;
                let then_branch = self.statement()?;
                let else_branch = if self.peek().token_type == TokenType::Else {
                    self.advance();
                    let stmt = self.statement()?;
                    Some(stmt)
                } else {
                    None
                };
                Statement::If(condition, Box::new(then_branch), else_branch.map(Box::new))
            }
            TokenType::While => {
                self.advance();
                self.consume(TokenType::LeftParen, "Expected '(' after 'while'.")?;
                let condition = self.expression()?;
                self.consume(TokenType::RightParen, "Expected ')' after condition.")?;
                let body = self.statement()?;
                Statement::While(condition, Box::new(body))
            }
            TokenType::For => {
                self.advance();
                self.consume(TokenType::LeftParen, "Expected '(' after 'for'.")?;
                let initializer = if self.peek().token_type == TokenType::Semicolon {
                    self.advance();
                    None
                } else if self.peek().token_type == TokenType::Var {
                    Some(self.declaration()?)
                } else {
                    Some(self.expression()?.into())
                };
                let condition = self.expression().ok();
                self.consume(TokenType::Semicolon, "Expected ';' after condition.")?;
                let increment = self.expression().ok();
                self.consume(TokenType::RightParen, "Expected ')' after ???.")?;
                let mut body = self.statement()?;

                if let Some(increment) = increment {
                    body = Statement::Block(vec![body.into(), increment.into()])
                };
                if let Some(condition) = condition {
                    body = Statement::While(condition, Box::new(body))
                };
                if let Some(initializer) = initializer {
                    body = Statement::Block(vec![initializer, body.into()])
                };

                body
            }
            _ => {
                let expr = self.expression()?;
                self.consume(TokenType::Semicolon, "Expected ';' after expression.")?;
                Statement::Expression(expr)
            }
        };
        Ok(statement)
    }

    fn identifier(&mut self) -> Result<Identifier> {
        let identifier = match self.peek().token_type {
            TokenType::Identifier(_) => Identifier(self.peek()),
            _ => bail!("Expected identfier."),
        };
        self.advance();
        Ok(identifier)
    }

    fn function(&mut self) -> Result<Statement> {
        let identifier = match self.peek().token_type {
            TokenType::Identifier(_) => self.peek(),
            _ => bail!("Expected identfier after fun."),
        };
        self.advance();

        self.consume(
            TokenType::LeftParen,
            "Expected '(' after function identifier",
        )?;
        let parameters = self.parameters()?;
        self.consume(
            TokenType::RightParen,
            "Expected ')' after function parameters",
        )?;

        let body = self.statement()?;
        Ok(Statement::Function(
            Identifier(identifier),
            parameters,
            body.into(),
        ))
    }

    fn parameters(&mut self) -> Result<Vec<Identifier>> {
        let mut identifiers = vec![];
        if let Ok(first_parameter) = self.identifier() {
            identifiers.push(first_parameter);
            while let TokenType::Comma = self.peek().token_type {
                self.advance();
                identifiers.push(self.identifier()?);
            }
        }
        Ok(identifiers)
    }

    pub fn declaration(&mut self) -> Result<Statement> {
        let declaration = if self.peek().token_type == TokenType::Var {
            self.advance();
            let identifier = match self.peek().token_type {
                TokenType::Identifier(_) => self.peek(),
                _ => bail!("Expected identfier after var."),
            };
            self.advance();
            let expr = match self.peek().token_type {
                TokenType::Equal => {
                    self.advance();
                    let expr = self.expression()?;
                    Some(expr)
                }
                _ => None,
            };
            self.consume(
                TokenType::Semicolon,
                "Expected ';' after variable declaration.",
            )?;
            Statement::Variable(Identifier(identifier), expr)
        } else if self.peek().token_type == TokenType::Fun {
            self.advance();
            self.function()?
        } else if self.peek().token_type == TokenType::Class {
            self.advance();
            let identifier = match self.peek().token_type {
                TokenType::Identifier(_) => self.peek(),
                _ => bail!("Expected identfier after class."),
            };
            self.advance();

            self.consume(
                TokenType::LeftBrace,
                "Expected '{' after function identifier",
            )?;

            let mut methods = vec![];
            while let Ok(fun) = self.function() {
                methods.push(fun);
                if self.is_at_end() {
                    break;
                }
                if self.peek().token_type == TokenType::RightBrace {
                    break;
                }
            }
            self.consume(
                TokenType::RightBrace,
                "Expected '}' after function identifier",
            )?;
            Statement::Class(Identifier(identifier), methods)
        } else {
            self.statement()?
        };
        Ok(declaration)
    }

    fn program(&mut self) -> Result<Vec<Statement>> {
        let mut declarations = vec![];
        loop {
            match self.declaration() {
                Ok(declaration) => {
                    declarations.push(declaration);
                    if self.is_at_end() {
                        break;
                    }
                }
                Err(err) => {
                    eprintln!("{}", err);
                    self.synchronize()?;
                }
            }
        }
        Ok(declarations)
    }

    fn synchronize(&mut self) -> Result<()> {
        self.advance();
        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return Ok(());
            } else {
                match self.peek().token_type {
                    TokenType::Class
                    | TokenType::Fun
                    | TokenType::Var
                    | TokenType::For
                    | TokenType::If
                    | TokenType::While
                    | TokenType::Print
                    | TokenType::Return => return Ok(()),
                    _ => (),
                }
                self.advance();
            }
        }
        bail!(ParserError::Synchronize)
    }

    fn is_at_end(&self) -> bool {
        // TODO: Check with EOF
        self.current == self.tokens.len() - 1
    }

    fn advance(&mut self) {
        if !self.is_at_end() {
            self.current += 1;
        }
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>> {
        self.program()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::Scanner;

    #[test]
    fn test_expressions() {
        let codes = vec![
            "1 + 2 * 3",
            "1 + 2 * 3 + 4",
            "1 + 2 + 3 + 4 * 5 * 6 + 7 + 8",
        ];
        let results = vec![
            "(+ 1 (* 2 3))",
            "(+ (+ 1 (* 2 3)) 4)",
            "(+ (+ (+ (+ (+ 1 2) 3) (* (* 4 5) 6)) 7) 8)",
        ];
        for (code, result) in codes.iter().zip(results.iter()) {
            let tokens = Scanner::scan(code).unwrap();
            let mut parser = Parser::new(tokens);
            let expr = parser.expression().unwrap();
            assert_eq!(expr.to_string(), *result);
        }
    }

    #[test]
    fn test_statements() {
        let codes = vec![
            "1 + 2 * 3;",
            "1 + 2* 3 + 4;",
            "print 1 + 2 + 3 + 4;",
            "print 1 + 2 + 3 + 4 * 5 * 6 + 7 + 8;",
        ];
        let results = vec![
            "(+ 1 (* 2 3))",
            "(+ (+ 1 (* 2 3)) 4)",
            "(print (+ (+ (+ 1 2) 3) 4))",
            "(print (+ (+ (+ (+ (+ 1 2) 3) (* (* 4 5) 6)) 7) 8))",
        ];
        for (code, result) in codes.iter().zip(results.iter()) {
            let tokens = Scanner::scan(code).unwrap();
            let mut parser = Parser::new(tokens);
            let statement = parser.statement().unwrap();
            assert_eq!(
                statement.to_string(),
                *result,
                "\"{}\" yielded {}",
                code,
                statement.to_string()
            );
        }
    }

    #[test]
    fn test_call() {
        let program = "
            someCall(1, 2, 3);
        ";
        let tokens = Scanner::scan(program).unwrap();
        let mut parser = Parser::new(tokens);
        let parsed = parser.parse().expect("Should be able to parse");
        let stmt = parsed.first().expect("Should be on statement");
        if let Statement::Expression(Expression::Call(_, args)) = stmt {
            assert_eq!(args.len(), 3, "Arguments of call should have length 3");
        } else {
            panic!()
        }
    }

    #[test]
    fn test_nested_function() {
        let program = "
            fun a() {
                fun b() {
                    return 1;
                }
                return b;
            }
            a()();
        ";
        let tokens = Scanner::scan(program).unwrap();
        let mut parser = Parser::new(tokens);
        parser.parse().expect("Should be able to parse");
    }

    #[test]
    fn test_class() {
        let program = "
            class EmptyClass {}
            class MyClass {
                classMethod() {
                    print 1;
                }
            }
        ";
        let tokens = Scanner::scan(program).unwrap();
        let mut parser = Parser::new(tokens);
        parser.parse().expect("Should be able to parse");
    }

    #[test]
    fn test_class_properties() {
        let program = "
            class MyClass {}
            var instance = MyClass();
            instance.test = 123;
            print instance.test;
        ";
        let tokens = Scanner::scan(program).unwrap();
        let mut parser = Parser::new(tokens);
        parser.parse().expect("Should be able to parse");
    }
}
