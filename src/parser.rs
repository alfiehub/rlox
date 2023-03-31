use anyhow::{bail, Result};

use crate::{
    ast::{Expression, Literal, Operator, UnaryOperator},
    token::{Token, TokenType},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<()> {
        if self.peek().token_type == token_type {
            self.advance();
            return Ok(());
        }
        bail!(message.to_owned());
    }

    pub fn expression(&mut self) -> Result<Expression> {
        self.equality()
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
                Expression::Literal(Literal(token.token_type.clone()))
            },
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
                Expression::Grouping(Box::new(expr))
            }
            _ => bail!("Expect expression."),
        };
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression> {
        Ok(match self.peek().token_type {
            TokenType::Bang | TokenType::Minus => {
                self.advance();
                let operator = self.previous();
                let right = self.unary()?;
                Expression::Unary(UnaryOperator(operator.token_type), Box::new(right))
            }
            _ => self.primary()?,
        })
    }

    fn factor(&mut self) -> Result<Expression> {
        let mut expr = self.unary()?;
        while let TokenType::Slash | TokenType::Star = self.peek().token_type {
            self.advance();
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expression::Binary(Box::new(expr), Operator(operator.token_type), Box::new(right));
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression> {
        let mut expr = self.factor()?;
        while let TokenType::Minus | TokenType::Plus = self.peek().token_type {
            self.advance();
            let operator = self.previous();
            let right = self.factor()?;
            expr = Expression::Binary(Box::new(expr), Operator(operator.token_type), Box::new(right));
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression> {
        let mut expr = self.term()?;
        while let TokenType::Greater | TokenType::GreaterEqual | TokenType::Less | TokenType::LessEqual = self.peek().token_type {
            self.advance();
            let operator = self.previous();
            let right = self.term()?;
            return Ok(Expression::Binary(Box::new(expr), Operator(operator.token_type), Box::new(right)));
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expression> {
        let mut expr = self.comparison()?;
        while let TokenType::BangEqual | TokenType::EqualEqual | TokenType::Comma = self.peek().token_type {
            self.advance();
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expression::Binary(Box::new(expr), Operator(operator.token_type), Box::new(right));
        }
        Ok(expr)
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
        Ok(())
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

    pub fn parse(tokens: Vec<Token>) -> Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::Scanner;

    #[test]
    fn test_parser_expression() {
        let code = "1 + 2 * 3".to_string();
        let scanner = Scanner::new();
        let tokens = scanner.scan(&code).unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.expression().unwrap();
        println!("{}", expr);
    }

    #[test]
    fn test_parser_comma_operator() {
        let code = "1 + 2, 3 + 4".to_string();
        let scanner = Scanner::new();
        let tokens = scanner.scan(&code).unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.expression().unwrap();
        println!("{}", expr);
    }

    #[test]
    fn test_long() {
        let code = "1 + 2 + 3 + 4 * 5 * 6 + 7 + 8".to_string();
        let scanner = Scanner::new();
        let tokens = scanner.scan(&code).unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.expression().unwrap();
        println!("{}", expr);
    }
}
