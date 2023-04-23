use anyhow::{bail, Result};

use crate::token::{TokenType, Token};

pub struct Scanner {}

impl Scanner {
    pub fn scan(source: &str) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        let mut line = 1;

        let mut iter = source.chars().peekable();
        while let Some(char) = iter.next() {
            match char {
                ' ' | '\r' | '\t' => (),
                '\n' => line += 1,
                '(' => tokens.push(Token::new_empty(TokenType::LeftParen, line)),
                ')' => tokens.push(Token::new_empty(TokenType::RightParen, line)),
                '{' => tokens.push(Token::new_empty(TokenType::LeftBrace, line)),
                '}' => tokens.push(Token::new_empty(TokenType::RightBrace, line)),
                ',' => tokens.push(Token::new_empty(TokenType::Comma, line)),
                '.' => tokens.push(Token::new_empty(TokenType::Dot, line)),
                '-' => tokens.push(Token::new_empty(TokenType::Minus, line)),
                '+' => tokens.push(Token::new_empty(TokenType::Plus, line)),
                ';' => tokens.push(Token::new_empty(TokenType::Semicolon, line)),
                '*' => tokens.push(Token::new_empty(TokenType::Star, line)),
                '!' => if let Some('=') = iter.peek() {
                    iter.next();
                    tokens.push(Token::new_empty(TokenType::BangEqual, line));
                } else {
                    tokens.push(Token::new_empty(TokenType::Bang, line));
                },
                '=' => if let Some('=') = iter.peek() {
                    iter.next();
                    tokens.push(Token::new_empty(TokenType::EqualEqual, line));
                } else {
                    tokens.push(Token::new_empty(TokenType::Equal, line));
                },
                '<' => if let Some('=') = iter.peek() {
                    iter.next();
                    tokens.push(Token::new_empty(TokenType::LessEqual, line));
                } else {
                    tokens.push(Token::new_empty(TokenType::Less, line));
                },
                '>' => if let Some('=') = iter.peek() {
                    iter.next();
                    tokens.push(Token::new_empty(TokenType::GreaterEqual, line));
                } else {
                    tokens.push(Token::new_empty(TokenType::Greater, line));
                },
                '/' => if let Some('/') = iter.peek() {
                    iter.next();
                    while let Some(ch) = iter.peek() {
                        if *ch == '\n' {
                            break;
                        }
                        iter.next();
                    }
                } else if let Some('*') = iter.peek() {
                    iter.next();
                    let mut nesting = 1;
                    while let Some(ch) = iter.next() {
                        if ch == '*' {
                            if let Some('/') = iter.peek() {
                                iter.next();
                                nesting -= 1;
                                if nesting == 0 {
                                    break;
                                }
                            }
                        } else if ch == '/' {
                            if let Some('*') = iter.peek() {
                                iter.next();
                                nesting += 1;
                            }
                        } else if ch == '\n' {
                            line += 1;
                        }
                    }
                } else {
                    tokens.push(Token::new_empty(TokenType::Slash, line));
                },
                '"' => if let Some(_) = iter.peek() {
                    let mut string = String::new();
                    // TODO: is a mutable closed here the best approach?
                    let mut closed = false;
                    while let Some(ch) = iter.next() {
                        if ch == '"' {
                            closed = true;
                            break;
                        }
                        if ch == '\n' {
                            line += 1;
                        }
                        string.push(ch);
                    }
                    if !closed {
                        bail!("Unterminated string");
                    }
                    tokens.push(Token::new(TokenType::String(string.clone()), string, line));
                } else {
                    bail!("Unterminated string");
                },
                '0'..='9' => {
                    let mut number = String::new();
                    number.push(char);
                    while let Some(ch) = iter.peek() {
                        if ch.is_digit(10) || *ch == '.' {
                            number.push(*ch);
                            iter.next();
                        } else {
                            break;
                        }
                    }
                    tokens.push(Token::new(TokenType::Number(number.parse::<f64>()?), number, line));
                },
                'A'..='Z' | 'a'..='z' => {
                    let mut identifier = String::new();
                    identifier.push(char);
                    while let Some(ch) = iter.peek() {
                        if ch.is_alphanumeric() {
                            identifier.push(*ch);
                            iter.next();
                        } else {
                            break;
                        }
                    }
                    tokens.push(
                        if let Some(keyword) = TokenType::from_identifier(&identifier) {
                            Token::new_empty(keyword, line)
                        } else {
                            Token::new(TokenType::Identifier(identifier.clone()), identifier, line)
                        }
                    )
                },
                _   => bail!("Unexpected charecter: {}", char),
            }
        }
        tokens.push(Token::new_empty(TokenType::Eof, line));
        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scan_simple() {
        let source = "( ) { } , . - + ; * ! != = == < <= > >= / //";
        let tokens = Scanner::scan(source).unwrap();
        assert_eq!(tokens.len(), 20);
    }

    #[test]
    fn test_scan_string() {
        let source = "\"hello world\"";
        let tokens = Scanner::scan(source).unwrap();
        assert_eq!(tokens.first().unwrap().token_type, TokenType::String("hello world".to_string()));
    }

    #[test]
    fn test_number() {
        let source = "123.45";
        let tokens = Scanner::scan(source).unwrap();
        assert_eq!(tokens.first().unwrap().token_type, TokenType::Number(123.45));
    }

    #[test]
    fn test_block_comment() {
        let source = "/* hello world */";
        let tokens = Scanner::scan(source).unwrap();
        assert_eq!(tokens.len(), 1);
    }

    #[test]
    fn test_nested_block_comment() {
        let source = "/* hello /* world */ */";
        let tokens = Scanner::scan(source).unwrap();
        assert_eq!(tokens.len(), 1);
    }

    #[test]
    fn test_deeply_nested_block_comment() {
        let source = "/* hello /* world /* /* /* lmao */ */ //// **** \n\n\n\n /* */ */ */ */";
        let tokens = Scanner::scan(source).unwrap();
        assert_eq!(tokens.len(), 1);
    }
}
