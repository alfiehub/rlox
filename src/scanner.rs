use std::pin::Pin;

use crate::token::{Token, TokenKind};

pub struct Scanner {
    // This field isn't dead, it's accessed through raw pointers
    #[allow(dead_code)]
    source: Pin<String>,
    pub start: *const u8,
    pub current: *const u8,
    pub line: isize,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        let source = std::pin::Pin::new(source);
        let start = source.as_ptr();

        Self {
            source,
            line: 1,
            start,
            current: start,
        }
    }

    pub fn scan(&mut self) {
        let mut line = -1;
        loop {
            let token = self.scan_token();
            if token.line != line {
                print!("{:>4} ", token.line);
                line = token.line;
            } else {
                print!("   | ");
            }
            let token_str = unsafe {
                std::str::from_utf8_unchecked(std::slice::from_raw_parts(token.start, token.length))
            };
            println!("{:>2} '{:.*}'", token.kind, token.length, token_str);
            if token.kind == TokenKind::TOKEN_EOF {
                break;
            }
        }
    }

    fn is_at_end(&self) -> bool {
        unsafe { *self.current == b'\0' }
    }

    pub fn advance(&mut self) -> u8 {
        unsafe {
            let value = *self.current;
            self.current = self.current.add(1);
            value
        }
    }

    fn matches(&mut self, c: u8) -> bool {
        unsafe {
            if self.is_at_end() || *self.current != c {
                false
            } else {
                self.current = self.current.add(1);
                true
            }
        }
    }

    fn peek(&self) -> u8 {
        unsafe { *self.current }
    }
    fn peek_next(&self) -> u8 {
        if self.is_at_end() {
            b'\0'
        } else {
            unsafe { *self.current.add(1) }
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();
            match c {
                b' ' | b'\r' | b'\t' => {
                    self.advance();
                }
                b'\n' => {
                    self.line += 1;
                    self.advance();
                }
                b'/' => {
                    if self.peek_next() == b'/' {
                        while self.peek() != b'\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    fn string(&mut self) -> Token {
        while self.peek() != b'"' && !self.is_at_end() {
            if self.peek() == b'\n' {
                self.line += 1
            }
            self.advance();
        }
        if self.is_at_end() {
            return Token::error("Unterminated string.", self);
        }
        self.advance();
        Token::new(TokenKind::TOKEN_STRING, self)
    }

    fn number(&mut self) -> Token {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == b'.' && self.peek_next().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        Token::new(TokenKind::TOKEN_NUMBER, self)
    }

    fn check_keyword(
        &mut self,
        start: usize,
        length: usize,
        rest: &str,
        token_kind: TokenKind,
    ) -> TokenKind {
        if self.current as usize - self.start as usize == start + length {
            let token_str = unsafe {
                std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                    self.start.add(start),
                    length,
                ))
            };
            if token_str == rest {
                return token_kind;
            }
        }
        TokenKind::TOKEN_IDENTIFIER
    }

    fn identifier_kind(&mut self) -> TokenKind {
        let start = unsafe { *self.start };
        match start {
            b'a' => self.check_keyword(1, 2, "nd", TokenKind::TOKEN_AND),
            b'c' => self.check_keyword(1, 4, "lass", TokenKind::TOKEN_CLASS),
            b'e' => self.check_keyword(1, 3, "lse", TokenKind::TOKEN_ELSE),
            b'i' => self.check_keyword(1, 1, "f", TokenKind::TOKEN_IF),
            b'n' => self.check_keyword(1, 2, "il", TokenKind::TOKEN_NIL),
            b'o' => self.check_keyword(1, 1, "r", TokenKind::TOKEN_OR),
            b'p' => self.check_keyword(1, 4, "rint", TokenKind::TOKEN_PRINT),
            b'r' => self.check_keyword(1, 5, "eturn", TokenKind::TOKEN_RETURN),
            b's' => self.check_keyword(1, 4, "uper", TokenKind::TOKEN_SUPER),
            b'v' => self.check_keyword(1, 2, "ar", TokenKind::TOKEN_VAR),
            b'w' => self.check_keyword(1, 4, "hile", TokenKind::TOKEN_WHILE),
            b'f' => {
                if self.current as usize - self.start as usize > 1 {
                    let second_char = unsafe { *self.start.add(1) };
                    match second_char {
                        b'a' => self.check_keyword(2, 3, "lse", TokenKind::TOKEN_FALSE),
                        b'o' => self.check_keyword(2, 1, "r", TokenKind::TOKEN_FOR),
                        b'u' => self.check_keyword(2, 1, "n", TokenKind::TOKEN_FUN),
                        _ => TokenKind::TOKEN_IDENTIFIER,
                    }
                } else {
                    TokenKind::TOKEN_IDENTIFIER
                }
            }
            b't' => {
                if self.current as usize - self.start as usize > 1 {
                    let second_char = unsafe { *self.start.add(1) };
                    match second_char {
                        b'h' => self.check_keyword(2, 2, "is", TokenKind::TOKEN_THIS),
                        b'r' => self.check_keyword(2, 2, "ue", TokenKind::TOKEN_TRUE),
                        _ => TokenKind::TOKEN_IDENTIFIER,
                    }
                } else {
                    TokenKind::TOKEN_IDENTIFIER
                }
            }
            _ => TokenKind::TOKEN_IDENTIFIER,
        }
    }

    fn identifier(&mut self) -> Token {
        while is_alpha(self.peek()) || self.peek().is_ascii_digit() {
            self.advance();
        }
        Token::new(self.identifier_kind(), self)
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();
        self.start = self.current;
        if self.is_at_end() {
            self.line += 1;
            return Token::new(TokenKind::TOKEN_EOF, self);
        }
        let c = self.advance();
        if is_alpha(c) {
            return self.identifier();
        } else if c.is_ascii_digit() {
            return self.number();
        };
        match c {
            b'(' => Token::new(TokenKind::TOKEN_LEFT_PAREN, self),
            b')' => Token::new(TokenKind::TOKEN_RIGHT_PAREN, self),
            b'{' => Token::new(TokenKind::TOKEN_LEFT_BRACE, self),
            b'}' => Token::new(TokenKind::TOKEN_RIGHT_BRACE, self),
            b';' => Token::new(TokenKind::TOKEN_SEMICOLON, self),
            b',' => Token::new(TokenKind::TOKEN_COMMA, self),
            b'.' => Token::new(TokenKind::TOKEN_DOT, self),
            b'-' => Token::new(TokenKind::TOKEN_MINUS, self),
            b'+' => Token::new(TokenKind::TOKEN_PLUS, self),
            b'/' => Token::new(TokenKind::TOKEN_SLASH, self),
            b'*' => Token::new(TokenKind::TOKEN_STAR, self),
            b'"' => self.string(),
            b'!' => Token::new(
                if self.matches(b'=') {
                    TokenKind::TOKEN_BANG_EQUAL
                } else {
                    TokenKind::TOKEN_BANG
                },
                self,
            ),
            b'=' => Token::new(
                if self.matches(b'=') {
                    TokenKind::TOKEN_EQUAL_EQUAL
                } else {
                    TokenKind::TOKEN_EQUAL
                },
                self,
            ),
            b'<' => Token::new(
                if self.matches(b'=') {
                    TokenKind::TOKEN_LESS_EQUAL
                } else {
                    TokenKind::TOKEN_LESS
                },
                self,
            ),
            b'>' => Token::new(
                if self.matches(b'=') {
                    TokenKind::TOKEN_GREATER_EQUAL
                } else {
                    TokenKind::TOKEN_GREATER
                },
                self,
            ),
            _ => Token::error("Unexpected character.", self),
        }
    }
}

fn is_alpha(byte: u8) -> bool {
    byte.is_ascii_alphabetic() || byte == b'_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scan_tokens() {
        let source = "
  // This is a comment
     (+)(+)--
     \"This is a string\"
     123.456
     super
     superb
     test
     while
     false
     for
     fun
     funky
     this
     true
     that
     !=
     <=
     >=
     ==
"
        .to_string();
        let mut scanner = Scanner::new(source);
        scanner.scan();
    }
}
