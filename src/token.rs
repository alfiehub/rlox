use crate::scanner::Scanner;

#[allow(non_camel_case_types)]
#[repr(u8)]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Single-character tokens.
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_SLASH,
    TOKEN_STAR,
    // One or two character tokens.
    TOKEN_BANG,
    TOKEN_BANG_EQUAL,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,
    // Literals.
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,
    // Keywords.
    TOKEN_AND,
    TOKEN_CLASS,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FUN,
    TOKEN_IF,
    TOKEN_NIL,
    TOKEN_OR,
    TOKEN_PRINT,
    TOKEN_RETURN,
    TOKEN_SUPER,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_WHILE,

    TOKEN_ERROR,
    #[default]
    TOKEN_EOF,
}
impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", *self as u8)
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub line: isize,
    pub length: usize,
    pub start: *const u8,
}

impl Default for Token {
    fn default() -> Self {
        Self {
            kind: Default::default(),
            line: Default::default(),
            length: Default::default(),
            start: std::ptr::null(),
        }
    }
}

impl Token {
    pub fn new(kind: TokenKind, scanner: &Scanner) -> Self {
        Self {
            kind,
            line: scanner.line,
            length: scanner.current as usize - scanner.start as usize,
            start: scanner.start,
        }
    }

    pub fn error(message: &str, scanner: &Scanner) -> Self {
        let message = message.to_owned();
        // This is messy, but we can't drop it as we rely on a pointer to it
        let message: &'static _ = message.leak();
        Self {
            kind: TokenKind::TOKEN_ERROR,
            line: scanner.line,
            length: message.len(),
            start: message.as_ptr(),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = String::from_utf8(
            unsafe { std::slice::from_raw_parts(self.start, self.length) }.to_vec(),
        )
        .unwrap();
        write!(f, "{}", string)
    }
}
