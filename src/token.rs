use std::fmt::Display;

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum TokenType {
    // Single character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier(String),
    String(String),
    Number(f64),

    // Kwtqoesa
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

impl TokenType {
    pub fn from_identifier(identifier: &str) -> Option<Self> {
        match identifier {
            "and" => Some(TokenType::And),
            "class" => Some(TokenType::Class),
            "else" => Some(TokenType::Else),
            "false" => Some(TokenType::False),
            "for" => Some(TokenType::For),
            "fun" => Some(TokenType::Fun),
            "if" => Some(TokenType::If),
            "nil" => Some(TokenType::Nil),
            "or" => Some(TokenType::Or),
            "print" => Some(TokenType::Print),
            "return" => Some(TokenType::Return),
            "super" => Some(TokenType::Super),
            "this" => Some(TokenType::This),
            "true" => Some(TokenType::True),
            "var" => Some(TokenType::Var),
            "while" => Some(TokenType::While),
            _ => None,
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::LeftBrace => write!(f, "{{"),
            Self::RightBrace => write!(f, "}}"),
            Self::Comma => write!(f, ","),
            Self::Dot => write!(f, "."),
            Self::Minus => write!(f, "-"),
            Self::Plus => write!(f, "+"),
            Self::Semicolon => write!(f, ";"),
            Self::Slash => write!(f, "/"),
            Self::Star => write!(f, "*"),
            Self::Bang => write!(f, "!"),
            Self::BangEqual => write!(f, "!="),
            Self::Equal => write!(f, "="),
            Self::EqualEqual => write!(f, "=="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            Self::Less => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),
            Self::Identifier(identifier) => write!(f, "{}", identifier),
            Self::String(string) => write!(f, "{}", string),
            Self::Number(number) => write!(f, "{}", number),
            Self::And => write!(f, "and"),
            Self::Class => write!(f, "class"),
            Self::Else => write!(f, "else"),
            Self::False => write!(f, "false"),
            Self::For => write!(f, "for"),
            Self::Fun => write!(f, "fun"),
            Self::If => write!(f, "if"),
            Self::Nil => write!(f, "nil"),
            Self::Or => write!(f, "or"),
            Self::Print => write!(f, "print"),
            Self::Return => write!(f, "return"),
            Self::Super => write!(f, "super"),
            Self::This => write!(f, "this"),
            Self::True => write!(f, "true"),
            Self::Var => write!(f, "var"),
            Self::While => write!(f, "while"),
            Self::Eof => write!(f, "EOF"),
        }
    }
}

#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: Option<String>,
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize) -> Self {
        Self {
            token_type,
            lexeme: Some(lexeme),
            line,
        }
    }

    pub fn new_empty(token_type: TokenType, line: usize) -> Self {
        Self {
            token_type,
            lexeme: None,
            line,
        }
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} @ L{}", self.token_type, self.line)
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_type)
    }
}
