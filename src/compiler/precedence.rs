use std::collections::HashMap;

use crate::token::TokenKind;

use super::Compiler;

#[allow(non_camel_case_types, dead_code)]
#[derive(Clone, Copy)]
#[repr(u8)]
pub(super) enum Precedence {
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR,         // or
    PREC_AND,        // and
    PREC_EQUALITY,   // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM,       // + -
    PREC_FACTOR,     // * /
    PREC_UNARY,      // ! -
    PREC_CALL,       // . ()
    PREC_PRIMARY,
}

pub(super) struct ParseRule {
    pub(super) prefix: Option<fn(&mut Compiler)>,
    pub(super) infix: Option<fn(&mut Compiler)>,
    pub(super) precedence: Precedence,
}

const PARSE_RULES: [(TokenKind, ParseRule); 40] = [
    (
        TokenKind::TOKEN_LEFT_PAREN,
        ParseRule {
            prefix: Some(Compiler::grouping),
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_RIGHT_PAREN,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_LEFT_BRACE,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_RIGHT_BRACE,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_COMMA,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_DOT,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_MINUS,
        ParseRule {
            prefix: Some(Compiler::unary),
            infix: Some(Compiler::binary),
            precedence: Precedence::PREC_TERM,
        },
    ),
    (
        TokenKind::TOKEN_PLUS,
        ParseRule {
            prefix: None,
            infix: Some(Compiler::binary),
            precedence: Precedence::PREC_TERM,
        },
    ),
    (
        TokenKind::TOKEN_SEMICOLON,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_SLASH,
        ParseRule {
            prefix: None,
            infix: Some(Compiler::binary),
            precedence: Precedence::PREC_FACTOR,
        },
    ),
    (
        TokenKind::TOKEN_STAR,
        ParseRule {
            prefix: None,
            infix: Some(Compiler::binary),
            precedence: Precedence::PREC_FACTOR,
        },
    ),
    (
        TokenKind::TOKEN_BANG,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_BANG_EQUAL,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_EQUAL,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_EQUAL_EQUAL,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_GREATER,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_GREATER_EQUAL,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_LESS,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_LESS_EQUAL,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_IDENTIFIER,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_STRING,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_NUMBER,
        ParseRule {
            prefix: Some(Compiler::number),
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_AND,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_CLASS,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_ELSE,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_FALSE,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_FOR,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_FUN,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_IF,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_NIL,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_OR,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_PRINT,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_RETURN,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_SUPER,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_THIS,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_TRUE,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_VAR,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_WHILE,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_ERROR,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
    (
        TokenKind::TOKEN_EOF,
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::PREC_NONE,
        },
    ),
];

lazy_static::lazy_static! {
    pub(super) static ref PARSE_RULES_MAP: HashMap<TokenKind, ParseRule> = std::collections::HashMap::from_iter(PARSE_RULES);
}
