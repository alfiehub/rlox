mod precedence;

use precedence::Precedence;

use crate::{
    chunk::Chunk,
    op_code::OpCode,
    scanner::Scanner,
    token::{Token, TokenKind},
    value::Value,
    vm::InterpretResult,
};
pub struct Compiler {
    chunk: Chunk,
    scanner: Scanner,
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,
}

impl Compiler {
    pub fn new(source: String) -> Self {
        let scanner = Scanner::new(source);
        Self {
            chunk: Chunk::default(),
            scanner,
            current: Token::default(),
            previous: Token::default(),
            had_error: false,
            panic_mode: false,
        }
    }

    fn advance(&mut self) {
        std::mem::swap(&mut self.previous, &mut self.current);
        loop {
            self.current = self.scanner.scan_token();
            if self.current.kind != TokenKind::TOKEN_ERROR {
                break;
            }
        }
    }

    fn consume(&mut self, expected_token_kind: TokenKind, msg: &str) {
        if self.current.kind == expected_token_kind {
            self.advance();
            return;
        }
        self.error_at_current(msg);
    }
    pub fn compile(mut self) -> InterpretResult<Chunk> {
        self.advance();
        self.expression();
        if self.had_error {
            return Err(crate::vm::InterpretError::InterpretCompileError);
        }
        self.end_compiler();
        Ok(self.chunk)
    }

    fn end_compiler(&mut self) {
        self.emit_return();
        #[cfg(feature = "debug_print_code")]
        if !self.had_error {
            self.chunk.disassemble("code");
        }
    }

    fn binary(&mut self) {
        let operator_kind = self.previous.kind;

        // Compile the right operand
        let rule = self.get_rule(operator_kind);
        let precedence: Precedence = unsafe { std::mem::transmute(rule.precedence as u8 + 1) };
        self.parse_precedence(precedence);

        // Emit the operator instruction
        match operator_kind {
            TokenKind::TOKEN_PLUS => self.emit_byte(OpCode::OP_ADD.to_byte()),
            TokenKind::TOKEN_MINUS => self.emit_byte(OpCode::OP_SUBTRACT.to_byte()),
            TokenKind::TOKEN_STAR => self.emit_byte(OpCode::OP_MULTIPLY.to_byte()),
            TokenKind::TOKEN_SLASH => self.emit_byte(OpCode::OP_DIVIDE.to_byte()),
            _ => {
                // Unreachable if the grammar is correct
                self.error_at_previous("Invalid binary operator");
            }
        }
    }

    fn unary(&mut self) {
        let operator_kind = self.previous.kind;
        self.parse_precedence(Precedence::PREC_UNARY);
        if operator_kind == TokenKind::TOKEN_MINUS {
            self.emit_byte(OpCode::OP_NEGATE.to_byte());
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenKind::TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn number(&mut self) {
        let value: f64 = self.previous.to_string().parse().unwrap();
        self.emit_constant(Value(value));
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::PREC_ASSIGNMENT);
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let token_kind = self.previous.kind;
        if let Some(prefix_rule) = self.get_rule(token_kind).prefix {
            prefix_rule(self);
        } else {
            self.error_at_previous("Expect expression.");
            return;
        }

        // Process infix operations based on precedence
        while precedence as u8 <= self.get_rule(self.current.kind).precedence as u8 {
            self.advance();
            if let Some(infix_rule) = self.get_rule(self.previous.kind).infix {
                infix_rule(self);
            }
        }
    }

    fn get_rule(&self, token_kind: TokenKind) -> &precedence::ParseRule {
        precedence::PARSE_RULES_MAP
            .get(&token_kind)
            .unwrap_or_else(|| {
                // This should never happen if all token kinds are covered in the map
                panic!("No parsing rule for token kind: {:?}", token_kind);
            })
    }

    fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_bytes(OpCode::OP_CONSTANT.to_byte(), constant);
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::OP_RETURN.to_byte());
    }

    fn emit_byte(&mut self, byte: u8) {
        self.chunk.write(byte, self.previous.line as usize);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        let constant = self.chunk.add_constant(value);
        if constant > u8::MAX as usize {
            // error("Too many constants in one chunk.");
            eprintln!("Too many constants in one chunk.")
        }
        constant as u8
    }

    fn error_at_current(&mut self, msg: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        error_at(&self.current, msg);
        self.had_error = true;
    }

    fn error_at_previous(&mut self, msg: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        error_at(&self.previous, msg);
        self.had_error = true;
    }
}

fn error_at(token: &Token, message: &str) {
    eprint!("[line {}] Error", token.line);

    if token.kind == TokenKind::TOKEN_EOF {
        eprint!(" at end");
    } else if token.kind == TokenKind::TOKEN_ERROR {
        // Nothing.
    } else {
        eprint!(
            " at '{}'",
            std::str::from_utf8(unsafe { std::slice::from_raw_parts(token.start, token.length) })
                .unwrap()
        );
    }

    eprintln!(": {}", message);
}
