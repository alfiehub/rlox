use crate::{chunk::Chunk, op_code::OpCode};

impl Chunk {
    pub fn disassemble(&self, name: &str) {
        println!("== {name} ==");
        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{offset:0>4} ");
        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:>4} ", self.lines[offset]);
        }
        // offset should never be outside self.code
        let instruction = self.code.get(offset).unwrap();
        let op_code = OpCode::from(*instruction);
        match op_code {
            OpCode::OP_CONSTANT => constant_instruction(&op_code.to_string(), self, offset),
            OpCode::OP_ADD
            | OpCode::OP_SUBTRACT
            | OpCode::OP_MULTIPLY
            | OpCode::OP_DIVIDE
            | OpCode::OP_NEGATE
            | OpCode::OP_RETURN => simple_instruction(&op_code.to_string(), offset),
        }
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{name}");
    offset + 1
}

fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant_index = chunk.code.get(offset + 1).unwrap();
    print!("{name:<16} {constant_index:>4} ");
    chunk.constants.print(*constant_index);
    println!();
    offset + 2
}
