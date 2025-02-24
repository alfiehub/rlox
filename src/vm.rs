use crate::{chunk::Chunk, op_code::OpCode};

pub struct Vm<'a> {
    chunk: &'a Chunk,
    /// Instruction Pointer
    ip: *const u8,
}

pub type InterpretResult = Result<(), InterpretError>;
#[derive(Debug)]
pub enum InterpretError {
    InterpretCompileError,
    InterpretRuntimeError,
}

macro_rules! read_byte {
    ($vm:expr) => {{
        let byte = unsafe { *$vm.ip };
        $vm.ip = $vm.ip.wrapping_add(1);
        byte
    }};
}

macro_rules! read_constant {
    ($vm:expr) => {{ &$vm.chunk.constants.values[read_byte!($vm) as usize] }};
}

impl<'a> Vm<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        let ip = chunk.code.first().unwrap();
        Self { chunk, ip }
    }

    pub fn free(self) {}

    pub fn interpret(&mut self, chunk: &'a Chunk) -> InterpretResult {
        self.chunk = chunk;
        self.run()
    }

    pub fn run(&mut self) -> InterpretResult {
        loop {
            let instruction = read_byte!(self);
            let op_code = OpCode::from(instruction);
            #[cfg(debug_assertions)]
            {
                let chunk_ptr: &*const u8 = &self.chunk.code.as_ptr();
                let chunk_ptr = *chunk_ptr as usize;
                let ip_ptr = self.ip as usize;
                self.chunk.disassemble_instruction(ip_ptr - chunk_ptr - 1);
            }
            match op_code {
                OpCode::OP_CONSTANT => {
                    let value = read_constant!(self);
                    value.print();
                    println!();
                }
                OpCode::OP_RETURN => {
                    return Ok(());
                }
            }
        }
    }
}
