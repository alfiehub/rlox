use std::{ops::Neg, pin::Pin};

use crate::{chunk::Chunk, op_code::OpCode, value::Value};

const STACK_MAX: usize = 256;

#[derive(Debug)]
struct Stack {
    /// `stack` must be pinned to prevent it from moving. It can't be moved because of the
    /// `stack_top` pointer.
    stack: Pin<Box<[Value; STACK_MAX]>>,
    stack_top: *mut Value,
}

impl Stack {
    fn new() -> Self {
        let mut stack = Box::pin([Value::default(); STACK_MAX]);
        let stack_top: *mut Value = stack.as_mut_ptr();
        Self { stack, stack_top }
    }

    fn push(&mut self, value: Value) {
        unsafe {
            *self.stack_top = value;
            self.stack_top = self.stack_top.add(1);
        }
    }

    fn pop(&mut self) -> Value {
        unsafe {
            self.stack_top = self.stack_top.sub(1);
            std::mem::take(self.stack_top.as_mut().unwrap())
        }
    }

    fn print(&self) {
        let mut current = self.stack.as_ptr();
        print!("          ");
        while current < self.stack_top {
            unsafe {
                print!("[");
                current.as_ref().unwrap().print();
                print!("]");
                current = current.add(1);
            }
        }
        println!();
    }
}

pub struct Vm<'a> {
    chunk: &'a Chunk,
    /// Instruction Pointer
    ip: *const u8,
    stack: Stack,
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

macro_rules! binary_op {
    ($stack:expr, $op:tt) => {{
        let b = $stack.pop().0;
        let a = $stack.pop().0;
        $stack.push(Value( a $op b ));
    }};
}

impl<'a> Vm<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        let ip = chunk.code.first().unwrap();
        Self {
            chunk,
            ip,
            stack: Stack::new(),
        }
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
                self.stack.print();

                // Some acrobatics to get the correct offset based on pointers
                let chunk_ptr: &*const u8 = &self.chunk.code.as_ptr();
                let chunk_ptr = *chunk_ptr as usize;
                let ip_ptr = self.ip as usize;
                self.chunk.disassemble_instruction(ip_ptr - chunk_ptr - 1);
            }
            match op_code {
                OpCode::OP_CONSTANT => {
                    let value = read_constant!(self);
                    self.stack.push(*value);
                }
                OpCode::OP_ADD => binary_op!(self.stack, +),
                OpCode::OP_SUBTRACT => binary_op!(self.stack, -),
                OpCode::OP_MULTIPLY => binary_op!(self.stack, *),
                OpCode::OP_DIVIDE => binary_op!(self.stack, /),
                OpCode::OP_NEGATE => {
                    let value = self.stack.pop();
                    self.stack.push(Value(value.0.neg()));
                }
                OpCode::OP_RETURN => {
                    let value = self.stack.pop();
                    value.print();
                    println!();
                    return Ok(());
                }
            }
        }
    }
}
