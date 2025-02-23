use rlox::{chunk::Chunk, op_code::OpCode, value::Value};

fn main() {
    let mut chunk = Chunk::default();
    let constant_index = chunk.add_constant(Value(1.2));
    chunk.write(OpCode::OP_CONSTANT.to_byte(), 123);
    chunk.write(constant_index, 123);
    chunk.write(OpCode::OP_RETURN.to_byte(), 123);
    chunk.disassemble("test chunk");
    chunk.free();
}
