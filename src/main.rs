use rlox::{chunk::Chunk, op_code::OpCode, value::Value, vm::Vm};

fn main() {
    let mut chunk = Chunk::default();
    let constant_index = chunk.add_constant(Value(1.2));
    chunk.write(OpCode::OP_CONSTANT.to_byte(), 123);
    chunk.write(constant_index, 123);

    let constant_index = chunk.add_constant(Value(3.4));
    chunk.write(OpCode::OP_CONSTANT.to_byte(), 123);
    chunk.write(constant_index, 123);

    chunk.write(OpCode::OP_ADD.to_byte(), 123);

    let constant_index = chunk.add_constant(Value(5.6));
    chunk.write(OpCode::OP_CONSTANT.to_byte(), 123);
    chunk.write(constant_index, 123);

    chunk.write(OpCode::OP_DIVIDE.to_byte(), 123);

    chunk.write(OpCode::OP_NEGATE.to_byte(), 123);
    chunk.write(OpCode::OP_RETURN.to_byte(), 123);
    let mut vm = Vm::new(&chunk);
    vm.run().unwrap();
    vm.free();
    chunk.free();
}
