#[cfg(test)]
mod tests;

use crate::{
    memory::{grow_capacity, grow_vec},
    value::{Value, Values},
};

/// A `Chunk` of bytecode
#[derive(Default)]
pub struct Chunk {
    count: usize,
    capacity: usize,
    pub(super) code: Vec<u8>,
    pub(super) constants: Values,
    pub(super) lines: Vec<usize>,
}

impl Chunk {
    pub fn write(&mut self, byte: u8, line: usize) {
        if self.capacity < self.count + 1 {
            // Grow the before pushing
            self.capacity = grow_capacity(self.capacity);
            grow_vec(&mut self.code, self.capacity);
            grow_vec(&mut self.lines, self.capacity);
        }
        self.code.push(byte);
        self.lines.push(line);
        self.count += 1;
    }

    pub fn free(&mut self) {
        let _ = std::mem::take(self);
    }

    /// Adds a constant and returns the index where it was appended
    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.write(value);
        (self.constants.count - 1) as u8
    }
}
