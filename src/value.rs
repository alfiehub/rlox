use crate::memory::{grow_capacity, grow_vec};

#[derive(Debug)]
pub struct Value(pub f64);

/// A list of values
#[derive(Default)]
pub struct Values {
    pub(super) count: usize,
    capacity: usize,
    pub(super) values: Vec<Value>,
}

impl Values {
    pub fn write(&mut self, value: Value) {
        if self.capacity < self.count + 1 {
            // Grow the before pushing
            self.capacity = grow_capacity(self.capacity);
            grow_vec(&mut self.values, self.capacity);
        }
        self.values.push(value);
        self.count += 1;
    }

    pub fn free(&mut self) {
        let _ = std::mem::take(self);
    }

    pub fn print(&self, index: u8) {
        print!("'{}'", self.values.get(index as usize).unwrap().0);
    }
}
