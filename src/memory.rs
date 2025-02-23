pub fn grow_capacity(old_capacity: usize) -> usize {
    if old_capacity < 8 {
        8
    } else {
        old_capacity.saturating_mul(2)
    }
}

pub fn grow_vec<T>(vec: &mut Vec<T>, capacity: usize) {
    let additional = capacity - vec.capacity();
    vec.reserve(additional);
}
