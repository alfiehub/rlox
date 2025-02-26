use crate::{chunk::Chunk, scanner::Scanner};

pub fn compile(source: String) -> Chunk {
    let mut scanner = Scanner::new(source);
    scanner.scan();

    todo!()
}
