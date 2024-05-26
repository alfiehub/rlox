mod environment;
#[allow(clippy::module_inception)]
mod interpreter;

pub use environment::Environment;
pub use interpreter::Interpreter;
