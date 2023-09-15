use anyhow::{anyhow, Result};
use clap::Parser;
use interpreter::Interpreter;

mod ast;
mod interpreter;
mod lox_type;
mod parser;
mod scanner;
mod token;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(short, long)]
    file: Option<String>,
}

fn run(code: String, interpreter: &mut Interpreter) -> Result<()> {
    let tokens = scanner::Scanner::scan(&code)?;
    let mut parser = parser::Parser::new(tokens);
    let ast = parser.parse()?;
    interpreter
        .interpret(&ast)
        .map_err(|e| anyhow!(format!("{e:?}")))?;
    Ok(())
}

fn run_file(file: String) -> Result<()> {
    let file_content = std::fs::read_to_string(file)?;
    let mut interpreter = interpreter::Interpreter::new();
    run(file_content, &mut interpreter)?;
    Ok(())
}

fn run_prompt() -> Result<()> {
    let mut interpreter = interpreter::Interpreter::new();
    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input)?;
        run(input, &mut interpreter)?;
    }
}

fn main() -> Result<()> {
    let args = Cli::parse();
    if let Some(file) = args.file {
        run_file(file)?;
    } else {
        run_prompt()?;
    };
    Ok(())
}
