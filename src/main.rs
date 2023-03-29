use clap::Parser;
use anyhow::Result;

mod ast;
mod scanner;
mod error;
mod token;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(short, long)]
    file: Option<String>,
}

fn run(code: String) -> Result<()> {
    let scanner = scanner::Scanner::new();
    let tokens = scanner.scan(&code)?;
    println!("{:?}", tokens);
    Ok(())
}

fn run_file(file: String) -> Result<()> {
    let file_content = std::fs::read_to_string(file)?;
    run(file_content)?;
    Ok(())
}

fn run_prompt() -> Result<()> {
    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input)?;
        run(input)?;
    };
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
