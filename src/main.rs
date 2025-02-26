use std::io::{self, BufRead, Write};

use rlox::{chunk::Chunk, vm::Vm};

fn print_prompt() -> io::Result<()> {
    print!("> ");
    // Need to manually flush as it's not done unless a \n is printed
    std::io::stdout().flush()
}

fn repl(vm: &mut Vm<'_>) -> io::Result<()> {
    let stdin = std::io::stdin();
    let mut lines = stdin.lock().lines();
    print_prompt()?;
    while let Some(Ok(line)) = lines.next() {
        if line.is_empty() {
            break;
        }
        let _ = vm.interpret(line);
        print_prompt()?;
    }
    println!();
    Ok(())
}

fn run_file(vm: &mut Vm<'_>, path: &str) -> std::io::Result<()> {
    let source = std::fs::read_to_string(path)?;
    if let Err(err) = vm.interpret(source) {
        match err {
            rlox::vm::InterpretError::InterpretCompileError => std::process::exit(65),
            rlox::vm::InterpretError::InterpretRuntimeError => std::process::exit(70),
        }
    }
    Ok(())
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    println!("{args:?}");
    let mut chunk = Chunk::default();
    let mut vm = Vm::new(&chunk);
    if args.len() == 1 {
        repl(&mut vm)?;
    } else if args.len() == 2 {
        let path = args.last().unwrap();
        run_file(&mut vm, path)?;
        println!("TODO {path}");
    } else {
        eprintln!("Usage: rlox [path]");
        std::process::exit(74)
    }
    vm.free();
    chunk.free();
    Ok(())
}
