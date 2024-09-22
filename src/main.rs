mod lexer;
mod parser;
mod compiler;

use std::io::prelude::*;
use std::process::ExitCode;

type Result<T> = std::result::Result<T, ()>;

fn main2() -> Result<()> {
    let file_path = std::env::args().nth(1).ok_or_else(|| {
        eprintln!("ERROR: source file must be provided");
    })?;

    let mut src_file = std::fs::File::open(&file_path).map_err(|err| {
        eprintln!("ERROR: could not open file `{file_path}`: {err}");
    })?;

    let mut buffer = String::new();
    let _ = src_file.read_to_string(&mut buffer).map_err(|err| {
        eprintln!("ERROR: could not read file `{file_path}`: {err}");
    })?;

    let mut lexer = lexer::Lexer::new(buffer.as_bytes());
    let syntax = parser::parse(&mut lexer)?;
    let _ = compiler::compile(&syntax)?;

    Ok(())
}

fn main() -> ExitCode {
    match main2() {
        Err(_) => ExitCode::FAILURE,
        Ok(_)  => ExitCode::SUCCESS,
    }
}
