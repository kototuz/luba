mod lexer;
mod parser;
mod codegen;

use std::process::exit;
use std::io::prelude::*;

fn main() {
    let file_path = std::env::args().nth(1).unwrap_or_else(|| {
        eprintln!("ERROR: source file must be provided");
        exit(1);
    });

    let mut src_file = std::fs::File::open(&file_path).unwrap_or_else(|err| {
        eprintln!("ERROR: could not open file `{file_path}`: {err}");
        exit(1);
    });

    let mut buffer = String::new();
    let _ = src_file.read_to_string(&mut buffer).inspect_err(|err| {
        eprintln!("ERROR: could not read file `{file_path}`: {err}");
        exit(1);
    });

    let syntax = parser::parse(buffer.as_bytes());

    let output_dir = std::env::current_dir().unwrap_or_else(|err| {
        eprintln!("ERROR: could not get current dir: {err}");
        exit(1);
    });

    codegen::gen_code(output_dir.as_path(), &syntax);
}
