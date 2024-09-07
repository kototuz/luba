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

    use std::path::PathBuf;
    let file_name = PathBuf::from(PathBuf::from(file_path).file_name().unwrap());
    let file_name = file_name.file_stem().unwrap();

    let (mut exprs, mut stmts) = parser::parse(&mut lexer::Lexer::new(&buffer));

    codegen::gen_dp(file_name.to_str().unwrap(), &mut exprs, &mut stmts);
}
