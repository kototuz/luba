mod lexer;
mod parser;
mod compiler;
mod semantic;

use std::io::prelude::*;
use std::process::ExitCode;

type Result<T> = std::result::Result<T, ()>;

#[cfg(debug_assertions)]
#[macro_export]
macro_rules! exit_failure { () => { panic!(); } }

#[cfg(not(debug_assertions))]
#[macro_export]
macro_rules! exit_failure { () => { std::process::exit(1); } }

#[macro_export]
macro_rules! lexical_err {
    ($loc:expr, $($arg:tt)*) => {
        eprint!("ERROR:{}: LexicalError: ", $loc);
        eprintln!($($arg)*);
        exit_failure!();
    }
}

#[macro_export]
macro_rules! syntax_err {
    ($loc:expr, $($arg:tt)*) => {
        eprint!("ERROR:{}: SyntaxError: ", $loc);
        eprintln!($($arg)*);
        exit_failure!();
    }
}

#[macro_export]
macro_rules! unexpected_token_err {
    ($loc:expr, $t:ident) => {
        syntax_err!($loc, "Unexpected {}", $t);
    }
}

#[macro_export]
macro_rules! semantic_err {
    ($loc:expr, $($arg:tt)*) => {
        eprint!("ERROR:{}: SemanticError: ", $loc);
        eprintln!($($arg)*);
        exit_failure!();
    }
}

#[macro_export]
macro_rules! compilation_err {
    ($($arg:tt)*) => {
        eprint!("ERROR: CompilationError: ");
        eprintln!($($arg)*);
        exit_failure!();
    }
}

fn main() {
    let file_path = std::env::args().nth(1).unwrap_or_else(|| {
        eprintln!("ERROR: source file must be provided");
        exit_failure!();
    });

    let mut src_file = std::fs::File::open(&file_path).unwrap_or_else(|err| {
        eprintln!("ERROR: could not open file `{file_path}`: {err}");
        exit_failure!();
    });

    let mut buffer = String::new();
    let _ = src_file.read_to_string(&mut buffer).unwrap_or_else(|err| {
        eprintln!("ERROR: could not read file `{file_path}`: {err}");
        exit_failure!();
    });

    let mut lexer = lexer::Lexer::new(buffer.as_bytes()); // lexical analysis (lazy)
    let ast = parser::parse(&mut lexer);                  // syntax  analysis
    compiler::compile(ast);                               // compilation
}
