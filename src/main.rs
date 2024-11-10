mod lexer;
mod parser;
mod compiler;
mod semantic;

use std::io::prelude::*;

use parser::Ast;

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

macro_rules! error {
    ($($arg:tt)*) => {
        eprint!("ERROR: ");
        eprintln!($($arg)*);
        exit_failure!();
    };
}

fn main() {
    let file_path = std::env::args().nth(1).unwrap_or_else(|| {
        error!("Source file must be provided");
    });

    let mut src_file = std::fs::File::open(&file_path).unwrap_or_else(|err| {
        error!("Could not open file `{file_path}`: {err}");
    });

    let mut buffer = String::new();
    let _ = src_file.read_to_string(&mut buffer).unwrap_or_else(|err| {
        error!("Could not read file `{file_path}`: {err}");
    });

    let output = std::fs::File::create("out.mcfunction").unwrap_or_else(|err| {
        error!("Could not create an output file: {err}");
    });

    let mut lexer = lexer::Lexer::new(buffer.as_bytes()); // lexical analysis (lazy)

    let ast: Ast = parser::parse(&mut lexer);             // syntax  analysis
    //println!("{ast:#?}");

    //println!("===========================================");

    let sem_data = semantic::Analyzer::analyze(&ast);     // semantic analyzis
    //println!("{sem_data:#?}");

    compiler::compile(output, &ast, sem_data);
}
