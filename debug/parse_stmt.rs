type Result<T> = std::result::Result<T, ()>;

#[path = "../src/lexer.rs"]
mod lexer;
#[path = "../src/parser.rs"]
mod parser;

use lexer::*;
use parser::*;

fn main() {
    let source: String = std::fs::read_to_string("examples/functions").unwrap();
    let mut lexer = Lexer::new(source.as_bytes());
    let program = parse(&mut lexer).unwrap();

    for (i, block) in program.blocks.iter().enumerate() {
        println!("{}: {:?}", i, block.range);
    }

    println!("----------------------------------");

    for stmt in &program.stmts {
        println!("{stmt:?}");
    }

    println!("----------------------------------");

    for func in program.fns {
        println!("fn {}", func.name);
        let mut r = program.blocks[func.blocks.start].range.clone();
        while r.start < r.end {
            println!("    {:?}", program.stmts[r.start]);
            if let Stmt::If { body, .. } = program.stmts[r.start] {
                r.start = program.blocks[body].range.end;
            } else {
                r.start += 1;
            }
        }
    }
}
