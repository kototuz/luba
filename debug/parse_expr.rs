mod parser;
mod lexer;

use parser::*;
use lexer::*;
fn main() {
    let source = std::env::args().skip(1).next().unwrap();
    println!("{}\n=>", source);

    let mut expr_buf: Vec<Expr> = Vec::new();
    let expr_range = parse_expr(
        &mut expr_buf,
        &mut Lexer::new(),
        source.as_bytes()
    );

    for expr_i in expr_range {
        match expr_buf[expr_i] {
            Expr::Num(z) => print!("({})", z),
            Expr::OpAdd  => print!("+"),
            Expr::OpSub  => print!("-"),
            Expr::OpMul  => print!("*"),
            Expr::OpDiv  => print!("/"),
            _ => unreachable!()
        }
    }
    print!("\n");
}
