mod lexer;
mod parser;
mod codegen;

fn main() {
    let source = "num1 = 324 + 2 + 3 + 5 + 6; num2 = 234;";
    //let mut lexer = lexer::Lexer::new(source);
    //
    //for x in lexer.by_ref() {
    //    println!("{}", x);
    //}

    let (exprs, stmts) = parser::parse(&mut lexer::Lexer::new(source));
    //println!("{exprs:?}");
    //println!("{stmts:?}");

    codegen::gen_dp("test", &exprs, &stmts);
}
