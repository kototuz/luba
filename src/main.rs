use std::str;

mod lexer;

fn main() {
    let str = "num1 = 10;\nnum2 = 20;\nnum3 = 3*4;".as_bytes();
    let mut lex = lexer::Lexer::new(str);
    while let Some(res) = lex.next_token() {
        if let Ok(tok) = res {
            println!("({}, {:?})", str::from_utf8(tok.data).unwrap(), tok.kind);
        }
    }
}
