use crate::lexer as lex;
use std::ops::Range;
use std::process::exit;

#[derive(Debug)]
pub enum Stmt<'a> {
    VarAssign { name: &'a str, expr: Range<usize> }
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    OpSub,
    OpAdd,
    OpMul,
    OpDiv,

    LParen,

    Var(&'a str),
    Num(i32),
}



pub fn parse<'a>(source: &'a [u8]) -> (Vec<Expr<'a>>, Vec<Stmt<'a>>) {
    use self::lex::*;

    let mut expr_buf: Vec<Expr> = Vec::new();
    let mut stmt_buf: Vec<Stmt> = Vec::new();
    let mut lex = Lexer::new();

    while let Some(tok) = lex.peek(source) {
        match tok.kind {
            TokenKind::Name => {
                let _ = lex.next(source);

                let _ = lex.expect_next(source, TokenKind::Eq);
                let expr = parse_expr(&mut expr_buf, &mut lex, source);
                stmt_buf.push(Stmt::VarAssign { name: tok.text, expr });
                let _ = lex.expect_next(source, TokenKind::Semicolon);
            },
            _ => {
                eprintln!("ERROR:{}: undefined statement", lex.loc);
                exit(1);
            }
        }
    }

    (expr_buf, stmt_buf) 
}

fn parse_expr<'a>(
    expr_buf: &mut Vec<Expr<'a>>,
    lex: &mut lex::Lexer<'a>,
    source: &'a [u8]
) -> Range<usize> {
    // the implementation based on: https://en.wikipedia.org/wiki/Shunting_yard_algorithm
    use self::lex::*;

    let mut tok: Token;
    let mut ret = Range { start: expr_buf.len(), end: 0 };
    let mut op_stack: Vec<Expr> = Vec::new();

    let read_tks = &[TokenKind::Name, TokenKind::Num, TokenKind::OpenParen];

    loop {
        tok = lex.expect_next_oneof(source, read_tks);
        match tok.kind {
            TokenKind::Name => { expr_buf.push(Expr::Var(tok.text)); break; },
            TokenKind::Num  => { expr_buf.push(Expr::Num(tok.text.parse::<i32>().unwrap())); break; },
            TokenKind::OpenParen => op_stack.push(Expr::LParen),
            _ => unreachable!()
        }
    }

    loop {
        tok = lex.expect_peek(source);
        match tok.kind {
            TokenKind::CloseParen => {
                while op_stack.last() != Some(&Expr::LParen) {
                    expr_buf.push(op_stack.pop().unwrap());
                    if op_stack.is_empty() {
                        eprintln!("ERROR:{}: mismatched parentheses", lex.loc);
                        exit(1);
                    }
                }
                let _ = op_stack.pop();
                let _ = lex.next(source);
                continue;
            },

            TokenKind::Plus => {
                while let Some(op) = op_stack.last() {
                    if *op == Expr::LParen { break; }
                    expr_buf.push(op_stack.pop().unwrap());
                }
                op_stack.push(Expr::OpAdd);
            },

            TokenKind::Minus => {
                while let Some(op) = op_stack.last() {
                    if *op == Expr::LParen { break; }
                    expr_buf.push(op_stack.pop().unwrap());
                }
                op_stack.push(Expr::OpSub);
            },

            TokenKind::Star => {
                while let Some(op) = op_stack.last() {
                    match op {
                        Expr::OpMul | Expr::OpDiv => {
                            expr_buf.push(op_stack.pop().unwrap());
                        },
                        _ => break
                    }
                }
                op_stack.push(Expr::OpMul);
            },

            TokenKind::Slash => {
                while let Some(op) = op_stack.last() {
                    match op {
                        Expr::OpMul | Expr::OpDiv => {
                            expr_buf.push(op_stack.pop().unwrap());
                        },
                        _ => break
                    }
                }
                op_stack.push(Expr::OpDiv);
            },

            _ => {
                op_stack.reverse();
                for op in &op_stack {
                    if *op == Expr::LParen {
                        eprintln!("ERROR:{}: mismatched parentheses", lex.loc);
                        exit(1);
                    }
                }

                expr_buf.append(&mut op_stack);
                ret.end = expr_buf.len();
                return ret;
            }
        }

        let _ = lex.next(source);
        loop {
            tok = lex.expect_next_oneof(source, read_tks);
            match tok.kind {
                TokenKind::Name => { expr_buf.push(Expr::Var(tok.text)); break; },
                TokenKind::Num  => { expr_buf.push(Expr::Num(tok.text.parse::<i32>().unwrap())); break; },
                TokenKind::OpenParen => op_stack.push(Expr::LParen),
                _ => unreachable!()
            }
        }
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_expr() {
        use super::Expr::*;

        // Syntax: v1;v2;op
        // 1 + 2          =>   12+
        // 1 + 2 + 3      =>   12+ 3 +
        // 1 + 2*3        =>   1 23* +
        // 1 * 2 * 3      =>   1 23* *
        // 1 + 2*3*4      =>   1 23* 4* +
        // 1 + 2*3*4 + 5  =>   1 23* 4* + 5+
        // 1 + 2*3 + 4*5  =>   1 23* + 45* +
        // 1 / 2 * 3      =>   12/ 3*
        // 1 / 2 * 3 * 4  =>   12/ 34**
        // 1 / 2 * 3 / 4  =>   12/ 3* 4/
        let map: &[(&str, &[Expr])] = &[
            ("1 + 2;",         &[Num(1), Num(2), OpAdd]),
            ("1 + 2 + 3;",     &[Num(1), Num(2), OpAdd, Num(3), OpAdd]),
            ("1 + 2*3;",       &[Num(1), Num(2), Num(3), OpMul, OpAdd]),
            ("1 * 2 * 3;",     &[Num(1), Num(2), OpMul, Num(3), OpMul]),
            ("1 + 2*3*4;",     &[Num(1), Num(2), Num(3), OpMul, Num(4), OpMul, OpAdd]),
            ("1 + 2*3*4 + 5;", &[Num(1), Num(2), Num(3), OpMul, Num(4), OpMul, OpAdd, Num(5), OpAdd]),
            ("1 + 2*3 + 4*5;", &[Num(1), Num(2), Num(3), OpMul, OpAdd, Num(4), Num(5), OpMul, OpAdd]),
            ("1 / 2 * 3;",     &[Num(1), Num(2), OpDiv, Num(3), OpMul]),
            ("1 / 2 * 3 * 4;", &[Num(1), Num(2), OpDiv, Num(3), OpMul, Num(4), OpMul]),
            ("1 / 2 * 3 / 4;", &[Num(1), Num(2), OpDiv, Num(3), OpMul, Num(4), OpDiv]),
            ("1 * (2 + 3);",   &[Num(1), Num(2), Num(3), OpAdd, OpMul]),
            ("1 * (2 + 3) + 2;",   &[Num(1), Num(2), Num(3), OpAdd, OpMul, Num(2), OpAdd]),
            ("3 + 4 * 2 / (1 - 5);", &[Num(3), Num(4), Num(2), OpMul, Num(1), Num(5), OpSub, OpDiv, OpAdd])
        ];

        let mut exprs: Vec<Expr> = Vec::new();
        for test in map {
            let range = parse_expr(&mut exprs, &mut lex::Lexer::new(), test.0.as_bytes());
            for x in range {
                assert_eq!(exprs[x], test.1[x]);
            }

            exprs.clear();
        }
    }

    #[test]
    fn test_parse_stmt() {
        let source = "a = 10;";
        let (_, stmts) = parse(source.as_bytes());
        assert_eq!(stmts.len(), 1);
        let Stmt::VarAssign { name, .. } = &stmts[0];
        assert_eq!(*name, "a");
    }
}
