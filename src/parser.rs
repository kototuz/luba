use crate::lexer as lex;
use std::process::exit;
use std::ops::Range;

#[derive(Debug)]
pub enum Stmt<'a> {
    VarAssign { name: &'a str, expr: Range<usize> }
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Var(&'a str),
    Num(i32),

    OpSub,
    OpAdd,
    OpMul,
    OpDiv
}



pub fn parse<'a>(lex: &mut lex::Lexer<'a>) -> (Vec<Expr<'a>>, Vec<Stmt<'a>>) {
    use self::lex::*;

    let mut expr_buf: Vec<Expr> = Vec::new();
    let mut stmt_buf: Vec<Stmt> = Vec::new();

    loop {
        if let Some(tok) = lex.next() {
            match tok.kind {
                TokenKind::Name => {
                    let _ = lex.expect_specific_next(TokenKind::Eq);
                    let expr = parse_expr(&mut expr_buf, lex);
                    stmt_buf.push(Stmt::VarAssign { name: tok.data, expr });
                    let _ = lex.expect_specific_next(TokenKind::Semicolon);
                },
                _ => todo!("now only variable assign stmt is avilable")
            }
        } else { return (expr_buf, stmt_buf); }
    }
}

// TODO: support for `()`
fn parse_expr<'a>(expr_buf: &mut Vec<Expr<'a>>, lex: &mut lex::Lexer<'a>) -> Range<usize> {
    use self::lex::*;

    // TODO: make seperate function for lexer. Something like `expect_oneof_next()`
    fn expect_read<'a>(lex: &mut Lexer<'a>) -> Expr<'a> {
        let tok = lex.expect_next();
        match tok.kind {
            TokenKind::Name => Expr::Var(tok.data),
            TokenKind::Num  => Expr::Num(
                tok.data.parse::<i32>().unwrap_or_else(|err| {
                    eprintln!("ERROR: could not parse `{}`: {err}", tok.data);
                    exit(1);
                })
            ),
            _ => unreachable!()
        }
    }

    //fn skip_open_parens(lex: &mut Lexer) -> bool {
    //    let mut ret = false;
    //    if lex.expect_peek().kind == TokenKind::OpenParen {
    //        ret = true;
    //    }
    //
    //    let _ = lex.next().unwrap();
    //    while lex.expect_peek().kind == TokenKind::OpenParen {
    //        let _ = lex.next().unwrap();
    //    }
    //    let _ = lex.next().unwrap();
    //
    //    ret
    //}

    // 1 + 2 * 3 => 1 23* +
    // 1 * 2 + 3 => 12* 3 +
    // 1 * 2 * 3 => 1 23* *
    // 1 + 2 * 3 * 4 => 1 23* 4* 5* 6*
    // 1 + 2 * 3 + 2 => 1 23*+ 2
    // 1 + 2 / 3 * 4 => 123/ 4*+
    // 1 / 2 * 3 * 4 * 5 => 12/34*5*
    // 1 / 2 / 3 * 4 / 5 => 1 23/ /4*5/
    let mut ret = Range { start: expr_buf.len(), end: 0 };
    expr_buf.push(expect_read(lex));
    loop {
        match lex.expect_peek().kind {
            TokenKind::Plus  => {
                let _ = lex.next();
                expr_buf.push(expect_read(lex));
                expr_buf.push(Expr::OpAdd);
            },
            TokenKind::Minus => {
                let _ = lex.next();
                expr_buf.push(expect_read(lex));
                expr_buf.push(Expr::OpAdd);
            },
            TokenKind::Star  => {
                let _ = lex.next();
                match expr_buf.last() {
                    Some(Expr::OpAdd | Expr::OpSub) => {
                        expr_buf.insert(expr_buf.len()-1, expect_read(lex));
                        expr_buf.insert(expr_buf.len()-1, Expr::OpMul);
                    },
                    _ => {
                        expr_buf.push(expect_read(lex));
                        expr_buf.push(Expr::OpMul);
                    }
                }
            },
            TokenKind::Slash => {
                let _ = lex.next();
                match expr_buf.last() {
                    Some(Expr::OpAdd | Expr::OpSub) => {
                        expr_buf.insert(expr_buf.len()-1, expect_read(lex));
                        expr_buf.insert(expr_buf.len()-1, Expr::OpDiv);
                    },
                    _ => {
                        expr_buf.push(expect_read(lex));
                        expr_buf.push(Expr::OpDiv);
                    }
                }
            },
            _  => {
                ret.end = expr_buf.len();
                return ret;
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
            ("1 / 2 * 3 / 4;", &[Num(1), Num(2), OpDiv, Num(3), OpMul, Num(4), OpDiv])
        ];

        let mut exprs: Vec<Expr> = Vec::new();
        for test in map {
            let range = parse_expr(&mut exprs, &mut lex::Lexer::new(test.0));
            dbg!("{}", &exprs);
            for x in range {
                assert_eq!(exprs[x], test.1[x]);
            }

            exprs.clear();
        }
    }

    #[test]
    fn test_parse_stmt() {
        let source = "a = 10;";
        let (_, stmts) = parse(&mut lex::Lexer::new(source));
        assert_eq!(stmts.len(), 1);
        let Stmt::VarAssign { name, .. } = &stmts[0];
        assert_eq!(*name, "a");
    }
}
