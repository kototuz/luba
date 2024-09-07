use crate::lexer as lex;
use std::process;
use std::ops::Range;

#[derive(Debug)]
pub enum Stmt<'a> {
    VarAssign { name: &'a str, expr: Range<usize> }
}

#[derive(Debug,  Clone, PartialEq)]
pub enum OpKind {
    Sub,
    Add,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Var(&'a str),
    Num(i32),
    Op(OpKind)
}



pub fn parse<'a>(lex: &mut lex::Lexer<'a>) -> (Vec<Expr<'a>>, Vec<Stmt<'a>>) {
    use lex::*;

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

fn parse_expr<'a>(expr_buf: &mut Vec<Expr<'a>>, lex: &mut lex::Lexer<'a>) -> Range<usize> {
    use lex::*;

    fn parse_num(src: &str) -> i32 {
        src.parse::<i32>().unwrap_or_else(|err| {
            eprintln!("ERROR: parser: could not parse num `{src}`: {err}");
            process::exit(1);
        })
    }

    // TODO: make seperate function for lexer. Something like `expect_oneof_next()`
    fn expect_read<'a>(lex: &mut Lexer<'a>) -> Expr<'a> {
        let tok = lex.expect_next();
        match tok.kind {
            TokenKind::Name => Expr::Var(tok.data),
            TokenKind::Num  => Expr::Num(parse_num(tok.data)),
            _ => unreachable!()
        }
    }

    // 1 + 1     => 11+
    // 1 + 1 + 1 => 11+ 1+ 1+ 1+ 1+ 1+
    // 1 - 1 - 1 => 11- 1-
    // 1 + 1 * 1 => 1 11* +
    // 1 + 1 / 1 => 1 11/ +

    let mut op: OpKind;
    let mut ret = Range { start: expr_buf.len(), end: 0 };

    expr_buf.push(expect_read(lex));

    // TODO: *, /
    op = match lex.expect_peek().kind {
        TokenKind::Plus  => OpKind::Add,
        TokenKind::Minus => OpKind::Sub,
        TokenKind::Star  => OpKind::Mul,
        TokenKind::Slash => OpKind::Div,
        _ => {
            ret.end = expr_buf.len();
            return ret;
        }
    };
    let _ = lex.next();

    loop {
        expr_buf.push(expect_read(lex));
        expr_buf.push(Expr::Op(op));

        op = match lex.expect_peek().kind {
            TokenKind::Plus  => OpKind::Add,
            TokenKind::Minus => OpKind::Sub,
            TokenKind::Star  => OpKind::Mul,
            TokenKind::Slash => OpKind::Div,
            _  => {
                ret.end = expr_buf.len();
                return ret;
            }
        };
        let _ = lex.next();
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_test() {
        let source = "a = 1 + 2 + 3 + b;";
        let (exprs, stmts) = parse(&mut lex::Lexer::new(source));

        // 12+ 3+ b+
        let expected = [
            Expr::Num(1),
            Expr::Num(2),
            Expr::Op(OpKind::Add),
            Expr::Num(3),
            Expr::Op(OpKind::Add),
            Expr::Var("b"),
            Expr::Op(OpKind::Add)
        ];

        let Stmt::VarAssign { name, expr } = &stmts[0];
        assert_eq!(*name, "a");
        assert_eq!(*expr, (0..expected.len()));

        for (i, expr) in exprs.iter().enumerate() {
            assert_eq!(*expr, expected[i]);
        }
    }
}
