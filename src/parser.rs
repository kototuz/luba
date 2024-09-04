use crate::lexer as lex;
use std::process;

pub struct Parser<'a> {
    expr_buf: Vec<Expr<'a>>,
}

#[derive(Debug)]
pub enum Stmt<'a> {
    VarAssign { name: &'a str, expr: Expr<'a> }
}

#[derive(Debug,  Clone)]
pub enum OpKind {
    Sub,
    Add,
    Mul,
    Div,
}

#[derive(Debug)]
struct Op {
    lhs_idx: usize,
    rhs_idx: usize,
    kind:    OpKind
}

#[derive(Debug)]
pub enum Expr<'a> {
    Var(&'a str),
    Num(i32),
    Op(Op)
}


//impl<'a> Parser<'a> {
//    pub fn new() -> Self {
//        Self { expr_buf: Vec::new() }
//    }

pub fn parse<'a>(lex: &mut lex::Lexer<'a>) -> (Vec<Expr<'a>>, Vec<Stmt<'a>>) {
    use lex::*;

    let mut stmt_buf: Vec<Stmt> = Vec::new();
    let mut expr_buf: Vec<Expr> = Vec::new();

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

fn parse_expr<'a>(expr_buf: &mut Vec<Expr<'a>>, lex: &mut lex::Lexer<'a>) -> Expr<'a> {
    use lex::*;

    fn parse_num(src: &str) -> i32 {
        src.parse::<i32>().unwrap_or_else(|err| {
            eprintln!("ERROR: parser: could not parse num `{src}`: {err}");
            process::exit(1);
        })
    }

    // var = a+b+c+d;

    let mut token: Token;
    let mut expr: Expr;
    let mut op = Op { lhs_idx: 0, rhs_idx: 0, kind: OpKind::Add };

    token = lex.expect_next();
    expr = match token.kind {
        TokenKind::Name => Expr::Var(token.data),
        TokenKind::Num  => Expr::Num(parse_num(token.data)),
        _ => unreachable!()
    };

    op.kind = match lex.expect_peek().kind {
        TokenKind::Plus  => OpKind::Add,
        TokenKind::Minus => OpKind::Sub,
        TokenKind::Star  => OpKind::Mul,
        TokenKind::Slash => OpKind::Div,
        _ => return expr
    };

    let _ = lex.next();

    expr_buf.push(expr);
    op.lhs_idx = expr_buf.len()-1;
    op.rhs_idx = expr_buf.len();

    loop {
        token = lex.expect_next();
        expr = match token.kind {
            TokenKind::Name => Expr::Var(token.data),
            TokenKind::Num  => Expr::Num(parse_num(token.data)),
            _ => unreachable!()
        };

        expr_buf.push(expr);

        let kind = match lex.expect_peek().kind {
            TokenKind::Plus  => OpKind::Add,
            TokenKind::Minus => OpKind::Sub,
            TokenKind::Star  => OpKind::Mul,
            TokenKind::Slash => OpKind::Div,
            _  => return Expr::Op(op)
        };

        let _ = lex.next();

        expr_buf.push(Expr::Op(op));
        op = Op {
            lhs_idx: expr_buf.len()-1,
            rhs_idx: expr_buf.len(),
            kind
        };
    }
}
//}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_test() {
        let source = "a = 1 + 2 + 3;";
        let (exprs, stmts) = parse(&mut lex::Lexer::new(source));

        assert_eq!(exprs.len(), 4);
        assert!(matches!(&exprs[0], Expr::Num(1)));
        assert!(matches!(&exprs[1], Expr::Num(2)));
        let Expr::Op(op) = &exprs[2] else { panic!(); };
        assert_eq!(op.lhs_idx, 0);
        assert_eq!(op.rhs_idx, 1);
        assert!(matches!(op.kind, OpKind::Add));
        assert!(matches!(&exprs[3], Expr::Num(3)));

        assert_eq!(stmts.len(), 1);
        let Stmt::VarAssign { name, expr } = &stmts[0];
        let Expr::Op(op) = expr else { panic!(); };
        assert_eq!(name, &"a");
        assert_eq!(op.lhs_idx, 2);
        assert_eq!(op.rhs_idx, 3);
        assert!(matches!(op.kind, OpKind::Add));
    }
}
