use std::ops::Range;

use lexer::*;
use super::{syntax_err, unexpected_token_err, exit_failure};

pub type ExprIdx = usize;
pub type Block<'a> = Vec<Stmt<'a>>;



#[derive(Debug)]
pub struct Ast<'a> {
    pub fn_decls: Vec<FnDecl<'a>>,
    pub expr_buf: Vec<Expr<'a>>,
}


#[derive(Debug)]
pub struct Stmt<'a> {
    pub loc: Loc,
    pub kind: StmtKind<'a>
}

#[derive(Debug)]
pub struct FnDecl<'a> {
    pub name: &'a str,
    pub params: Vec<&'a str>,
    pub has_result: bool,
    pub body: Block<'a>,
    pub loc: Loc,
}

#[derive(Debug)]
pub enum StmtKind<'a> {
    VarAssign { name: &'a str, expr: ExprRange },
    VarDecl(&'a str),
    If { cond: ExprRange, then: Block<'a>, elze: Block<'a> },
    For { cond: ExprRange, body: Block<'a> },
}

#[derive(Debug, Clone)]
pub struct ExprRange {
    pub loc: Loc,
    pub start: ExprIdx,
    pub end: ExprIdx,
}

// TODO: add loc to better error reporting
#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    BinOp(BinOpKind),
    Var(&'a str),
    Num(i32),
    OpenParen,
}



pub fn parse<'a>(lex: &mut Lexer<'a>) -> Ast<'a> {
    let mut ast = Ast {
        expr_buf: Vec::new(),
        fn_decls: Vec::new(),
    };

    while let Some(token) = lex.next_any() {
        // TODO: global variables
        if token != Token::Keyword(Keyword::Fn) {
            unexpected_token_err!(lex.loc, token);
        } 

        let loc = lex.loc.clone();
        let name = lex.expect_ident();

        // parameters
        lex.expect_punct(Punct::OpenParen);
        let mut params: Vec<&str> = Vec::new();
        match lex.expect_any() {
            Token::Punct(Punct::CloseParen) => {},
            Token::Ident(param_name) => {
                params.push(param_name);
                loop {
                    match lex.expect_any() {
                        Token::Punct(Punct::CloseParen) => break,
                        Token::Punct(Punct::Comma) => {
                            params.push(lex.expect_ident());
                        },
                        t @ _ => { unexpected_token_err!(lex.loc, t); }
                    }
                }
            },
            t @ _ => { unexpected_token_err!(lex.loc, t); }
        }

        // result
        let has_result = match lex.expect_any() {
            Token::Punct(Punct::OpenCurly) => false,
            Token::Keyword(Keyword::Int) => {
                lex.expect_punct(Punct::OpenCurly);
                true
            },
            t @ _ => { unexpected_token_err!(lex.loc, t); }
        };

        // body
        let body = parse_block(&mut ast.expr_buf, lex);

        ast.fn_decls.push(FnDecl {
            name, params, loc,
            has_result, body
        });
    }

    ast
}

fn parse_block<'a>(
    expr_buf:  &mut Vec<Expr<'a>>,
    lex: &mut Lexer<'a>,
) -> Block<'a> {
    let mut expr_range: ExprRange;
    let mut token = lex.expect_any();
    let mut block = Block::new();
    loop {
        let loc = lex.loc.clone();
        match token {
            Token::Ident(var_name) => {
                match lex.expect_any() {
                    Token::Punct(Punct::Semicolon) => {
                        block.push(Stmt {
                            loc,
                            kind: StmtKind::VarDecl(var_name)
                        });
                    },

                    Token::Punct(Punct::Colon) => {
                        block.push(Stmt {
                            loc: loc.clone(),
                            kind: StmtKind::VarDecl(var_name)
                        });

                        lex.expect_punct(Punct::Eq);
                        expr_range = parse_expr(
                            expr_buf, lex,
                            Punct::Semicolon
                        );

                        block.push(Stmt {
                            loc,
                            kind: StmtKind::VarAssign {
                                name: var_name,
                                expr: expr_range,
                            }
                        });
                    },

                    Token::Punct(Punct::Eq) => {
                        expr_range = parse_expr(
                            expr_buf, lex,
                            Punct::Semicolon
                        );

                        block.push(Stmt {
                            loc,
                            kind: StmtKind::VarAssign {
                                name: var_name,
                                expr: expr_range,
                            }
                        });
                    },

                    t @ _ => { unexpected_token_err!(lex.loc, t); }
                }
            },

            Token::Keyword(Keyword::For) => {
                expr_range = parse_expr(expr_buf, lex, Punct::OpenCurly);
                block.push(Stmt {
                    loc, kind: StmtKind::For {
                        cond: expr_range,
                        body: parse_block(expr_buf, lex),
                    }
                });
            },

            Token::Keyword(Keyword::If) => {
                expr_range = parse_expr(expr_buf, lex, Punct::OpenCurly);
                let then = parse_block(expr_buf, lex);
                token = lex.expect_any();
                if let Token::Keyword(Keyword::Else) = token {
                    lex.expect_punct(Punct::OpenCurly);
                    let elze = parse_block(expr_buf, lex);
                    block.push(Stmt {
                        loc,
                        kind: StmtKind::If {
                            cond: expr_range,
                            then, elze
                        }
                    });
                } else {
                    block.push(Stmt {
                        loc,
                        kind: StmtKind::If {
                            cond: expr_range,
                            then,
                            elze: Vec::new(),
                        }
                    });
                    continue;
                }
            },

            Token::Punct(Punct::CloseCurly) => break,

            t @ _ => { unexpected_token_err!(lex.loc, t); }
        }
        token = lex.expect_any();
    }

    block
}

pub fn parse_expr<'a>(
    expr_buf: &mut Vec<Expr<'a>>,
    lex: &mut Lexer<'a>,
    end: Punct
) -> ExprRange {
    // the implementation based on: https://en.wikipedia.org/wiki/Shunting_yard_algorithm

    fn bin_op_prec(bin_op_kind: BinOpKind) -> u8 {
        match bin_op_kind {
            BinOpKind::Or  => 0,
            BinOpKind::And => 1,
            BinOpKind::Eq  | BinOpKind::Ne  => 2,
            BinOpKind::Gt
            | BinOpKind::Ge
            | BinOpKind::Lt
            | BinOpKind::Le => 3,
            BinOpKind::Add | BinOpKind::Sub => 4,
            BinOpKind::Mul | BinOpKind::Div => 5,
        }
    }

    let mut ret = ExprRange {
        loc: lex.loc.clone(),
        start: expr_buf.len(), end: 0 
    };

    let mut op_stack: Vec<Expr> = Vec::new();

    loop {
        loop {
            match lex.expect_any() {
                Token::Ident(text) => {
                    expr_buf.push(Expr::Var(text));
                    break;
                },
                Token::Number(num) => {
                    expr_buf.push(Expr::Num(num));
                    break;
                },
                Token::Punct(Punct::OpenParen) => {
                    op_stack.push(Expr::OpenParen);
                },
                t @ _ => { unexpected_token_err!(lex.loc, t); }
            }
        }

        'outer: loop {
            match lex.expect_any() {
                Token::BinOp(kind0) => {
                    while let Some(Expr::BinOp(kind1)) = op_stack.last() {
                        if bin_op_prec(kind1.clone()) < bin_op_prec(kind0.clone()) {
                            break;
                        }
                        expr_buf.push(op_stack.pop().unwrap());
                    }
                    op_stack.push(Expr::BinOp(kind0.clone()));
                    break;
                },

                Token::Punct(Punct::CloseParen) => {
                    while op_stack.last() != Some(&Expr::OpenParen) {
                        if op_stack.is_empty() {
                            syntax_err!(lex.loc, "Mismatched parentheses");
                        }
                        expr_buf.push(op_stack.pop().unwrap());
                    }

                    let _ = op_stack.pop();
                },

                Token::Punct(p) if p == end => {
                    op_stack.reverse();
                    for op in &op_stack {
                        if *op == Expr::OpenParen {
                            syntax_err!(lex.loc, "Mismatched parentheses");
                        }
                    }

                    expr_buf.append(&mut op_stack);
                    ret.end = expr_buf.len();
                    return ret;
                },

                t @ _ => { unexpected_token_err!(lex.loc, t); }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic]
    fn mismatched_parentheses() {
        let mut expr_buf: Vec<Expr> = Vec::new();
        let range = parse_expr(&mut expr_buf, &mut Lexer::new(b"(a));"), Punct::Semicolon);
    }

    #[test]
    #[should_panic]
    fn unexpected_comma() {
        let mut expr_buf: Vec<Expr> = Vec::new();
        let range = parse_expr(&mut expr_buf, &mut Lexer::new(b"(a + a,b);"), Punct::Semicolon);
    }

    #[test]
    #[should_panic]
    fn unexpected_paren() {
        let mut expr_buf: Vec<Expr> = Vec::new();
        let range = parse_expr(&mut expr_buf, &mut Lexer::new(b"a + ();"), Punct::Semicolon);
    }

    #[test]
    fn expr() {
        use super::Expr::*;

        // Program: v1;v2;op
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
        // f(1, f(2 + 3)); => 1 23+ sa f sa sa
        let map: &[(&str, &[Expr])] = &[
            ("1 + 2 == 3 - 1;", &[Num(1), Num(2), BinOp(BinOpKind::Add), Num(3), Num(1), BinOp(BinOpKind::Sub), BinOp(BinOpKind::Eq)]),
            ("f();",           &[FnCall("f")]),
            ("f(1, 2);",       &[Num(1), SetArg(0), Num(2), SetArg(1), FnCall("f")]),
            ("f(1, 2 + 3);",   &[Num(1), SetArg(0), Num(2), Num(3), BinOp(BinOpKind::Add), SetArg(1), FnCall("f")]),
            ("f(1, f(2, 3));", &[Num(1), SetArg(0), Num(2), SetArg(0), Num(3), SetArg(1), FnCall("f"), SetArg(1), FnCall("f")]),
            ("f(1, f(2, 3 + 4));", &[Num(1), SetArg(0), Num(2), SetArg(0), Num(3), Num(4), BinOp(BinOpKind::Add), SetArg(1), FnCall("f"), SetArg(1), FnCall("f")]),
            ("f(f(1, 2), f(3, 4));", &[Num(1), SetArg(0), Num(2), SetArg(1), FnCall("f"), SetArg(0), Num(3), SetArg(0), Num(4), SetArg(1), FnCall("f"), SetArg(1), FnCall("f")]),
            ("1 + 2;",         &[Num(1), Num(2), BinOp(BinOpKind::Add)]),
            ("1 + 2 + 3;",     &[Num(1), Num(2), BinOp(BinOpKind::Add), Num(3), BinOp(BinOpKind::Add)]),
            ("1 + 2*3;",       &[Num(1), Num(2), Num(3), BinOp(BinOpKind::Mul), BinOp(BinOpKind::Add)]),
            ("1 * 2 * 3;",     &[Num(1), Num(2), BinOp(BinOpKind::Mul), Num(3), BinOp(BinOpKind::Mul)]),
            ("1 + 2*3*4;",     &[Num(1), Num(2), Num(3), BinOp(BinOpKind::Mul), Num(4), BinOp(BinOpKind::Mul), BinOp(BinOpKind::Add)]),
            ("1 + 2*3*4 + 5;", &[Num(1), Num(2), Num(3), BinOp(BinOpKind::Mul), Num(4), BinOp(BinOpKind::Mul), BinOp(BinOpKind::Add), Num(5), BinOp(BinOpKind::Add)]),
            ("1 + 2*3 + 4*5;", &[Num(1), Num(2), Num(3), BinOp(BinOpKind::Mul), BinOp(BinOpKind::Add), Num(4), Num(5), BinOp(BinOpKind::Mul), BinOp(BinOpKind::Add)]),
            ("1 / 2 * 3;",     &[Num(1), Num(2), BinOp(BinOpKind::Div), Num(3), BinOp(BinOpKind::Mul)]),
            ("1 / 2 * 3 * 4;", &[Num(1), Num(2), BinOp(BinOpKind::Div), Num(3), BinOp(BinOpKind::Mul), Num(4), BinOp(BinOpKind::Mul)]),
            ("1 / 2 * 3 / 4;", &[Num(1), Num(2), BinOp(BinOpKind::Div), Num(3), BinOp(BinOpKind::Mul), Num(4), BinOp(BinOpKind::Div)]),
            ("1 * (2 + 3);",   &[Num(1), Num(2), Num(3), BinOp(BinOpKind::Add), BinOp(BinOpKind::Mul)]),
            ("1 * (2 + 3) + 2;",   &[Num(1), Num(2), Num(3), BinOp(BinOpKind::Add), BinOp(BinOpKind::Mul), Num(2), BinOp(BinOpKind::Add)]),
            ("3 + 4 * 2 / (1 - 5);", &[Num(3), Num(4), Num(2), BinOp(BinOpKind::Mul), Num(1), Num(5), BinOp(BinOpKind::Sub), BinOp(BinOpKind::Div), BinOp(BinOpKind::Add)])
        ];

        let mut exprs: Vec<Expr> = Vec::new();
        for test in map {
            let range = parse_expr(&mut exprs, &mut Lexer::new(test.0.as_bytes()), Punct::Semicolon);
            for x in range {
                assert_eq!(exprs[x], test.1[x], "{:?}", test);
            }

            exprs.clear();
        }
    }
}
