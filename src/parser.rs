use std::ops::Range;

use lexer::*;
use super::{syntax_err, exit_failure};

pub type ExprRange = Range<usize>;
type StmtIdx =  usize;
type BlockIdx = usize;
type BlockRange = Range<BlockIdx>;

#[derive(Debug)]
pub struct Block {
    parent: Option<BlockIdx>,
    pub range: BlockRange
}

#[derive(Debug)]
pub enum Stmt<'a> {
    VarAssign { name: &'a str, expr: ExprRange },
    Return(ExprRange),
    If { cond: ExprRange, body: BlockIdx }
}

// STMT_BUFFER:
// if_block(cond: 1==1, block_len: 3)
//   var_assign
//   var_assign
//   fn_call
// if_block(cond: 1==1, block_len: 3)
//   var_assign
//   var_assign
//   if_block(cond: 1==1, block_len: 3)
// var_assign

#[derive(Debug)]
pub struct FnDecl<'a> {
    pub name: &'a str,
    pub blocks: BlockRange, // blocks[0] - function body
    pub params: Vec<&'a str>
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    BinOp(BinOpKind),
    OpenParen,
    SetArg(u32),
    Var(&'a str),
    FnCall(&'a str),
    Num(i64),
}

pub struct Program<'a> {
    pub exprs:  Vec<Expr<'a>>,
    pub stmts:  Vec<Stmt<'a>>,
    pub fns:    Vec<FnDecl<'a>>,
    pub blocks: Vec<Block>,
}


pub fn parse<'a>(lex: &mut Lexer<'a>) -> Program<'a> {
    let mut ret = Program {
        exprs: Vec::new(),
        stmts: Vec::new(),
        fns:   Vec::new(),
        blocks: Vec::new()
    };

    let mut curr_block_idx = 0;
    while let Some(token) = lex.next_any() {
        if !matches!(token, Token::Keyword(Keyword::Fn)) {
            lex.unexpected_token_err(token);
        }

        let mut fn_decl = FnDecl {
            name: lex.expect_ident(),
            params: Vec::new(), // TODO: maybe make one param buffer for every `fn_decl`
            blocks: BlockRange { start: curr_block_idx, end: 0 },
        };

        ret.blocks.push(Block {
            parent: None,
            range: BlockRange {
                start: ret.stmts.len(),
                end: 0
            }
        });

        { // reading parameters
            lex.expect_punct(Punct::OpenParen);
            match lex.expect_any() {
                Token::Ident(text) => {
                    fn_decl.params.push(text);
                    loop {
                        match lex.expect_any() {
                            Token::Punct(Punct::Comma) => {
                                fn_decl.params.push(lex.expect_ident())
                            },
                            Token::Punct(Punct::CloseParen) => break,
                            t @ _ => lex.unexpected_token_err(t)
                        }
                    }
                },
                Token::Punct(Punct::CloseParen) => {},
                t @ _ => lex.unexpected_token_err(t)
            }
        }

        lex.expect_punct(Punct::OpenCurly);

        while let Some(token) = lex.next_any() {
            match token {
                Token::Ident(text) => {
                    lex.expect_punct(Punct::Eq);
                    let expr = parse_expr(&mut ret.exprs, lex, Punct::Semicolon);
                    ret.stmts.push(Stmt::VarAssign { name: text, expr });
                },

                Token::Keyword(Keyword::Return) => {
                    ret.stmts.push(
                        Stmt::Return(
                            parse_expr(&mut ret.exprs, lex, Punct::Semicolon)
                        )
                    )
                },

                Token::Keyword(Keyword::If) => {
                    let expr = parse_expr(&mut ret.exprs, lex, Punct::OpenCurly);
                    ret.stmts.push(Stmt::If {
                        cond: expr,
                        body: ret.blocks.len()
                    });

                    ret.blocks.push(Block {
                        parent: Some(curr_block_idx),
                        range: BlockRange {
                            start: ret.stmts.len(),
                            end: 0
                        }
                    });

                    curr_block_idx = ret.blocks.len()-1;
                },

                Token::Punct(Punct::CloseCurly) => {
                    ret.blocks[curr_block_idx].range.end = ret.stmts.len();
                    if let Some(parent_idx) = ret.blocks[curr_block_idx].parent {
                        curr_block_idx = parent_idx;
                    } else {
                        break;
                    }
                },

                t @ _ => lex.unexpected_token_err(t)
            }
        }

        curr_block_idx = ret.blocks.len();
        fn_decl.blocks.end = ret.blocks.len();
        ret.fns.push(fn_decl);
    }

    ret
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

    let mut ret = ExprRange { start: expr_buf.len(), end: 0 };
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
                Token::Punct(Punct::CloseParen) => {
                    if let Some(Expr::OpenParen) = op_stack.pop() {
                        if let Some(Expr::FnCall(name)) = op_stack.pop() {
                            expr_buf.push(Expr::FnCall(name));
                            break;
                        }
                    }
                    lex.unexpected_token_err(Token::Punct(Punct::CloseParen));
                },
                t @ _ => lex.unexpected_token_err(t)
            }
        }

        'outer: loop {
            match lex.expect_any() {
                Token::Punct(Punct::Comma) => {
                    loop {
                        match op_stack.last() {
                            Some(&Expr::OpenParen) => {
                                if op_stack.len() < 2 || !matches!(
                                    op_stack.get(op_stack.len()-2),
                                    Some(Expr::FnCall(_))
                                ) {
                                    lex.unexpected_token_err(Token::Punct(Punct::Comma));
                                }
                                expr_buf.push(Expr::SetArg(0));
                                op_stack.push(Expr::SetArg(1));
                                break 'outer;
                            },

                            Some(&Expr::SetArg(idx)) => {
                                expr_buf.push(op_stack.pop().unwrap());
                                op_stack.push(Expr::SetArg(idx+1));
                                break 'outer;
                            },

                            Some(_) => expr_buf.push(op_stack.pop().unwrap()),
                            None => lex.unexpected_token_err(Token::Punct(Punct::Comma))
                        }
                    }
                },

                Token::Punct(Punct::OpenParen) => {
                    let Some(Expr::Var(name)) = expr_buf.pop()
                    else {
                        lex.unexpected_token_err(Token::Punct(Punct::OpenParen));
                    };
                    op_stack.push(Expr::FnCall(name));
                    op_stack.push(Expr::OpenParen);
                    break;
                },

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
                    if matches!(op_stack.last(), Some(&Expr::FnCall(_))) {
                        expr_buf.push(op_stack.pop().unwrap());
                    }
                },

                Token::Punct(end) => {
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

                t @ _ => lex.unexpected_token_err(t)
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
