use std::ops::Range;

use lexer::*;
use super::Result;

#[derive(Debug)]
pub enum Stmt<'a> {
    VarAssign { name: &'a str, expr: Range<usize> },
    Return(Expr<'a>), // TODO: full capabilities of expressions
}

type Block = Range<usize>;

pub struct FnDecl<'a> {
    pub name: &'a str,
    pub body: Block
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    OpSub,
    OpAdd,
    OpMul,
    OpDiv,

    OpenParen,

    Var(&'a str),
    FnCall(&'a str),
    Num(i32),
}

pub struct Syntax<'a> {
    pub exprs: Vec<Expr<'a>>,
    pub stmts: Vec<Stmt<'a>>,
    pub fns:   Vec<FnDecl<'a>>
}

pub fn parse<'a>(lex: &mut Lexer<'a>) -> Result<Syntax<'a>> {
    let mut ret = Syntax {
        exprs: Vec::new(),
        stmts: Vec::new(),
        fns:   Vec::new(),
    };

    while let Some(tok) = lex.next()? {
        if tok.kind != TokenKind::KeywordFn {
            eprintln!(
                "ERROR:{}: function declaration was expected, but found `{}`",
                lex.loc,
                tok.kind,
            );
            return Err(());
        }

        let name = lex.expect_next(TokenKind::Name)?;

        // TODO: parameters
        let _ = lex.expect_next(TokenKind::OpenParen)?;
        let _ = lex.expect_next(TokenKind::CloseParen)?;

        let _ = lex.expect_next(TokenKind::OpenCurly)?;

        let mut block = Block { start: ret.stmts.len(), end: ret.stmts.len() };
        while let Some(t) = lex.next()? {
            match t.kind {
                TokenKind::Name => {
                    let _ = lex.expect_next(TokenKind::Eq)?;
                    let expr = parse_expr(&mut ret.exprs, lex)?;
                    let _ = lex.expect_next(TokenKind::Semicolon)?;
                    ret.stmts.push(Stmt::VarAssign { name: t.text, expr });
                    block.end += 1;
                },

                TokenKind::KeywordReturn => {
                    let token = lex.expect_next_oneof(&[
                        TokenKind::Name,
                        TokenKind::Num
                    ])?;
                    let _ = lex.expect_next(TokenKind::Semicolon)?;
                    ret.stmts.push(Stmt::Return(
                            match token.kind {
                                TokenKind::Name => Expr::Var(token.text),
                                TokenKind::Num  => Expr::Num(
                                    token.text.parse::<i32>().unwrap()
                                ),
                                _ => unreachable!()
                            }
                    ));
                    block.end += 1;
                },

                TokenKind::CloseCurly => break,
                _ => todo!()
            }
        }

        ret.fns.push(FnDecl {
            name: name.text,
            body: block
        });
    }

    Ok(ret)
}

pub fn parse_expr<'a>(
    expr_buf: &mut Vec<Expr<'a>>,
    lex: &mut Lexer<'a>,
) -> Result<Range<usize>> {
    // the implementation based on: https://en.wikipedia.org/wiki/Shunting_yard_algorithm

    let mut ret = Range { start: expr_buf.len(), end: 0 };
    let mut op_stack: Vec<Expr> = Vec::new();

    //expr_buf.push(expect_read_expr(lex, &mut op_stack)?);

    const READ: &[TokenKind] = 
        &[TokenKind::Name, TokenKind::Num, TokenKind::OpenParen];
    const READ_OR_CLOSE_PAREN: &[TokenKind] =
        &[TokenKind::Name, TokenKind::Num, TokenKind::OpenParen, TokenKind::CloseParen];
    const OPERATIONS_OR_CLOSE_PAREN: &[TokenKind] = &[
        TokenKind::Plus,
        TokenKind::Minus,
        TokenKind::Star,
        TokenKind::Slash,
        TokenKind::CloseParen
    ];

    let mut token = lex.expect_next_oneof(READ)?;
    loop {
        match token.kind {
            TokenKind::OpenParen => {
                while token.kind == TokenKind::OpenParen {
                    op_stack.push(Expr::OpenParen);
                    token = lex.expect_next_oneof(READ)?;
                }
            },

            TokenKind::Name => {
                if let Some(Token { kind: TokenKind::OpenParen, .. }) = lex.peek()? {
                    op_stack.push(Expr::FnCall(token.text));
                    op_stack.push(Expr::OpenParen);
                    let _ = lex.next();
                    token = lex.expect_next_oneof(READ_OR_CLOSE_PAREN)?;
                } else {
                    expr_buf.push(Expr::Var(token.text));
                    token = if let Some(res) = lex.peek()? {
                        if !OPERATIONS_OR_CLOSE_PAREN.contains(&res.kind) { break }
                        let _ = lex.next();
                        res
                    } else { break };
                }
            },

            TokenKind::Num  => {
                expr_buf.push(Expr::Num(token.text.parse::<i32>().unwrap()));
                token = if let Some(res) = lex.peek()? {
                    if !OPERATIONS_OR_CLOSE_PAREN.contains(&res.kind) { break }
                    let _ = lex.next();
                    res
                } else { break };
            },

            TokenKind::CloseParen => {
                while op_stack.last() != Some(&Expr::OpenParen) {
                    expr_buf.push(op_stack.pop().unwrap());
                    if op_stack.is_empty() {
                        eprintln!("ERROR:{}: mismatched parentheses", lex.loc);
                        return Err(());
                    }
                }
                let _ = op_stack.pop();
                token = if let Some(res) = lex.peek()? {
                    if !OPERATIONS_OR_CLOSE_PAREN.contains(&res.kind) { break }
                    let _ = lex.next();
                    res
                } else { break };
            },

            TokenKind::Plus => {
                while let Some(op) = op_stack.last() {
                    if *op == Expr::OpenParen { break; }
                    expr_buf.push(op_stack.pop().unwrap());
                }
                op_stack.push(Expr::OpAdd);
                token = lex.expect_next_oneof(READ)?;
            },

            TokenKind::Minus => {
                while let Some(op) = op_stack.last() {
                    if *op == Expr::OpenParen { break; }
                    expr_buf.push(op_stack.pop().unwrap());
                }
                op_stack.push(Expr::OpSub);
                token = lex.expect_next_oneof(READ)?;
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
                token = lex.expect_next_oneof(READ)?;
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
                token = lex.expect_next_oneof(READ)?;
            },

            _ => break
        }
    }

    op_stack.reverse();
    for op in &op_stack {
        if *op == Expr::OpenParen {
            eprintln!("ERROR:{}: mismatched parentheses", lex.loc);
            return Err(());
        }
    }

    expr_buf.append(&mut op_stack);
    ret.end = expr_buf.len();
    Ok(ret)
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
            ("f()",           &[FnCall("f")]),
            ("1 + 2",         &[Num(1), Num(2), OpAdd]),
            ("1 + 2 + 3",     &[Num(1), Num(2), OpAdd, Num(3), OpAdd]),
            ("1 + 2*3",       &[Num(1), Num(2), Num(3), OpMul, OpAdd]),
            ("1 * 2 * 3",     &[Num(1), Num(2), OpMul, Num(3), OpMul]),
            ("1 + 2*3*4",     &[Num(1), Num(2), Num(3), OpMul, Num(4), OpMul, OpAdd]),
            ("1 + 2*3*4 + 5", &[Num(1), Num(2), Num(3), OpMul, Num(4), OpMul, OpAdd, Num(5), OpAdd]),
            ("1 + 2*3 + 4*5", &[Num(1), Num(2), Num(3), OpMul, OpAdd, Num(4), Num(5), OpMul, OpAdd]),
            ("1 / 2 * 3",     &[Num(1), Num(2), OpDiv, Num(3), OpMul]),
            ("1 / 2 * 3 * 4", &[Num(1), Num(2), OpDiv, Num(3), OpMul, Num(4), OpMul]),
            ("1 / 2 * 3 / 4", &[Num(1), Num(2), OpDiv, Num(3), OpMul, Num(4), OpDiv]),
            ("1 * (2 + 3)",   &[Num(1), Num(2), Num(3), OpAdd, OpMul]),
            ("1 * (2 + 3) + 2",   &[Num(1), Num(2), Num(3), OpAdd, OpMul, Num(2), OpAdd]),
            ("3 + 4 * 2 / (1 - 5)", &[Num(3), Num(4), Num(2), OpMul, Num(1), Num(5), OpSub, OpDiv, OpAdd])
        ];

        let mut exprs: Vec<Expr> = Vec::new();
        for test in map {
            let range = parse_expr(&mut exprs, &mut Lexer::new(test.0.as_bytes())).unwrap();
            for x in range {
                assert_eq!(exprs[x], test.1[x]);
            }

            exprs.clear();
        }
    }
}
