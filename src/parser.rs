use std::ops::Range;

use lexer::*;
use super::Result;

type ExprRange = Range<usize>;
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

pub struct FnDecl<'a> {
    pub name: &'a str,
    pub blocks: BlockRange, // blocks[0] - function body
    pub params: Vec<&'a str>
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    OpSub,
    OpAdd,
    OpMul,
    OpDiv,
    OpenParen,
    SetArg(u32),
    Var(&'a str),
    FnCall(&'a str),
    Num(i32),
}

pub struct Program<'a> {
    pub exprs:  Vec<Expr<'a>>,
    pub stmts:  Vec<Stmt<'a>>,
    pub fns:    Vec<FnDecl<'a>>,
    pub blocks: Vec<Block>,
}

pub fn parse<'a>(lex: &mut Lexer<'a>) -> Result<Program<'a>> {
    let mut ret = Program {
        exprs: Vec::new(),
        stmts: Vec::new(),
        fns:   Vec::new(),
        blocks: Vec::new()
    };

    let mut curr_block_idx = 0;
    while let Some(tok) = lex.next()? {
        if tok.kind != TokenKind::KeywordFn {
            eprintln!(
                "ERROR:{}: function declaration was expected, but found `{}`",
                lex.loc,
                tok.kind,
            );
            return Err(());
        }

        let mut fn_decl = FnDecl {
            name: lex.expect_next(TokenKind::Name)?.text,
            params: Vec::new(), // TODO: maybe make one param buffer for every `fn_decl`
            blocks: BlockRange { start: ret.blocks.len(), end: 0 },
        };

        ret.blocks.push(Block {
            parent: None,
            range: BlockRange {
                start: ret.stmts.len(),
                end: 0
            }
        });

        { // reading parameters
            let _ = lex.expect_next(TokenKind::OpenParen)?;
            let mut tok = lex.expect_next_oneof(&[TokenKind::Name, TokenKind::CloseParen])?;
            if tok.kind == TokenKind::Name {
                fn_decl.params.push(tok.text);
                tok = lex.expect_next_oneof(&[TokenKind::Comma, TokenKind::CloseParen])?;
                while tok.kind == TokenKind::Comma {
                    fn_decl.params.push(lex.expect_next(TokenKind::Name)?.text);
                    tok = lex.expect_next_oneof(&[TokenKind::Comma, TokenKind::CloseParen])?;
                }
            }
        }

        let _ = lex.expect_next(TokenKind::OpenCurly)?;
        while let Some(t) = lex.next()? {
            match t.kind {
                TokenKind::Name => {
                    let _ = lex.expect_next(TokenKind::Eq)?;
                    let expr = parse_expr(&mut ret.exprs, lex, TokenKind::Semicolon)?;
                    ret.stmts.push(Stmt::VarAssign { name: t.text, expr });
                },

                TokenKind::KeywordReturn => {
                    ret.stmts.push(Stmt::Return(parse_expr(&mut ret.exprs, lex, TokenKind::Semicolon)?));
                },

                // var
                // var
                // if
                //   var
                //   if
                //     var
                //     var
                //   if
                //     var
                //     var
                //     var
                //     var
                //   var
                //   var
                // var
                TokenKind::KeywordIf => {
                    let expr = parse_expr(&mut ret.exprs, lex, TokenKind::OpenCurly)?;
                    ret.stmts.push(Stmt::If {
                        cond: expr,
                        body: ret.blocks.len() - fn_decl.blocks.start
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

                TokenKind::CloseCurly => {
                    ret.blocks[curr_block_idx].range.end = ret.stmts.len();
                    if let Some(parent_idx) = ret.blocks[curr_block_idx].parent {
                        curr_block_idx = parent_idx;
                    } else {
                        break;
                    }
                },

                _ => todo!()
            }
        }

        curr_block_idx += 1;
        fn_decl.blocks.end = ret.blocks.len();
        ret.fns.push(fn_decl);
    }

    Ok(ret)
}

pub fn parse_expr<'a>(
    expr_buf: &mut Vec<Expr<'a>>,
    lex: &mut Lexer<'a>,
    end_token: TokenKind
) -> Result<ExprRange> {
    // the implementation based on: https://en.wikipedia.org/wiki/Shunting_yard_algorithm

    let mut ret = ExprRange { start: expr_buf.len(), end: 0 };
    let mut op_stack: Vec<Expr> = Vec::new();

    //expr_buf.push(expect_read_expr(lex, &mut op_stack)?);

    const READ: &[TokenKind] = 
        &[TokenKind::Name, TokenKind::Num, TokenKind::OpenParen];
    const READ_OR_CLOSE_PAREN: &[TokenKind] =
        &[TokenKind::Name, TokenKind::Num, TokenKind::OpenParen, TokenKind::CloseParen];
    let OPERATIONS_OR_CLOSE_PAREN_OR_SEMICOLON: &[TokenKind] = &[
        TokenKind::Plus,
        TokenKind::Minus,
        TokenKind::Star,
        TokenKind::Slash,
        TokenKind::Comma,
        TokenKind::CloseParen,
        end_token
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

            // f(1, f(2 + 3)) => 1s(0)23+s(0)fs(1)f
            // stack:
            TokenKind::Name => {
                if let Some(Token { kind: TokenKind::OpenParen, .. }) = lex.peek()? {
                    op_stack.push(Expr::FnCall(token.text));
                    op_stack.push(Expr::OpenParen);
                    let _ = lex.next();
                    token = lex.expect_next_oneof(READ_OR_CLOSE_PAREN)?;
                    if token.kind != TokenKind::CloseParen {
                        op_stack.push(Expr::SetArg(0));
                    }
                } else {
                    expr_buf.push(Expr::Var(token.text));
                    token = lex.expect_next_oneof(OPERATIONS_OR_CLOSE_PAREN_OR_SEMICOLON)?;
                }
            },

            TokenKind::Num  => {
                expr_buf.push(Expr::Num(token.text.parse::<i32>().unwrap()));
                token = lex.expect_next_oneof(OPERATIONS_OR_CLOSE_PAREN_OR_SEMICOLON)?;
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
                if matches!(op_stack.last(), Some(&Expr::FnCall(_))) {
                    expr_buf.push(op_stack.pop().unwrap());
                }

                token = lex.expect_next_oneof(OPERATIONS_OR_CLOSE_PAREN_OR_SEMICOLON)?;
            },

            TokenKind::Comma => {
                loop {
                    if let Some(&Expr::SetArg(i)) = op_stack.last() {
                        expr_buf.push(op_stack.pop().unwrap());
                        op_stack.push(Expr::SetArg(i+1));
                        break;
                    } else {
                        expr_buf.push(op_stack.pop().unwrap());
                        if op_stack.is_empty() {
                            eprintln!("ERROR:{}: unexpected `,`", lex.loc);
                            return Err(())
                        }
                    }
                }
                token = lex.expect_next_oneof(READ)?;
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

            end_token => break,

            _ => unreachable!()
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
            ("f();",           &[FnCall("f")]),
            ("f(1, 2);",       &[Num(1), SetArg(0), Num(2), SetArg(1), FnCall("f")]),
            ("f(1, f(2, 3));", &[Num(1), SetArg(0), Num(2), SetArg(0), Num(3), SetArg(1), FnCall("f"), SetArg(1), FnCall("f")]),
            ("f(f(1, 2), f(3, 4));", &[Num(1), SetArg(0), Num(2), SetArg(1), FnCall("f"), SetArg(0), Num(3), SetArg(0), Num(4), SetArg(1), FnCall("f"), SetArg(1), FnCall("f")]),
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
            let range = parse_expr(&mut exprs, &mut Lexer::new(test.0.as_bytes())).unwrap();
            for x in range {
                assert_eq!(exprs[x], test.1[x], "{:?}", test);
            }

            exprs.clear();
        }
    }
}
