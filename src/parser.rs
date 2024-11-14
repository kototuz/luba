use std::fmt;

use crate::lexer::*;
use super::{syntax_err, unexpected_token_err, exit_failure};

pub type Block<'a> = Vec<Stmt<'a>>;



#[derive(Debug)]
pub struct Ast<'a> {
    pub fn_decls: Vec<FnDecl<'a>>,
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
    FnCall { name: &'a str, args: Vec<Expr> },
    VarAssign { name: &'a str, expr: Expr },
    VarDeclAssign { name: &'a str, expr: Expr },
    VarDecl(&'a str),
    ReturnVal(Expr),
    Return,
    BuilinFnCall { name: &'a str, arg: &'a str },
    If { cond: Expr, then: Block<'a>, elzeifs: Vec<ElseIf<'a>>, elze: Block<'a>},
    For { body: Block<'a>, init: Option<Box<Stmt<'a>>>, cond: Option<Expr>, post: Option<Box<Stmt<'a>>> },
    Continue,
    Break,
}

#[derive(Debug)]
pub struct ElseIf<'a> {
    pub cond: Expr,
    pub then: Block<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub loc: Loc,
    pub kind: ExprKind
}

#[derive(Debug, PartialEq)]
pub enum ExprKind {
    FnCall(Box<FnCallExpr>),
    BinOp(Box<BinOpExpr>),
    Var(&'static str),
    Num(i32),
}

#[derive(Debug, PartialEq)]
pub struct FnCallExpr {
    pub name: &'static str,
    pub args: Vec<Expr>
}

#[derive(Debug, PartialEq)]
pub struct BinOpExpr {
    pub lhs: Expr,
    pub rhs: Expr,
    pub op: BinOpKind
}


pub fn parse<'a>(lex: &mut Lexer<'a>) -> Ast<'a> {
    let mut ast = Ast {
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
        let has_result = match lex.expect_peek_any() {
            Token::Punct(Punct::OpenCurly) => false,
            Token::Keyword(Keyword::Int) => {
                lex.next_any();
                true
            },
            t @ _ => { unexpected_token_err!(lex.loc, t); }
        };

        // body
        let body = parse_block(lex);

        ast.fn_decls.push(FnDecl {
            name, params, loc,
            has_result, body
        });
    }

    ast
}

fn parse_stmt<'a>(lex: &mut Lexer<'a>) -> Stmt<'a> {
    let loc = lex.loc.clone();
    match lex.expect_peek_any() {
        Token::Keyword(Keyword::For) => {
            lex.next_any();

            let mut init: Option<Box<Stmt>> = None;
            let mut cond: Option<Expr>      = None;
            let mut post: Option<Box<Stmt>> = None;

            match lex.expect_peek_any() {
                Token::Punct(Punct::Semicolon) => { lex.next_any(); },
                _ => {
                    init = Some(Box::new(parse_stmt(lex)));
                    lex.expect_punct(Punct::Semicolon);
                }
            }

            match lex.expect_peek_any() {
                Token::Punct(Punct::Semicolon) => { lex.next_any(); },
                _ => {
                    cond = Some(parse_expr(lex, 0));
                    lex.expect_punct(Punct::Semicolon);
                }
            }

            match lex.expect_peek_any() {
                Token::Punct(Punct::OpenCurly) => {},
                _ => { post = Some(Box::new(parse_stmt(lex))); }
            }

            Stmt {
                loc, kind: StmtKind::For {
                    init, cond, post,
                    body: parse_block(lex)
                }
            }
        },

        Token::Punct(Punct::At) => {
            lex.next_any();

            let name = lex.expect_ident();
            match lex.expect_any() {
                Token::StrLit(lit) => {
                    Stmt {
                        loc, kind: StmtKind::BuilinFnCall {
                            name, arg: lit
                        }
                    }
                },

                t @ _ => {
                    unexpected_token_err!(lex.loc, t);
                }
            }
        },

        Token::Keyword(Keyword::Continue) => {
            lex.next_any();
            Stmt { loc, kind: StmtKind::Continue }
        }

        Token::Keyword(Keyword::Break) => {
            lex.next_any();
            Stmt { loc, kind: StmtKind::Break }
        },

        Token::Keyword(Keyword::If) => {
            lex.next_any();

            let cond = parse_expr(lex, 0);
            let then = parse_block(lex);
            let mut elzeifs: Vec<ElseIf> = Vec::new();
            let mut elze: Block = Block::new();

            while lex.expect_peek_any() == Token::Keyword(Keyword::Else) {
                lex.next_any();
                if lex.expect_peek_any() == Token::Keyword(Keyword::If) {
                    lex.next_any();
                    elzeifs.push(ElseIf {
                        cond: parse_expr(lex, 0),
                        then: parse_block(lex),
                    });
                } else {
                    elze = parse_block(lex);
                    break;
                }
            }

            Stmt {
                loc, kind: StmtKind::If {
                    cond, then, elzeifs, elze
                }
            }
        },

        Token::Keyword(Keyword::Return) => {
            lex.next_any();
            if let Token::Punct(Punct::CloseCurly) = lex.expect_peek_any() {
                Stmt {
                    loc: lex.loc.clone(),
                    kind: StmtKind::Return
                }
            } else {
                let expr = parse_expr(lex, 0);
                Stmt {
                    loc: lex.loc.clone(),
                    kind: StmtKind::ReturnVal(expr)
                }
            }
        },

        Token::Ident(var_name) => {
            lex.next_any();
            match lex.expect_peek_any() {
                Token::Punct(Punct::Colon) => {
                    lex.next_any();
                    lex.expect_punct(Punct::Eq);
                    let expr = parse_expr(lex, 0);
                    Stmt {
                        loc,
                        kind: StmtKind::VarDeclAssign {
                            name: var_name, expr,
                        }
                    }
                },

                Token::Punct(Punct::Eq) => {
                    lex.next_any();
                    let expr = parse_expr(lex, 0);
                    Stmt {
                        loc,
                        kind: StmtKind::VarAssign {
                            name: var_name,
                            expr 
                        }
                    }
                },

                Token::Punct(Punct::OpenParen) => {
                    lex.next_any();

                    let mut args: Vec<Expr> = Vec::new();
                    if lex.expect_peek_any() == Token::Punct(Punct::CloseParen) {
                        lex.next_any();
                        return Stmt {
                            loc, kind: StmtKind::FnCall {
                                args, name: var_name
                            }
                        };
                    }

                    loop {
                        args.push(parse_expr(lex, 0));
                        match lex.expect_any() {
                            Token::Punct(Punct::Comma) => {},
                            Token::Punct(Punct::CloseParen) => break,
                            t @ _ => { unexpected_token_err!(lex.loc, t); }
                        }
                    }

                    Stmt {
                        loc, kind: StmtKind::FnCall {
                            args, name: var_name
                        }
                    }
                },

                _ => Stmt { loc, kind: StmtKind::VarDecl(var_name) }
            }
        },

        t @ _ => { unexpected_token_err!(lex.loc, t); }
    }
}

fn parse_block<'a>( lex: &mut Lexer<'a>) -> Block<'a> {
    let mut block = Block::new();

    lex.expect_punct(Punct::OpenCurly);
    while lex.expect_peek_any() != Token::Punct(Punct::CloseCurly) {
        block.push(parse_stmt(lex));
    }
    lex.expect_punct(Punct::CloseCurly);

    block
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ExprKind::Var(nam) => write!(f, "{nam}"),
            ExprKind::Num(n)   => write!(f, "{n}"),
            ExprKind::BinOp(data) => {
                write!(f, "[{} ", data.op)?;
                write!(f, "{} ", data.lhs)?;
                write!(f, "{}]", data.rhs)
            },
            ExprKind::FnCall(data) => {
                write!(f, "{}(", data.name)?;
                write!(f, "{}", data.args[0])?;
                for i in 1..data.args.len() {
                    write!(f, ", {}", data.args[i])?;
                }
                write!(f, ")")
            },
        }
    }
}

pub fn parse_expr(lex: &mut Lexer, prec: u8) -> Expr {
    // the implementation based on the Pratt Parsing algorithm
    let token: Token;
    let mut lhs = match lex.expect_any() {
        Token::Number(n) => Expr { loc: lex.loc.clone(), kind: ExprKind::Num(n) },
        Token::Ident(name) => {
            if lex.expect_peek_any() == Token::Punct(Punct::OpenParen) {
                lex.next_any();
                let mut args: Vec<Expr> = Vec::new();
                if lex.expect_peek_any() == Token::Punct(Punct::CloseParen) {
                    lex.next_any();
                } else {
                    args.push(parse_expr(lex, 0));
                    while lex.expect_any() != Token::Punct(Punct::CloseParen) {
                        args.push(parse_expr(lex, 0));
                    }
                }

                Expr {
                    loc: lex.loc.clone(),
                    kind: ExprKind::FnCall(Box::new(FnCallExpr {
                        name, args
                    }))
                }
            } else {
                Expr {
                    loc: lex.loc.clone(),
                    kind: ExprKind::Var(name)
                }
            }
        },
        Token::Punct(Punct::OpenParen) => {
            let lhs = parse_expr(lex, 0);
            token = lex.expect_any();
            if token != Token::Punct(Punct::CloseParen) {
                unexpected_token_err!(lex.loc, token);
            }
            lhs
        },
        t @ _ => { unexpected_token_err!(lex.loc, t); }
    };

    loop {
        match lex.expect_peek_any() {
            Token::BinOp(kind) => {
                let this_prec = bin_op_prec(kind.clone());
                if  this_prec < prec {
                    break
                } else {
                    lex.next_any();
                    let rhs = parse_expr(lex, this_prec);
                    lhs = Expr {
                        loc: lex.loc.clone(),
                        kind: ExprKind::BinOp(Box::new(BinOpExpr {
                            lhs, rhs, op:  kind
                        }))
                    }
                }
            },

            _ => break,
        }
    }

    lhs
}

fn bin_op_prec(bin_op_kind: BinOpKind) -> u8 {
    match bin_op_kind {
        BinOpKind::Or  => 0,
        BinOpKind::And => 1,
        BinOpKind::Eq  | BinOpKind::Ne  => 2,
        BinOpKind::Add | BinOpKind::Sub => 4,
        BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod => 5,
        BinOpKind::Gt
        | BinOpKind::Ge
        | BinOpKind::Lt
        | BinOpKind::Le => 3,
    }
}

// TODO: make tests
