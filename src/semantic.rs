use std::collections::HashMap;

use crate::parser::*;
use super::{semantic_err, exit_failure};

type SymbolTable<'a> = Vec<&'a str>;
type FnDeclIdx = usize;

pub struct Analyzer<'a> {
    pub global_st: HashMap<&'a str, FnDecl>,
    pub local_st: SymbolTable<'a>,
}


pub struct FnDecl {
    param_count: usize,
    ip: usize,
    has_result: bool,
}

impl<'a> Analyzer<'a> {
    pub fn new() -> Self {
        Self {
            global_st: HashMap::new(),
            local_st: SymbolTable::new(),
        }
    }

    pub fn analyze_fn_decl(&mut self, fn_decl: &crate::parser::FnDecl<'a>, ip: usize) {
        if self.global_st.get(&fn_decl.name).is_some() {
            semantic_err!(
                fn_decl.loc,
                "Redeclaration of function `{}`",
                fn_decl.name
            );
        }

        self.global_st.insert(fn_decl.name, FnDecl {
            ip, param_count: fn_decl.params.len(),
            has_result: fn_decl.has_result
        });

        for param in &fn_decl.params {
            self.local_st.push(param);
        }

        self.analyze_block(&fn_decl.body);
    }

    pub fn var_sp2_offset(&self, name: &'a str) -> usize {
        for (i, entry) in self.local_st.iter().enumerate() {
            if *entry == name {
                return i;
            }
        }
        panic!("name `{name}` is not in ast or you forget to analyze the fn_decl before");
    }

    fn analyze_block(&mut self, block: &Block<'a>) {
        for stmt in block {
            match &stmt.kind {
                StmtKind::VarDecl(name) => {
                    if self.local_st.contains(&name) {
                        semantic_err!(stmt.loc, "Redeclaration of variable `{name}`");
                    }
                    self.local_st.push(name);
                },

                StmtKind::If { cond, then, elze } => {
                    self.analyze_expr(cond);
                    self.analyze_block(then);
                    self.analyze_block(elze);
                },

                StmtKind::For { cond, body } => {
                    self.analyze_expr(cond);
                    self.analyze_block(body);
                },

                StmtKind::VarAssign { name, expr } => {
                    if !self.local_st.contains(&name) {
                        semantic_err!(stmt.loc, "Assignment to undeclared variable `{name}`");
                    }
                    self.analyze_expr(expr);
                },
            }
        }
    }

    fn analyze_expr(&self, expr: &Expr) {
        match expr {
            Expr::Num(_) => {},
            Expr::Var(name) => if !self.local_st.contains(name) {
                todo!();
            },
            Expr::BinOp { lhs, rhs, .. } => {
                self.analyze_expr(lhs);
                self.analyze_expr(rhs);
            },
            Expr::FnCall { name, args } => {
                if self.global_st.get(name).is_none() {
                    todo!();
                }
                for arg in args {
                    self.analyze_expr(arg);
                }
            },
        }
    }
}

