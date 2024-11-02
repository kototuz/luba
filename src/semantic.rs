use std::collections::HashMap;

use crate::parser::*;
use super::{semantic_err, exit_failure};

type SymbolTable<'a> = Vec<&'a str>;
type FnDeclIdx = usize;

pub struct Analyzer<'a> {
    pub global_st: HashMap<&'a str, FnDecl>,
    pub local_st:  HashMap<&'a str, usize>,
    pub local_idx: usize,
    pub local_count: usize,
    pub param_count: usize,
    pub has_result: bool,
}


pub struct FnDecl {
    pub param_count: usize,
    pub has_result: bool,
    pub ip: usize,
}

impl<'a> Analyzer<'a> {
    pub fn new() -> Self {
        Self {
            global_st: HashMap::new(),
            local_st: HashMap::new(),
            local_idx:   0,
            param_count: 0,
            local_count: 0,
            has_result:  false,
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

        self.local_idx = if fn_decl.has_result { 1 } else { 0 };
        self.local_st.clear();
        self.global_st.insert(fn_decl.name, FnDecl {
            ip, param_count: fn_decl.params.len(),
            has_result: fn_decl.has_result
        });

        self.param_count = fn_decl.params.len();
        for param in &fn_decl.params {
            self.local_st.insert(param, self.local_idx);
            self.local_idx += 1;
        }

        self.has_result = fn_decl.has_result;

        self.local_idx += 2; // this slots reserved for ip and sp2
        self.analyze_block(&fn_decl.body);
        self.local_count = self.local_st.len() - self.param_count;
    }

    pub fn var_sp2_offset(&self, name: &'a str) -> usize {
        *self.local_st.get(name)
            .expect("the name is not in ast or you forget to analyze the fn_decl before")
    }

    fn analyze_block(&mut self, block: &Block<'a>) {
        for stmt in block {
            match &stmt.kind {
                StmtKind::VarDecl(name) => {
                    if self.local_st.contains_key(name) {
                        semantic_err!(stmt.loc, "Redeclaration of variable `{name}`");
                    }
                    self.local_st.insert(name, self.local_idx);
                    self.local_idx += 1;
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
                    if !self.local_st.contains_key(name) {
                        semantic_err!(stmt.loc, "Assignment to undeclared variable `{name}`");
                    }
                    self.analyze_expr(expr);
                },

                StmtKind::ReturnVal(expr) => {
                    if !self.has_result {
                        semantic_err!(stmt.loc, "The function doesn't return value");
                    }
                    self.analyze_expr(expr)
                },

                StmtKind::Return => {
                    if self.has_result {
                        semantic_err!(stmt.loc, "The function must return value");
                    }
                },

                StmtKind::FnCall { name, args } => {
                    if self.global_st.get(name).is_none() {
                        semantic_err!(stmt.loc, "Global function `{name}` is not defined");
                    }
                    for arg in args {
                        self.analyze_expr(arg);
                    }
                },
            }
        }
    }

    fn analyze_expr(&self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Num(_)       => {},
            ExprKind::Var(name)    => {
                if self.local_st.get(name).is_none() {
                    semantic_err!(expr.loc, "Variable `{name}` doesn't exist");
                }
            },

            ExprKind::FnCall(data) => {
                if self.global_st.get(data.name).is_none() {
                    semantic_err!(expr.loc, "Function `{}` doesn't exist", data.name);
                }
                for arg in &data.args {
                    self.analyze_expr(arg);
                }
            },

            ExprKind::BinOp(data)  => {
                self.analyze_expr(&data.lhs);
                self.analyze_expr(&data.rhs);
            },
        }
    }
}

// TODO: the analyzer consists of elements that related to `FnDecl`
