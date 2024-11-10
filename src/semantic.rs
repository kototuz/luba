use std::collections::HashMap;

use crate::parser::*;
use super::{semantic_err, exit_failure};

pub type VarName<'a>     = &'a str;
pub type FnName<'a>      = &'a str;
pub type SP2             = usize;
pub type LocalScope<'a>  = HashMap<VarName<'a>, SP2>;
pub type GlobalScope<'a> = HashMap<FnName<'a>, &'a FnDecl<'a>>;

pub struct Analyzer<'a> {
    global_scope: GlobalScope<'a>,
    local_scope:  LocalScope<'a>,
    curr_fn_decl: &'a FnDecl<'a>,
}

impl<'a> Analyzer<'a> {
    const LOCAL_STACK_OFFSET: usize = 2;

    pub fn analyze(ast: &'a Ast<'a>) -> Vec<LocalScope<'a>> {
        let mut local_scopes: Vec<LocalScope> =
            Vec::with_capacity(ast.fn_decls.len());

        if ast.fn_decls.is_empty() {
            return local_scopes;
        }

        let mut this = Self {
            global_scope: GlobalScope::with_capacity(ast.fn_decls.len()),
            local_scope:  LocalScope::new(),
            curr_fn_decl: &ast.fn_decls[0],
        };

        for fn_decl in &ast.fn_decls {
            if this.global_scope.contains_key(fn_decl.name) {
                semantic_err!(
                    fn_decl.loc, "Redeclaration of function `{}`",
                    fn_decl.name
                );
            }

            this.global_scope.insert(fn_decl.name, &fn_decl);

            for i in 0..fn_decl.params.len() {
                this.local_scope.insert(fn_decl.params[i], i+1);
            }

            this.curr_fn_decl = &fn_decl;
            this.analyze_block(&fn_decl.body, false);

            if fn_decl.has_result {
                this.check_return_value(&fn_decl.body);
            }

            local_scopes.push(this.local_scope);
            this.local_scope = LocalScope::new(); 
        }

        local_scopes
    }

    fn check_return_value(&mut self, block: &Block<'a>) {
        let stmt = block.last().unwrap();
        if !matches!(stmt.kind, StmtKind::ReturnVal(_)) {
            semantic_err!(stmt.loc, "Return value is missed");
        }
    }

    fn analyze_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Num(_) => {},
            ExprKind::Var(name) => {
                if !self.local_scope.contains_key(name) {
                    semantic_err!(expr.loc, "Variable `{name}` doesn't exist");
                }
            },

            ExprKind::FnCall(data) => {
                if let Some(fn_decl) = self.global_scope.get(data.name) {
                    if !fn_decl.has_result {
                        semantic_err!(expr.loc, "Function `{}` doesn't return value", data.name);
                    }
                } else {
                    semantic_err!(expr.loc, "Function `{}` doesn't exist", data.name);
                }
            },

            ExprKind::BinOp(data) => {
                self.analyze_expr(&data.lhs);
                self.analyze_expr(&data.rhs);
            },
        }
    }

    fn analyze_block(&mut self, block: &Block<'a>, in_loop: bool) {
        for stmt in block {
            match &stmt.kind {
                StmtKind::VarDecl(name) => {
                    if self.local_scope.contains_key(name) {
                        semantic_err!(stmt.loc, "Redeclaration of variable `{name}`");
                    }
                    self.local_scope.insert(
                        name, Self::LOCAL_STACK_OFFSET+self.local_scope.len()
                    );
                },

                StmtKind::VarAssign { name, expr } => {
                    if !self.local_scope.contains_key(name) {
                        semantic_err!(stmt.loc, "Variable `{name}` doesn't exist");
                    }
                    self.analyze_expr(expr);
                },

                StmtKind::FnCall { name, args } => {
                    if let Some(fn_decl) = self.global_scope.get(name) {
                        if args.len() != fn_decl.params.len() {
                            semantic_err!(
                                stmt.loc, "Function `{}` accepts only {} parameters",
                                name, fn_decl.params.len()
                            );
                        }
                    }

                    for arg in args {
                        self.analyze_expr(arg);
                    }
                },

                StmtKind::If { cond, then, elzeifs, elze } => {
                    self.analyze_expr(cond);
                    self.analyze_block(then, in_loop);
                    for elzeif in elzeifs {
                        self.analyze_expr(&elzeif.cond);
                        self.analyze_block(&elzeif.then, in_loop);
                    }
                    self.analyze_block(elze, in_loop);
                },

                StmtKind::BuilinFnCall { name, arg } => {
                    match *name {
                        "cmd" => {},
                        "log" => {
                            if !self.local_scope.contains_key(arg) {
                                semantic_err!(stmt.loc, "Varible `{arg}` doesn't exist");
                            }
                        },

                        _ => {
                            semantic_err!(stmt.loc, "Builtin function `{name}` doesn't exist");
                        }
                    }
                },

                StmtKind::For { body }  => {
                    self.analyze_block(body, true);
                },

                StmtKind::Break => {
                    if !in_loop {
                        semantic_err!(stmt.loc, "`break` is not in a loop");
                    }
                },

                StmtKind::Continue => {
                    if !in_loop {
                        semantic_err!(stmt.loc, "`continue` is not in a loop");
                    }
                },

                StmtKind::Return => {
                    if self.curr_fn_decl.has_result {
                        semantic_err!(
                            stmt.loc, "Function `{}` must return value",
                            self.curr_fn_decl.name
                        );
                    }
                },

                StmtKind::ReturnVal(_) => {
                    if !self.curr_fn_decl.has_result {
                        semantic_err!(
                            stmt.loc, "Function `{}` mustn't return value",
                            self.curr_fn_decl.name
                        );
                    }
                },
            }
        }
    }
}
