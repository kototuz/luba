use std::collections::HashMap;

use crate::parser::*;
use super::{semantic_err, exit_failure};

pub type Name<'a> = &'a str;
pub type SP2      = usize;
pub type ScopeIdx = usize;

#[derive(Default, Debug)]
pub struct Scope<'a> {
    pub items:  HashMap<Name<'a>, Type>,
    pub parent: ScopeIdx
}

#[derive(Debug)]
pub struct FnDeclInfo {
    pub has_result:  bool,
    pub param_count: usize,
    pub local_count: usize,
}

#[derive(Debug)]
pub enum Type {
    Var(SP2),
    FnDecl(FnDeclInfo),
}

pub struct Analyzer<'a> {
    scopes: Vec<Scope<'a>>,
    sp2:    SP2   
}

#[derive(Default)]
struct Flags {
    in_loop:    bool,
    has_result: bool,
}

impl<'a> Analyzer<'a> {
    pub fn analyze(ast: &'a Ast<'a>) -> Vec<Scope<'a>> {
        let mut analyzer = Self {
            scopes: Vec::new(),
            sp2:    0,
        };

        analyzer.analyze_block(&ast.stmts, 0, &Flags::default());

        analyzer.scopes
    }

    fn name_exists_global(&self, name: Name<'a>, scope: ScopeIdx) -> bool {
        if self.scopes[scope].items.contains_key(name) {
            true
        } else if scope != 0 {
            self.name_exists_global(name, self.scopes[scope].parent)
        } else {
            false
        }
    }

    fn get_type_global(&self, name: Name<'a>, scope: ScopeIdx) -> Option<&Type> {
        self.scopes[scope].items.get(name).or_else(|| {
            if scope == 0 { return None; }
            self.get_type_global(name, self.scopes[scope].parent)
        })
    }

    fn check_return_value(&mut self, block: &Block<'a>) {
        let stmt = block.last().unwrap();
        if !matches!(stmt.kind, StmtKind::ReturnVal(_)) {
            semantic_err!(stmt.loc, "Return value is missed");
        }
    }

    fn analyze_expr(&mut self, expr: &Expr, scope: ScopeIdx) {
        match &expr.kind {
            ExprKind::Num(_) => {},
            ExprKind::Var(name) => {
                if !matches!(self.get_type_global(name, scope), Some(Type::Var(_))){
                    semantic_err!(expr.loc, "Variable `{name}` is not found");
                }
            },

            ExprKind::FnCall(data) => {
                if let Some(Type::FnDecl(fn_decl)) = self.get_type_global(data.name, scope){
                    if !fn_decl.has_result {
                        semantic_err!(expr.loc, "Function `{}` doesn't return value", data.name);
                    }
                    if data.args.len() != fn_decl.param_count {
                        semantic_err!(expr.loc, "Function `{}`'s arguments are incorrect", data.name);
                    }
                } else {
                    semantic_err!(expr.loc, "Function `{}` is not found", data.name);
                }
            },

            ExprKind::BinOp(data) => {
                self.analyze_expr(&data.lhs, scope);
                self.analyze_expr(&data.rhs, scope);
            },
        }
    }

    fn analyze_stmt(&mut self, stmt: &'a Stmt<'a>, scope_idx: ScopeIdx, flags: &Flags) {
        match &stmt.kind {
            StmtKind::FnDecl(data) => {
                if self.scopes[scope_idx].items.contains_key(data.name) {
                    semantic_err!(stmt.loc, "Redeclaration of function `{}`", data.name);
                }

                let mut new_scope = Scope {
                    items: HashMap::with_capacity(data.params.len()),
                    parent: scope_idx
                };

                self.sp2 = if data.has_result { 1 } else { 0 };
                for p in &data.params {
                    new_scope.items.insert(p, Type::Var(self.sp2));
                    self.sp2 += 1;
                }
                self.sp2 += 2;

                let scope = self.scopes.len();
                self.scopes.push(new_scope);
                let flags = Flags {
                    in_loop:    false,
                    has_result: data.has_result
                };

                let mut local_count = self.sp2;
                for stmt in &data.body {
                    self.analyze_stmt(stmt, scope, &flags);
                }
                local_count = self.sp2 - local_count;

                self.scopes[scope_idx].items.insert(data.name, Type::FnDecl(FnDeclInfo {
                    has_result:  data.has_result,
                    param_count: data.params.len(),
                    local_count
                }));
            },

            StmtKind::VarDecl(name) => {
                if self.scopes[scope_idx].items.contains_key(name) {
                    semantic_err!(stmt.loc, "Redeclaration of variable `{name}`");
                }
                self.scopes[scope_idx].items.insert(name, Type::Var(self.sp2));
                self.sp2 += 1;
            },

            StmtKind::VarDeclAssign { name, expr } => {
                if self.scopes[scope_idx].items.contains_key(name) {
                    semantic_err!(stmt.loc, "Redeclaration of variable `{name}`");
                }
                self.analyze_expr(expr, scope_idx);
                self.scopes[scope_idx].items.insert(name, Type::Var(self.sp2));
                self.sp2 += 1;
            },

            StmtKind::VarAssign { name, expr } => {
                if !matches!(self.get_type_global(name, scope_idx), Some(Type::Var(_))) {
                    semantic_err!(stmt.loc, "Variable `{name}` is not found");
                }
                self.analyze_expr(expr, scope_idx);
            },

            StmtKind::FnCall { name, args } => {
                if let Some(Type::FnDecl(data)) = self.get_type_global(name, scope_idx){
                    if args.len() != data.param_count {
                        semantic_err!(
                            stmt.loc, "Function `{}` accepts only {} parameters",
                            name, data.param_count
                        );
                    }
                } else {
                    semantic_err!(stmt.loc, "Function `{name}` is not found");
                }

                for arg in args {
                    self.analyze_expr(arg, scope_idx);
                }
            },

            StmtKind::If { cond, then, elzeifs, elze } => {
                self.analyze_expr(cond, scope_idx);
                self.analyze_block(then, scope_idx, flags);
                for elzeif in elzeifs {
                    self.analyze_expr(&elzeif.cond, scope_idx);
                    self.analyze_block(&elzeif.then, scope_idx, flags);
                }
                self.analyze_block(elze, scope_idx, flags);
            },

            StmtKind::BuilinFnCall { name, arg } => {
                match *name {
                    "cmd" => {},
                    "log" => {
                        if !matches!(self.get_type_global(arg, scope_idx), Some(Type::Var(_))) {
                            semantic_err!(stmt.loc, "Varible `{arg}` is not found");
                        }
                    },

                    _ => {
                        semantic_err!(stmt.loc, "Builtin function `{name}` doesn't exist");
                    }
                }
            },

            StmtKind::For { body, init, cond, post }  => {
                // TODO: init statement must be in the for scope
                if let Some(s) = init { self.analyze_stmt(s, scope_idx, flags); }
                if let Some(e) = cond { self.analyze_expr(e, scope_idx); }
                if let Some(s) = post { self.analyze_stmt(s, scope_idx, flags); }
                self.analyze_block(body, scope_idx, &Flags {
                    in_loop: true,
                    has_result: flags.has_result
                });
            },

            StmtKind::Break => {
                if !flags.in_loop {
                    semantic_err!(stmt.loc, "`break` is not in a loop");
                }
            },

            StmtKind::Continue => {
                if !flags.in_loop {
                    semantic_err!(stmt.loc, "`continue` is not in a loop");
                }
            },

            StmtKind::Return => {
                if flags.has_result {
                    semantic_err!(stmt.loc, "The function must return value");
                }
            },

            StmtKind::ReturnVal(expr) => {
                if !flags.has_result {
                    semantic_err!(stmt.loc, "The function mustn't return value");
                }
                self.analyze_expr(expr, scope_idx);
            },
        }
    }

    fn analyze_block(&mut self, block: &'a Block<'a>, scope: ScopeIdx, flags: &Flags) {
        self.scopes.push(Scope { items: HashMap::new(), parent: scope });
        let curr_scope = self.scopes.len()-1;
        for stmt in block {
            self.analyze_stmt(stmt, curr_scope, flags);
        }
    }
}
