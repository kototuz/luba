use std::collections::HashMap;

use crate::parser::*;
use super::{semantic_err, exit_failure};

//type SymbolTable<'a> = Vec<&'a str>;
//type FnDeclIdx = usize;
//
//pub struct Analyzer<'a> {
//    pub global_st: HashMap<&'a str, FnDecl>,
//    pub local_st:  HashMap<&'a str, usize>,
//    pub local_idx: usize,
//    pub local_count: usize,
//    pub param_count: usize,
//    pub has_result: bool,
//}
//
//
//pub struct FnDecl {
//    pub param_count: usize,
//    pub has_result: bool,
//    pub ip: usize,
//}
//
//impl<'a> Analyzer<'a> {
//    pub fn new() -> Self {
//        Self {
//            global_st: HashMap::new(),
//            local_st: HashMap::new(),
//            local_idx:   0,
//            param_count: 0,
//            local_count: 0,
//            has_result:  false,
//        }
//    }
//
//    pub fn analyze_fn_decl(&mut self, fn_decl: &crate::parser::FnDecl<'a>, ip: usize) {
//        if self.global_st.get(&fn_decl.name).is_some() {
//            semantic_err!(
//                fn_decl.loc,
//                "Redeclaration of function `{}`",
//                fn_decl.name
//            );
//        }
//
//        self.local_idx = if fn_decl.has_result { 1 } else { 0 };
//        self.local_st.clear();
//        self.global_st.insert(fn_decl.name, FnDecl {
//            ip, param_count: fn_decl.params.len(),
//            has_result: fn_decl.has_result
//        });
//
//        self.param_count = fn_decl.params.len();
//        for param in &fn_decl.params {
//            self.local_st.insert(param, self.local_idx);
//            self.local_idx += 1;
//        }
//
//        self.has_result = fn_decl.has_result;
//
//        self.local_idx += 2; // this slots reserved for ip and sp2
//        self.analyze_block(&fn_decl.body);
//        self.local_count = self.local_st.len() - self.param_count;
//    }
//
//    pub fn var_sp2_offset(&self, name: &'a str) -> usize {
//        *self.local_st.get(name)
//            .expect("the name is not in ast or you forget to analyze the fn_decl before")
//    }
//
//    fn analyze_block(&mut self, block: &Block<'a>) {
//        for stmt in block {
//            match &stmt.kind {
//                StmtKind::VarDecl(name) => {
//                    if self.local_st.contains_key(name) {
//                        semantic_err!(stmt.loc, "Redeclaration of variable `{name}`");
//                    }
//                    self.local_st.insert(name, self.local_idx);
//                    self.local_idx += 1;
//                },
//
//                StmtKind::If { cond, then, elze } => {
//                    self.analyze_expr(cond);
//                    self.analyze_block(then);
//                    self.analyze_block(elze);
//                },
//
//                StmtKind::For { cond, body } => {
//                    self.analyze_expr(cond);
//                    self.analyze_block(body);
//                },
//
//                StmtKind::VarAssign { name, expr } => {
//                    if !self.local_st.contains_key(name) {
//                        semantic_err!(stmt.loc, "Assignment to undeclared variable `{name}`");
//                    }
//                    self.analyze_expr(expr);
//                },
//
//                StmtKind::ReturnVal(expr) => {
//                    if !self.has_result {
//                        semantic_err!(stmt.loc, "The function doesn't return value");
//                    }
//                    self.analyze_expr(expr)
//                },
//
//                StmtKind::Return => {
//                    if self.has_result {
//                        semantic_err!(stmt.loc, "The function must return value");
//                    }
//                },
//
//                StmtKind::FnCall { name, args } => {
//                    if self.global_st.get(name).is_none() {
//                        semantic_err!(stmt.loc, "Global function `{name}` is not defined");
//                    }
//                    for arg in args {
//                        self.analyze_expr(arg);
//                    }
//                },
//            }
//        }
//    }
//
//    fn analyze_expr(&self, expr: &Expr) {
//        match &expr.kind {
//            ExprKind::Num(_)       => {},
//            ExprKind::Var(name)    => {
//                if self.local_st.get(name).is_none() {
//                    semantic_err!(expr.loc, "Variable `{name}` doesn't exist");
//                }
//            },
//
//            ExprKind::FnCall(data) => {
//                if self.global_st.get(data.name).is_none() {
//                    semantic_err!(expr.loc, "Function `{}` doesn't exist", data.name);
//                }
//                for arg in &data.args {
//                    self.analyze_expr(arg);
//                }
//            },
//
//            ExprKind::BinOp(data)  => {
//                self.analyze_expr(&data.lhs);
//                self.analyze_expr(&data.rhs);
//            },
//        }
//    }
//}

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

            local_scopes.push(this.local_scope);
            this.local_scope = LocalScope::new(); 
        }

        local_scopes
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

                StmtKind::If { cond, then } => {
                    self.analyze_expr(cond);
                    self.analyze_block(then, in_loop);
                },

                StmtKind::IfElse { cond, then, elze } => {
                    self.analyze_expr(cond);
                    self.analyze_block(then, in_loop);
                    self.analyze_block(elze, in_loop);
                },

                StmtKind::BuilinFnCall { name, arg } => {
                    if *name != "log" {
                        semantic_err!(stmt.loc, "Builtin function `{name}` doesn't exist");
                    }
                    if !self.local_scope.contains_key(arg) {
                        semantic_err!(stmt.loc, "Varible `{arg}` doesn't exist");
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

// TODO: the analyzer consists of elements that related to `FnDecl`
