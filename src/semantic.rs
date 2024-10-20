use crate::parser::*;

use super::{semantic_err, exit_failure};

type SymbolTable<'a> = Vec<&'a str>;
type FnDeclIdx = usize;

pub struct Analyzer<'a> {
    pub global_st: SymbolTable<'a>,
    pub local_st: SymbolTable<'a>,
    ast: &'a Ast<'a>,
}



impl<'a> Analyzer<'a> {
    pub fn new(ast: &'a Ast<'a>) -> Self {
        Self {
            global_st: SymbolTable::new(),
            local_st: SymbolTable::new(),
            ast
        }
    }

    pub fn analyze_fn_decl(&mut self, idx: FnDeclIdx) {
        let name = self.ast.fn_decls[idx].name;
        if self.global_st.contains(&self.ast.fn_decls[idx].name) {
            semantic_err!(
                self.ast.fn_decls[idx].loc,
                "Redeclaration of function `{name}`",
            );
        }

        self.global_st.push(name);

        for param in &self.ast.fn_decls[idx].params {
            self.local_st.push(param);
        }

        self.analyze_block(&self.ast.fn_decls[idx].body);
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
                    self.analyze_expr(cond.clone());
                    self.analyze_block(then);
                    self.analyze_block(elze);
                },

                StmtKind::For { cond, body } => {
                    self.analyze_expr(cond.clone());
                    self.analyze_block(body);
                },

                StmtKind::VarAssign { name, expr } => {
                    if !self.local_st.contains(&name) {
                        semantic_err!(stmt.loc, "Assignment to undeclared variable `{name}`");
                    }
                    self.analyze_expr(expr.clone());
                },
            }
        }
    }

    fn analyze_expr(&self, mut expr: ExprRange) {
        while expr.start < expr.end {
            if let Expr::Var(name) = self.ast.expr_buf[expr.start] {
                if !self.local_st.contains(&name) {
                    semantic_err!(expr.loc, "Variable `{name}` not found");
                }
            }
            expr.start += 1;
        }
    }
}

