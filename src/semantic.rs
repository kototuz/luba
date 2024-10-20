use parser::*;

use super::{semantic_err, exit_failure};

pub struct SymbolTable<'a>(pub Vec<&'a str>);



impl<'a> SymbolTable<'a> {
    pub fn var_sp2_offset(&self, name: &'a str) -> usize {
        for (i, entry) in self.0.iter().enumerate() {
            if *entry == name {
                return i;
            }
        }
        panic!("name `{name}` is not from ast");
    }
}

pub fn analyze<'a>(ast: &Ast<'a>) -> SymbolTable<'a> {
    let mut st = SymbolTable(Vec::new());
    analyze_block(&mut st, ast, &ast.stmts);
    st
}

fn analyze_block<'a>(st: &mut SymbolTable<'a>, ast: &Ast, block: &Block<'a>) {
    for stmt in block {
        match &stmt.kind {
            StmtKind::VarDecl(name) => {
                if st.0.contains(&name) {
                    semantic_err!(stmt.loc, "Redeclaration of variable `{name}`");
                }
                st.0.push(name);
            },

            StmtKind::If { cond, then, elze } => {
                analyze_expr(st, ast, cond.clone());
                analyze_block(st, ast, then);
                analyze_block(st, ast, elze);
            },

            StmtKind::For { cond, body } => {
                analyze_expr(st, ast, cond.clone());
                analyze_block(st, ast, body);
            },

            StmtKind::VarAssign { name, expr } => {
                if !st.0.contains(&name) {
                    semantic_err!(stmt.loc, "Assignment to undeclared variable `{name}`");
                }
                analyze_expr(st, ast, expr.clone());
            },
        }
    }
}

fn analyze_expr(st: &mut SymbolTable, ast: &Ast, mut expr: ExprRange) {
    while expr.start < expr.end {
        if let Expr::Var(name) = ast.expr_buf[expr.start] {
            if !st.0.contains(&name) {
                semantic_err!(expr.loc, "Variable `{name}` not found");
            }
        }
        expr.start += 1;
    }
}
