use parser::*;

use super::{semantic_err, exit_failure};

pub struct SymbolTable<'a>(pub Vec<&'a str>);

struct Analyzer<'a> {
    st: SymbolTable<'a>,
    ast: &'a Ast<'a>,
}



impl<'a> Analyzer<'a> {
    fn analyze_block(&mut self, block: &Block<'a>) {
        for stmt in block {
            match &stmt.kind {
                StmtKind::VarDecl(name) => {
                    if self.st.0.contains(&name) {
                        semantic_err!(stmt.loc, "Redeclaration of variable `{name}`");
                    }
                    self.st.0.push(name);
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
                    if !self.st.0.contains(&name) {
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
                if !self.st.0.contains(&name) {
                    semantic_err!(expr.loc, "Variable `{name}` not found");
                }
            }
            expr.start += 1;
        }
    }
}

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



pub fn analyze<'a>(ast: &'a Ast<'a>) -> SymbolTable<'a> {
    let mut analyzer = Analyzer {
        st: SymbolTable(Vec::new()), ast
    };

    analyzer.analyze_block(&ast.stmts);

    analyzer.st
}
