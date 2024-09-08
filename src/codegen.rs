use crate::parser;
use crate::dp;

pub fn gen_dp<'a>(
    dp_name: &'a str,
    exprs: &Vec<parser::Expr>,
    stmts: &Vec<parser::Stmt>
) {
    for stmt in stmts {
        match stmt {
            parser::Stmt::VarAssign { name, expr } => {
                println!("var `{name}`");
                for x in expr.clone() {
                    println!("{:?}", exprs[x]);
                }
                println!("");
            }
        }
    }
    //use crate::parser;
    //use std::io::prelude::*;
    //
    //let dp = Datapack::create(dp_name);
    //let mut fn_file = dp.create_mcfn_file("test");
    //let parser::Stmt::VarAssign { name, expr } = &stmts[0];
    //let r = expr.clone();
    //for x in r {
    //    println!("{:?}", exprs[x]);
    //    fn_file.write_all(b"scoreboard objectives add v0 dummy").expect("Could not write");
    //    fn_file.write_all(b"");
    //}
}
