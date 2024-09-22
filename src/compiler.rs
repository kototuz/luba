use std::io::prelude::*;
use std::io::Result as IOResult;
use std::fs::File;

use super::Result;
use super::parser::*;

fn write_premain(main: &mut File) -> IOResult<()> {
    let _ = writeln!(main, "scoreboard objectives add r0 dummy")?;
    let _ = writeln!(main, "scoreboard objectives add r1 dummy")?;
    Ok(())
}

fn compile_fn_decl(
    syntax: &Syntax,
    fn_file: &mut File,
    fn_decl: &FnDecl
) -> IOResult<()> {
    if fn_decl.name == "main" { let _ = write_premain(fn_file)?; }
    for stmt_i in fn_decl.body.clone() {
        match &syntax.stmts[stmt_i] {
            // TODO: refactor the expression generation (specifically that pattern `12341234123+++++---++`)
            Stmt::VarAssign { name, expr } => {
                let _ = writeln!(fn_file, "\n# assign var `{}`", name)?;
                for expr_i in expr.clone() {
                    match syntax.exprs[expr_i] {
                        Expr::Var(n) => {
                            let _ = writeln!(fn_file, "data modify storage mcs stack append from storage mcs local[-1].{n}",)?;
                        },

                        Expr::Num(n) => {
                            let _ = writeln!(fn_file, "data modify storage mcs stack append value {n}")?;
                        },

                        Expr::OpAdd  => {
                            let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                            let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs stack[-1]")?;
                            let _ = writeln!(fn_file, "scoreboard players operation accum r0 += accum r1")?;
                            let _ = writeln!(fn_file, "execute store result storage mcs stack[-2] int 1 run scoreboard players get accum r0")?;
                            let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                        },

                        Expr::OpSub  => {
                            let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                            let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs stack[-1]")?;
                            let _ = writeln!(fn_file, "scoreboard players operation accum r0 -= accum r1")?;
                            let _ = writeln!(fn_file, "execute store result storage mcs stack[-2] int 1 run scoreboard players get accum r0")?;
                            let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                        },

                        Expr::OpMul  => {
                            let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                            let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs stack[-1]")?;
                            let _ = writeln!(fn_file, "scoreboard players operation accum r0 *= accum r1")?;
                            let _ = writeln!(fn_file, "execute store result storage mcs stack[-2] int 1 run scoreboard players get accum r0")?;
                            let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                        },

                        Expr::OpDiv  => {
                            let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                            let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs stack[-1]")?;
                            let _ = writeln!(fn_file, "scoreboard players operation accum r0 /= accum r1")?;
                            let _ = writeln!(fn_file, "execute store result storage mcs stack[-2] int 1 run scoreboard players get accum r0")?;
                            let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                        },

                        Expr::FnCall(n) => {
                            let _ = writeln!(fn_file, "data modify storage mcs local append value {{}}")?;
                            let _ = writeln!(fn_file, "function test:{n}")?;
                            let _ = writeln!(fn_file, "data remove storage mcs local[-1]")?;
                            let _ = writeln!(fn_file, "data modify storage mcs stack append from storage mcs return")?;
                        },
                        _ => unreachable!()
                    }
                }

                let _ = writeln!(fn_file, "data modify storage mcs local[-1].{name} set from storage mcs stack[0]")?;
                let _ = writeln!(fn_file, "data remove storage mcs stack[0]");
            },

            Stmt::Return(expr) => {
                match expr {
                    Expr::Num(z) => {
                        let _ = writeln!(fn_file, "data modify storage mcs return set value {z}")?;
                        let _ = writeln!(fn_file, "return 1")?;
                    },

                    Expr::Var(n) => {
                        let _ = writeln!(fn_file, "data modify storage mcs return set from storage mcs local[-1].{n}")?;
                        let _ = writeln!(fn_file, "return 1")?;
                    },

                    _ => unreachable!()
                }
            },
        }
    }
    Ok(())
}

pub fn compile(syntax: &Syntax) -> Result<()> {
    let function_dir = std::env::current_dir().map_err(|err| {
        eprintln!("ERROR: could not get current directory: {err}");
    })?.join("function");
    let _ = std::fs::create_dir_all(&function_dir).map_err(|err| {
        eprintln!("ERROR: could not create function dir: {err}");
    })?;

    for fn_decl in &syntax.fns {
        let mut file = File::create(
            function_dir.join(fn_decl.name)
            .with_extension("mcfunction")).map_err(|err| {
                eprintln!("ERROR: could not create function declaration file `{}`: {err}", fn_decl.name);
            }
        )?;

        let _ = compile_fn_decl(syntax, &mut file, fn_decl).map_err(|err| {
            eprintln!("ERROR: compilation failed: {err}");
        })?;
    }

    Ok(())
}
