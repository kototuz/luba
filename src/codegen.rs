use crate::parser;
use super::Result;

use std::io::prelude::*;
use std::process::exit;
use std::path::Path;
use std::fs::File;

fn create_mcfn_file(dir: &Path, name: &str) -> File {
    File::create(dir.join(name).with_extension("mcfunction")).unwrap_or_else(|err| {
        eprintln!("ERROR: could not create a `{name}.mcfunction`: {err}");
        exit(1);
    })
}

fn write_err(err: std::io::Error) {
    eprintln!("ERROR: could not write file: {err}");
}

fn gen_premain(main: &mut File) -> Result<()> {
    let _ = writeln!(main, "scoreboard objectives add r0 dummy").map_err(write_err)?;
    let _ = writeln!(main, "scoreboard objectives add r1 dummy").map_err(write_err)?;
    Ok(())
}

pub fn gen_code(
    output_dir_path: &Path,
    syntax: &parser::Syntax
) -> Result<()> {
    let function_dir = output_dir_path.join("function");
    let _ = std::fs::create_dir_all(&function_dir)
        .inspect_err(|err| {
            eprintln!("ERROR: could not create the `function` dir: {err}");
            exit(1);
        });

    let mut curr_fn: File;
    for func in &syntax.fns {
        curr_fn = create_mcfn_file(&function_dir, func.name);
        if func.name == "main" { let _ = gen_premain(&mut curr_fn)?; }
        for stmt_i in func.body.clone() {
            match &syntax.stmts[stmt_i] {
                // TODO: refactor the expression generation (specifically that pattern `12341234123+++++---++`)
                parser::Stmt::VarAssign { name, expr } => {
                    let _ = writeln!(&mut curr_fn, "\n# assign var `{}`", name).map_err(write_err)?;
                    for expr_i in expr.clone() {
                        match syntax.exprs[expr_i] {
                            parser::Expr::Var(n) => {
                                let _ = writeln!(&mut curr_fn, "data modify storage mcs stack append from storage mcs local[-1].{n}",).map_err(write_err)?;
                            },

                            parser::Expr::Num(n) => {
                                let _ = writeln!(&mut curr_fn, "data modify storage mcs stack append value {n}").map_err(write_err)?;
                            },

                            parser::Expr::OpAdd  => {
                                let _ = writeln!(&mut curr_fn, "execute store result score accum r0 run data get storage mcs stack[-2]").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "execute store result score accum r1 run data get storage mcs stack[-1]").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "scoreboard players operation accum r0 += accum r1").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "execute store result storage mcs stack[-2] int 1 run scoreboard players get accum r0").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "data remove storage mcs stack[-1]").map_err(write_err)?;
                            },

                            parser::Expr::OpSub  => {
                                let _ = writeln!(&mut curr_fn, "execute store result score accum r0 run data get storage mcs stack[-2]").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "execute store result score accum r1 run data get storage mcs stack[-1]").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "scoreboard players operation accum r0 -= accum r1").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "execute store result storage mcs stack[-2] int 1 run scoreboard players get accum r0").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "data remove storage mcs stack[-1]").map_err(write_err)?;
                            },

                            parser::Expr::OpMul  => {
                                let _ = writeln!(&mut curr_fn, "execute store result score accum r0 run data get storage mcs stack[-2]").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "execute store result score accum r1 run data get storage mcs stack[-1]").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "scoreboard players operation accum r0 *= accum r1").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "execute store result storage mcs stack[-2] int 1 run scoreboard players get accum r0").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "data remove storage mcs stack[-1]").map_err(write_err)?;
                            },

                            parser::Expr::OpDiv  => {
                                let _ = writeln!(&mut curr_fn, "execute store result score accum r0 run data get storage mcs stack[-2]").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "execute store result score accum r1 run data get storage mcs stack[-1]").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "scoreboard players operation accum r0 /= accum r1").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "execute store result storage mcs stack[-2] int 1 run scoreboard players get accum r0").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "data remove storage mcs stack[-1]").map_err(write_err)?;
                            },

                            parser::Expr::FnCall(n) => {
                                let _ = writeln!(&mut curr_fn, "data modify storage mcs local append value {{}}").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "function test:{n}").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "data remove storage mcs local[-1]").map_err(write_err)?;
                                let _ = writeln!(&mut curr_fn, "data modify storage mcs stack append from storage mcs return").map_err(write_err)?;
                            },

                            _ => unreachable!()
                        }
                    }

                    let _ = writeln!(&mut curr_fn, "execute store result storage mcs local[-1].{name} int 1 run data get storage mcs stack[0]").map_err(write_err)?;
                    let _ = writeln!(&mut curr_fn, "data remove storage mcs stack[0]");
                },

                parser::Stmt::Return(expr) => {
                    match expr {
                        parser::Expr::Num(z) => {
                            let _ = writeln!(&mut curr_fn, "data modify storage mcs return set value {z}").map_err(write_err)?;
                            let _ = writeln!(&mut curr_fn, "return 1").map_err(write_err)?;
                        },

                        parser::Expr::Var(n) => {
                            let _ = writeln!(&mut curr_fn, "data modify storage mcs return set from storage mcs local[-1].{n}").map_err(write_err)?;
                            let _ = writeln!(&mut curr_fn, "return 1").map_err(write_err)?;
                        },

                        _ => unreachable!()
                    }
                },

                _ => todo!()
            }
        }
    }

    Ok(())
}
