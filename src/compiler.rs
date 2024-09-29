use std::io::prelude::*;
use std::io::Result as IOResult;
use std::fs::File;

use super::Result;
use super::parser::*;

fn write_premain(main: &mut File) -> IOResult<()> {
    let _ = writeln!(main, "# premain")?;
    let _ = writeln!(main, "scoreboard objectives add r0 dummy")?;
    let _ = writeln!(main, "scoreboard objectives add r1 dummy")?;
    let _ = writeln!(main, "data modify storage mcs local append value {{}}")?;
    Ok(())
}

fn write_postmain(main: &mut File) -> IOResult<()> {
    let _ = writeln!(main, "\n# postmain")?;
    let _ = writeln!(main, "data remove storage mcs local")?;
    let _ = writeln!(main, "data remove storage mcs stack")?;
    let _ = writeln!(main, "data remove storage mcs return")?;
    Ok(())
}

fn compile_expr(
    fn_file: &mut File,
    exprs: &[Expr],
    to_var: &str,
) -> IOResult<()> {
    let mut i = 0;
    let mut local_scope_offset = 1;
    while i < exprs.len() {
        if i == exprs.len()-1 {
            match exprs[i] {
                Expr::Var(name) => {
                    let _ = writeln!(fn_file, "data modify storage {to_var} set from storage mcs local[-1].{name}")?;
                },

                Expr::Num(num) => {
                    let _ = writeln!(fn_file, "data modify storage {to_var} set value {num}")?;
                },

                Expr::OpAdd => {
                    let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                    let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs stack[-1]")?;
                    let _ = writeln!(fn_file, "execute store result storage mcs {to_var} int 1 run scoreboard players operation accum r0 += accum r1")?;
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                }

                Expr::OpSub  => {
                    let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                    let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs stack[-1]")?;
                    let _ = writeln!(fn_file, "execute store result storage mcs {to_var} int 1 run scoreboard players operation accum r0 -= accum r1")?;
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                },

                Expr::OpMul  => {
                    let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                    let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs stack[-1]")?;
                    let _ = writeln!(fn_file, "execute store result storage mcs {to_var} int 1 run scoreboard players operation accum r0 *= accum r1")?;
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                },

                Expr::OpDiv  => {
                    let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                    let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs stack[-1]")?;
                    let _ = writeln!(fn_file, "execute store result storage mcs {to_var} int 1 run scoreboard players operation accum r0 /= accum r1")?;
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                },

                Expr::FnCall(name) => {
                    if !matches!(exprs[i-1], Expr::SetArg(_)) {
                        let _ = writeln!(fn_file, "data modify storage mcs local append value {{}}")?;
                    }
                    let _ = writeln!(fn_file, "function test:{name}")?;
                    let _ = writeln!(fn_file, "data remove storage mcs local[-1]")?;
                    let _ = writeln!(fn_file, "data modify storage mcs {to_var} set from storage mcs return")?;
                },

                _ => unreachable!()
            }
        } else if let Expr::SetArg(idx) = exprs[i+1] {
            if let Expr::FnCall(name) = exprs[i] {
                if !matches!(exprs[i-1], Expr::SetArg(_)) {
                    let _ = writeln!(fn_file, "data modify storage mcs local append value {{}}")?;
                } else { local_scope_offset -= 1; }
                let _ = writeln!(fn_file, "function test:{name}")?;
                let _ = writeln!(fn_file, "data remove storage mcs local[-1]")?;
                if idx == 0 {
                    let _ = writeln!(fn_file, "data modify storage mcs local append value {{}}")?;
                    local_scope_offset += 1;
                }
                let _ = writeln!(fn_file, "data modify storage mcs local[-1].{idx} set from storage mcs return")?;
            } else {
                if idx == 0 {
                    let _ = writeln!(fn_file, "data modify storage mcs local append value {{}}")?;
                    local_scope_offset += 1;
                }

                match exprs[i] {
                    Expr::Var(name) => {
                        let _ = writeln!(fn_file, "data modify storage mcs local[-1].{idx} set from storage mcs local[-{local_scope_offset}].{name}")?;
                    },

                    Expr::Num(num) => {
                        let _ = writeln!(fn_file, "data modify storage mcs local[-1].{idx} set value {num}")?;
                    },

                    Expr::OpAdd => {
                        let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                        let _ = writeln!(fn_file, "execute store result storage mcs local[-1].{idx} int 1 run scoreboard players operation accum r0 += accum r1")?;
                        let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                        let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                    },

                    Expr::OpSub => {
                        let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                        let _ = writeln!(fn_file, "execute store result storage mcs local[-1].{idx} int 1 run scoreboard players operation accum r0 -= accum r1")?;
                        let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                        let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                    },

                    Expr::OpMul => {
                        let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                        let _ = writeln!(fn_file, "execute store result storage mcs local[-1].{idx} int 1 run scoreboard players operation accum r0 *= accum r1")?;
                        let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                        let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                    },

                    Expr::OpDiv => {
                        let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                        let _ = writeln!(fn_file, "execute store result storage mcs local[-1].{idx} int 1 run scoreboard players operation accum r0 /= accum r1")?;
                        let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                        let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                    },

                    Expr::FnCall(name) => {
                        if !matches!(exprs[i-1], Expr::SetArg(_)) {
                            let _ = writeln!(fn_file, "data modify storage mcs local append value {{}}")?;
                        } else { local_scope_offset -= 1; }
                        let _ = writeln!(fn_file, "function test:{name}")?;
                        let _ = writeln!(fn_file, "data remove storage mcs local[-1]")?;
                        let _ = writeln!(fn_file, "data modify storage mcs stack append from storage mcs return")?;
                    },

                    _ => unreachable!()
                }
            }
            i += 1;
        } else {
            match exprs[i] {
                Expr::Var(name) => {
                    let _ = writeln!(fn_file, "data modify storage mcs stack append from storage mcs local[-{local_scope_offset}].{name}")?;
                },

                Expr::Num(n) => {
                    let _ = writeln!(fn_file, "data modify storage mcs stack append value {n}")?;
                },

                Expr::OpAdd  => {
                    let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                    let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs stack[-1]")?;
                    let _ = writeln!(fn_file, "execute store result storage mcs stack[-2] int 1 run scoreboard players operation accum r0 += accum r1")?;
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                },

                Expr::OpSub  => {
                    let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                    let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs stack[-1]")?;
                    let _ = writeln!(fn_file, "execute store result storage mcs stack[-2] int 1 run scoreboard players operation accum r0 -= accum r1")?;
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                },

                Expr::OpMul  => {
                    let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                    let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs stack[-1]")?;
                    let _ = writeln!(fn_file, "execute store result storage mcs stack[-2] int 1 run scoreboard players operation accum r0 *= accum r1")?;
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                },

                Expr::OpDiv  => {
                    let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                    let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs stack[-1]")?;
                    let _ = writeln!(fn_file, "execute store result storage mcs stack[-2] int 1 run scoreboard players operation accum r0 /= accum r1")?;
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                },

                _ => unreachable!()
            }
        }
        i += 1;
    }

    Ok(())
}

fn compile_fn_decl(
    syntax: &Program,
    fn_file: &mut File,
    fn_decl: &FnDecl
) -> IOResult<()> {
    if fn_decl.name == "main" { let _ = write_premain(fn_file)?; }

    if fn_decl.params.len() > 0 { let _ = writeln!(fn_file, "# parameters")?; }
    for (i, param) in fn_decl.params.iter().enumerate() {
        let _ = writeln!(fn_file, "data modify storage mcs local[-1].{param} set from storage mcs local[-1].{i}")?;
    }

    for stmt_i in fn_decl.body.clone() {
        match &syntax.stmts[stmt_i] {
            // TODO: refactor the expression generation (specifically that pattern `12341234123+++++---++`)
            Stmt::VarAssign { name, expr } => {
                let _ = writeln!(fn_file, "\n# assign var `{}`", name)?;
                let _ = compile_expr(fn_file, &syntax.exprs[expr.clone()], format!("local[-1].{name}").as_str())?;
            },

            Stmt::Return(expr_range) => {
                let _ = writeln!(fn_file, "\n# return")?;
                let _ = compile_expr(fn_file, &syntax.exprs[expr_range.clone()], "return")?;
                let _ = writeln!(fn_file, "return 1")?;
            },
        }
    }

    if fn_decl.name == "main" { let _ = write_postmain(fn_file)?; }

    Ok(())
}

pub fn compile(syntax: &Program) -> Result<()> {
    let function_dir = std::env::current_dir().map_err(|err| {
        eprintln!("ERROR: could not get current directory: {err}");
    })?.join("function");

    if std::path::Path::exists(&function_dir) {
        let _ = std::fs::remove_dir_all(&function_dir).map_err(|err| {
            eprintln!("ERROR: could not clean `function` directory: {err}");
        })?;
    } 

    let _ = std::fs::create_dir_all(&function_dir).map_err(|err| {
        eprintln!("ERROR: could not create function dir: {err}");
    })?;

    for fn_decl in &syntax.fns {
        let mut file =
            File::create(function_dir.join(fn_decl.name)
                .with_extension("mcfunction"))
                .map_err(|err| {
                    eprintln!("ERROR: could not create function declaration file `{}`: {err}", fn_decl.name);
                }
            )?;

        let _ = compile_fn_decl(syntax, &mut file, fn_decl).map_err(|err| {
            eprintln!("ERROR: compilation failed: {err}");
        })?;
    }

    Ok(())
}
