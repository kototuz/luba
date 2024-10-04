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

enum WriteTarget<'a> {
    R0,
    R1,
    Var(&'a str),
    None
}

fn compile_expr(
    fn_file: &mut File,
    exprs: &[Expr],
    write_target: WriteTarget
) -> IOResult<()> {
    let mut i = 0;
    let mut local_scope_offset = 1;
    while i < exprs.len() {
        if i == exprs.len()-1 {
            match exprs[i] {
                Expr::Var(from) => {
                    match write_target {
                        WriteTarget::Var(to) => {
                            let _ = writeln!(fn_file, "data modify storage mcs {to} set from storage mcs local[-1].{from}")?;
                        },
                        WriteTarget::R0 => {
                            let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs local[-1].{from}")?;
                        },
                        WriteTarget::R1 => {
                            let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs local[-1].{from}")?;
                        },
                        WriteTarget::None => {}
                    }
                },

                Expr::Num(num) => {
                    match write_target {
                        WriteTarget::Var(to) => {
                            let _ = writeln!(fn_file, "data modify storage mcs {to} set value {num}")?;
                        },
                        WriteTarget::R0 => {
                            let _ = writeln!(fn_file, "scoreboard players set accum r0 {num}")?;
                        },
                        WriteTarget::R1 => {
                            let _ = writeln!(fn_file, "scoreboard players set accum r1 {num}")?;
                        },
                        WriteTarget::None => {}
                    }
                },

                Expr::OpAdd => {
                    let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                    let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs stack[-1]")?;
                    match write_target {
                        WriteTarget::Var(to) => {
                            let _ = writeln!(fn_file, "execute store result storage mcs {to} int 1 run scoreboard players operation accum r0 += accum r1")?;
                        },
                        WriteTarget::R0 => {
                            let _ = writeln!(fn_file, "scoreboard players operation accum r0 += accum r1")?;
                        },
                        WriteTarget::R1 => {
                            let _ = writeln!(fn_file, "scoreboard players operation accum r1 += accum r0")?;
                        },
                        WriteTarget::None => {}
                    }
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                }

                Expr::OpSub  => {
                    let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                    let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs stack[-1]")?;
                    match write_target {
                        WriteTarget::Var(to) => {
                            let _ = writeln!(fn_file, "execute store result storage mcs {to} int 1 run scoreboard players operation accum r0 -= accum r1")?;
                        },
                        WriteTarget::R0 => {
                            let _ = writeln!(fn_file, "scoreboard players operation accum r0 -= accum r1")?;
                        },
                        WriteTarget::R1 => {
                            let _ = writeln!(fn_file, "execute store result score accum r1 run scoreboard players operation accum r0 -= accum r1")?;
                        },
                        WriteTarget::None => {}
                    }
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                },

                Expr::OpMul  => {
                    let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                    let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs stack[-1]")?;
                    match write_target {
                        WriteTarget::Var(to) => {
                            let _ = writeln!(fn_file, "execute store result storage mcs {to} int 1 run scoreboard players operation accum r0 *= accum r1")?;
                        },
                        WriteTarget::R0 => {
                            let _ = writeln!(fn_file, "scoreboard players operation accum r0 *= accum r1")?;
                        },
                        WriteTarget::R1 => {
                            let _ = writeln!(fn_file, "execute store result score accum r1 run scoreboard players operation accum r0 *= accum r1")?;
                        },
                        WriteTarget::None => {}
                    }
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                },

                Expr::OpDiv  => {
                    let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs stack[-2]")?;
                    let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs stack[-1]")?;
                    match write_target {
                        WriteTarget::Var(to) => {
                            let _ = writeln!(fn_file, "execute store result storage mcs {to} int 1 run scoreboard players operation accum r0 /= accum r1")?;
                        },
                        WriteTarget::R0 => {
                            let _ = writeln!(fn_file, "scoreboard players operation accum r0 /= accum r1")?;
                        },
                        WriteTarget::R1 => {
                            let _ = writeln!(fn_file, "execute store result score accum r1 run scoreboard players operation accum r0 /= accum r1")?;
                        },
                        WriteTarget::None => {}
                    }
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                    let _ = writeln!(fn_file, "data remove storage mcs stack[-1]")?;
                },

                Expr::FnCall(name) => {
                    if !matches!(exprs[i-1], Expr::SetArg(_)) {
                        let _ = writeln!(fn_file, "data modify storage mcs local append value {{}}")?;
                    }
                    let _ = writeln!(fn_file, "function test:{name}")?;
                    let _ = writeln!(fn_file, "data remove storage mcs local[-1]")?;
                    match write_target {
                        WriteTarget::Var(to) => {
                            let _ = writeln!(fn_file, "data modify storage mcs {to} set from storage mcs return")?;
                        },
                        WriteTarget::R0 => {
                            let _ = writeln!(fn_file, "execute store result score accum r0 run data get storage mcs return")?;
                        },
                        WriteTarget::R1 => {
                            let _ = writeln!(fn_file, "execute store result score accum r1 run data get storage mcs return")?;
                        },
                        WriteTarget::None => {}
                    }
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

fn compile_block(
    program: &Program,
    fn_decl: &FnDecl,
    fn_file: &mut File,
    block_idx: usize,
) -> IOResult<()> {
    let mut range = program.blocks[fn_decl.blocks.start+block_idx].range.clone();
    while range.start != range.end {
        match &program.stmts[range.start] {
            // TODO: refactor the expression generation (specifically that pattern `12341234123+++++---++`)
            Stmt::VarAssign { name, expr } => {
                let _ = writeln!(fn_file, "\n# assign var `{}`", name)?;
                let name = format!("local[-1].{}", name);
                let _ = compile_expr(fn_file, &program.exprs[expr.clone()], WriteTarget::Var(name.as_str()))?;
            },

            Stmt::Return(expr_range) => {
                let _ = writeln!(fn_file, "\n# return")?;
                let _ = compile_expr(fn_file, &program.exprs[expr_range.clone()], WriteTarget::Var("return"))?;
                let _ = writeln!(fn_file, "return 1")?;
            },

            Stmt::If { cond, body } => {
                let _ = writeln!(fn_file, "\n# if block")?;
                let _ = compile_expr(fn_file, &program.exprs[cond.clone()], WriteTarget::R0)?;
                let _ = writeln!(fn_file, "execute if score accum r0 matches 1.. store result score accum r0 run function test:{body}_{}", fn_decl.name)?;
                let _ = writeln!(fn_file, "execute if score accum r0 matches 1 run return 1");
                range.start = program.blocks[fn_decl.blocks.start + *body].range.end;
                continue;
            }
        }
        range.start += 1;
    }
    Ok(())
}

pub fn compile(program: &Program) -> IOResult<()> {
    let mut file_path: std::path::PathBuf;
    { // creating the function directory
        file_path = std::env::current_dir().inspect_err(|err| {
            eprintln!("ERROR: could not get current directory: {err}");
        })?.join("function");

        if std::path::Path::exists(&file_path) {
            let _ = std::fs::remove_dir_all(&file_path).inspect_err(|err| {
                eprintln!("ERROR: could not clean `function` directory: {err}");
            })?;
        } 

        let _ = std::fs::create_dir_all(&file_path).inspect_err(|err| {
            eprintln!("ERROR: could not create function dir: {err}");
        })?;
    }

    file_path.push("olla");
    for fn_decl in &program.fns {
        file_path.set_file_name(fn_decl.name);
        file_path.set_extension("mcfunction");

        let mut file = File::create(&file_path).inspect_err(|err| {
            eprintln!("ERROR: could not create file `{}.mcfunction`: {err}", fn_decl.name);
        })?;

        let _ = writeln!(file, "# parameters")?;
        for (i, param) in fn_decl.params.iter().enumerate() {
            writeln!(file, "data modify storage mcs local[-1].{param} set from storage mcs local[-1].{i}")
                .inspect_err(|err| {
                    eprintln!("ERROR: could not write file: {err}");
                })?;
        }

        if fn_decl.name == "main" {
            write_premain(&mut file).inspect_err(|err| {
                eprintln!("ERROR: could not write premain: {err}");
            })?;
            let _ = compile_block(program, fn_decl, &mut file, 0)?;
            write_postmain(&mut file).inspect_err(|err| {
                eprintln!("ERROR: could not write postmain: {err}");
            })?;
        } else {
            let _ = compile_block(program, fn_decl, &mut file, 0)?;
        }

        let mut i = 1;
        let len = fn_decl.blocks.end - fn_decl.blocks.start;
        while i < len {
            file_path.set_file_name(format!("{}_{}", i, fn_decl.name));
            file_path.set_extension("mcfunction");
            file = File::create(&file_path).inspect_err(|err| {
                eprintln!("ERROR: could not create function block file: {err}");
            })?;
            let _ = compile_block(program, fn_decl, &mut file, i)?;

            i += 1;
        }
    }

    Ok(())
}
