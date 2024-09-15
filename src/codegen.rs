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

fn create_main(dir: &Path) -> File {
    let mut main_file = create_mcfn_file(dir, "main");

    { // generate pre-main
      // TODO: for math operations consider using `minecraft storage`
        let _ = writeln!(&mut main_file, "scoreboard objectives add reg0 dummy").map_err(write_err);
        let _ = writeln!(&mut main_file, "scoreboard objectives add reg1 dummy").map_err(write_err);
        let _ = writeln!(&mut main_file, "scoreboard objectives add reg2 dummy").map_err(write_err);
        let _ = writeln!(&mut main_file, "scoreboard objectives add reg3 dummy").map_err(write_err);
    }

    main_file
}

fn write_err(err: std::io::Error) {
    eprintln!("ERROR: could not write file: {err}");
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
        curr_fn = if func.name == "main" {
            create_main(&function_dir)
        } else {
            create_mcfn_file(&function_dir, func.name)
        };

        for stmt_i in func.body.clone() {
            match &syntax.stmts[stmt_i] {
                parser::Stmt::VarAssign { name, expr } => {
                    let _ = writeln!(&mut curr_fn, "\n# assign var `{}`", name).map_err(write_err);

                    let mut reg_idx = 0;
                    for expr_i in expr.clone() {
                        match syntax.exprs[expr_i] {
                            parser::Expr::Var(n) => {
                                let _ = writeln!(
                                    &mut curr_fn,
                                    "execute store result score accum reg{} run data get storage minecraft:storage {}",
                                    reg_idx,
                                    n
                                ).map_err(write_err)?;
                                reg_idx += 1;
                            },

                            parser::Expr::Num(n) => {
                                let _ = writeln!(
                                    &mut curr_fn,
                                    "scoreboard players set accum reg{} {}",
                                    reg_idx, n
                                ).map_err(write_err)?;
                                reg_idx += 1;
                            },

                            parser::Expr::OpAdd  => {
                                reg_idx -= 1;
                                let _ = writeln!(
                                    &mut curr_fn,
                                    "scoreboard players operation accum reg{} += accum reg{}",
                                    reg_idx-1,
                                    reg_idx
                                ).map_err(write_err)?;
                            },

                            parser::Expr::OpSub  => {
                                reg_idx -= 1;
                                let _ = writeln!(
                                    &mut curr_fn,
                                    "scoreboard players operation accum reg{} -= accum reg{}",
                                    reg_idx-1,
                                    reg_idx
                                ).map_err(write_err)?;
                            },

                            parser::Expr::OpMul  => {
                                reg_idx -= 1;
                                let _ = writeln!(
                                    &mut curr_fn,
                                    "scoreboard players operation accum reg{} *= accum reg{}",
                                    reg_idx-1,
                                    reg_idx
                                ).map_err(write_err)?;
                            },

                            parser::Expr::OpDiv  => {
                                reg_idx -= 1;
                                let _ = writeln!(
                                    &mut curr_fn,
                                    "scoreboard players operation accum reg{} /= accum reg{}",
                                    reg_idx-1,
                                    reg_idx
                                ).map_err(write_err)?;
                            },

                            _ => unreachable!()
                        }
                    }

                    let _ = writeln!(
                        &mut curr_fn,
                        "execute store result storage minecraft:storage {} int 1 run scoreboard players get accum reg0",
                        name
                    ).map_err(write_err)?;
                },

                _ => todo!()
            }
        }
    }

    Ok(())
}
