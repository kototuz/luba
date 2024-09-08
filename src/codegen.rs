use crate::parser;
use crate::dp;
use std::io::prelude::*;
use std::process::exit;

fn write_file(file: &mut std::fs::File, text: &[u8]) {
    let _ = file.write_all(text).inspect_err(|err| {
        eprintln!("ERROR: could not write file: {err}");
        exit(1);
    });
}

fn create_main(dp: &dp::Datapack) -> std::fs::File {
    let mut main_file = dp.create_fn_file("main");

    { // generate pre-main
        write_file(&mut main_file, b"scoreboard objectives add reg0 dummy\n");
        write_file(&mut main_file, b"scoreboard objectives add reg1 dummy\n");
        write_file(&mut main_file, b"scoreboard objectives add reg2 dummy\n");
        write_file(&mut main_file, b"scoreboard objectives add reg3 dummy\n");
    }

    main_file
}

pub fn gen_dp<'a>(
    dp_name: &'a str,
    exprs: &Vec<parser::Expr>,
    stmts: &Vec<parser::Stmt>
) {
    let dp = dp::Datapack::create(dp_name);

    let mut main = create_main(&dp);
    for stmt in stmts {
        match stmt {
            parser::Stmt::VarAssign { name, expr } => {
                write_file(&mut main, b"\n# variable assignment\n");

                let mut reg_idx = 0;
                for x in expr.clone() {
                    let expr = &exprs[x];
                    match expr {
                        parser::Expr::Var(n) => {
                            write_file(
                                &mut main,
                                format!(
                                    "execute store result score accum reg{} run data get storage minecraft:storage {}\n",
                                    reg_idx,
                                    n
                                ).as_bytes()
                            );
                            reg_idx += 1;
                        },
                        parser::Expr::Num(n) => {
                            write_file(
                                &mut main,
                                format!(
                                    "scoreboard players set accum reg{} {}\n",
                                    reg_idx,
                                    n
                                ).as_bytes()
                            );
                            reg_idx += 1;
                        },
                        parser::Expr::OpAdd  => {
                            reg_idx -= 1;
                            write_file(
                                &mut main,
                                format!(
                                    "scoreboard players operation accum reg{} += accum reg{}\n",
                                    reg_idx-1,
                                    reg_idx
                                ).as_bytes()
                            );
                        },
                        parser::Expr::OpSub  => {
                            reg_idx -= 1;
                            write_file(
                                &mut main,
                                format!(
                                    "scoreboard players operation accum reg{} -= accum reg{}\n",
                                    reg_idx-1,
                                    reg_idx
                                ).as_bytes()
                            );
                        },
                        parser::Expr::OpMul  => {
                            reg_idx -= 1;
                            write_file(
                                &mut main,
                                format!(
                                    "scoreboard players operation accum reg{} *= accum reg{}\n",
                                    reg_idx-1,
                                    reg_idx
                                ).as_bytes()
                            );
                        },
                        parser::Expr::OpDiv  => {
                            reg_idx -= 1;
                            write_file(
                                &mut main,
                                format!(
                                    "scoreboard players operation accum reg{} /= accum reg{}\n",
                                    reg_idx-1,
                                    reg_idx
                                ).as_bytes()
                            );
                        },
                    }
                }

                write_file(
                    &mut main,
                    format!(
                        "execute store result storage minecraft:storage {} int 1 run scoreboard players get accum reg0\n",
                        name
                    ).as_bytes()
                );
            }
        }
    }
}
