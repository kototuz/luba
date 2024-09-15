use crate::parser;
use std::io::prelude::*;
use std::process::exit;
use std::path::Path;
use std::fs::File;

fn write_file(file: &mut File, text: &[u8]) {
    let _ = file.write_all(text).inspect_err(|err| {
        eprintln!("ERROR: could not write file: {err}");
        exit(1);
    });
}

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
        write_file(&mut main_file, b"scoreboard objectives add reg0 dummy\n");
        write_file(&mut main_file, b"scoreboard objectives add reg1 dummy\n");
        write_file(&mut main_file, b"scoreboard objectives add reg2 dummy\n");
        write_file(&mut main_file, b"scoreboard objectives add reg3 dummy\n");
    }

    main_file
}

pub fn gen_dp(
    output_dir_path: &Path,
    syntax: &parser::Syntax
) {
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
                    write_file(&mut curr_fn, format!("\n# assign var `{}`\n", name).as_bytes());

                    let mut reg_idx = 0;
                    for expr_i in expr.clone() {
                        match syntax.exprs[expr_i] {
                            parser::Expr::Var(n) => {
                                write_file(
                                    &mut curr_fn,
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
                                    &mut curr_fn,
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
                                    &mut curr_fn,
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
                                    &mut curr_fn,
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
                                    &mut curr_fn,
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
                                    &mut curr_fn,
                                    format!(
                                        "scoreboard players operation accum reg{} /= accum reg{}\n",
                                        reg_idx-1,
                                        reg_idx
                                    ).as_bytes()
                                );
                            },
                            _ => unreachable!()
                        }
                    }

                    write_file(
                        &mut curr_fn,
                        format!(
                            "execute store result storage minecraft:storage {} int 1 run scoreboard players get accum reg0\n",
                            name
                        ).as_bytes()
                    );
                },
                _ => todo!()
            }
        }
    }
}
