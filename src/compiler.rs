use std::{collections::HashMap, fs::File, io::{Seek, SeekFrom, Write}};

use crate::{compilation_err, exit_failure, lexer::BinOpKind, parser::{Ast, Block, ExprKind, Stmt, StmtKind}, semantic::{Name, Scope, ScopeIdx, Type, SP2}};

type IP = usize;
type FilePos = u64;
type JmpLabel = usize;

struct Loop {
    start: JmpLabel,
    end:   JmpLabel,
}

struct CallLabelUsage<'a> {
    pos:  FilePos,
    name: &'a str,
}

struct JmpLabelUsage {
    pos:   FilePos,
    label: JmpLabel,
}

struct Compiler<'a> {
    call_labels: HashMap<&'a str, IP>,
    call_label_usages: Vec<CallLabelUsage<'a>>,

    jmp_labels: Vec<IP>,
    jmp_label_usages: Vec<JmpLabelUsage>,

    curr_scope_idx: ScopeIdx,
    scopes: Vec<Scope<'a>>,
    file: File,
    ip: IP,
}

macro_rules! write_ln {
    ($file:expr, $($args:tt)*) => {
        let _ = write!($file, $($args)*).unwrap_or_else(|err| {
            compilation_err!("Could not write: {err}");
        });
    }
}

macro_rules! cmd {
    ($comp:ident, $($arg:tt)*) => {
        write_ln!($comp.file, "data modify storage redvm insts append value '");
        write_ln!($comp.file, $($arg)*);
        write_ln!($comp.file, "'\n");
        $comp.ip += 1;
    };
}

macro_rules! inst {
    ($comp:ident, $($arg:tt)*) => {
        write_ln!($comp.file, "data modify storage redvm insts append value 'function redvm:insts/");
        write_ln!($comp.file, $($arg)*);
        write_ln!($comp.file, "'\n");
        $comp.ip += 1;
    };
}

impl<'a> Compiler<'a> {
    const RET_JMP_LABEL: usize = 0;

    fn call_label(&mut self, name: &'a str) {
        inst!(self, "call {{_:0000000000}}");
        let pos = self.curr_pos()-13; // -13 to get the start position of addr
        self.call_label_usages.push(CallLabelUsage { pos, name });
    }

    fn jmpif_label(&mut self, label: JmpLabel) {
        inst!(self, "jmp_if {{_:0000000000}}");
        let pos = self.curr_pos()-13; // -13 to get the start position of addr
        self.jmp_label_usages.push(JmpLabelUsage { pos, label });
    }

    fn jmp_label(&mut self, label: JmpLabel) {
        cmd!(self, "scoreboard players set ip redvm.regs 0000000000");
        let pos = self.curr_pos()-12; // -12 to get the start position of addr
        self.jmp_label_usages.push(JmpLabelUsage { pos, label });
    }

    fn new_jmp_label(&mut self) -> JmpLabel {
        self.jmp_labels.push(0); // label is not init yet
        self.jmp_labels.len()-1
    }

    fn set_jmp_label(&mut self, label: JmpLabel) {
        self.jmp_labels[label] = self.ip;
    }

    fn set_call_label(&mut self, label_name: &'a str) {
        write_ln!(self.file, "\n# {label_name}\n"); // this is just a comment
        self.call_labels.insert(label_name, self.ip);
    }

    fn write_call_labels(mut self) {
        for i in 0..self.call_label_usages.len() {
            self.seek(self.call_label_usages[i].pos);
            let data = *self.call_labels.get(self.call_label_usages[i].name).unwrap();
            write_ln!(self.file, "{data:0>10}");
        }
    }

    fn write_jmp_labels(&mut self) {
        let currpos = self.curr_pos();
        for i in 0..self.jmp_label_usages.len() {
            self.seek(self.jmp_label_usages[i].pos);
            let data = self.jmp_labels[self.jmp_label_usages[i].label];
            assert_ne!(data, 0);
            write_ln!(self.file, "{data:0>10}");
        }

        self.jmp_labels.clear();
        self.jmp_label_usages.clear();
        self.seek(currpos);
    }

    fn compile_expr(&mut self, expr: &ExprKind, scope: ScopeIdx) {
        match expr {
            ExprKind::Num(n) => {
                inst!(self, "const {{_:{n}}}");
            },

            ExprKind::Var(name) => {
                inst!(
                    self, "get_local {{_:{}}}",
                    self.get_type_var(name, scope)
                );
            },

            ExprKind::BinOp(data) => {
                self.compile_expr(&data.lhs.kind, scope);
                self.compile_expr(&data.rhs.kind, scope);
                inst!(self, "{}", Self::binop_to_inst(data.op.clone()));
            },

            ExprKind::FnCall(data) => {
                cmd!(self, "scoreboard players add sp redvm.regs 1");
                for arg in &data.args {
                    self.compile_expr(&arg.kind, scope);
                }
                self.call_label(data.name);
                cmd!(self, "scoreboard players remove sp redvm.regs {}", data.args.len());
            },
        }
    }

    fn get_type_var(&self, name: Name, scope: ScopeIdx) -> SP2 {
        println!("{}", self.curr_scope_idx);
        if let Some(Type::Var(sp2)) = self.scopes[scope].items.get(name) {
            *sp2
        } else if scope != 0 {
            self.get_type_var(name, self.scopes[scope].parent)
        } else {
            unreachable!("{name}");
        }
    }

    fn compile_stmt(&mut self, stmt: &Stmt<'a>, scope: ScopeIdx, lup: &Loop) {
        match &stmt.kind {
            StmtKind::VarDecl(_) => {},
            StmtKind::VarAssign { name, expr } => {
                self.compile_expr(&expr.kind, scope);
                let local_idx = self.get_type_var(name, scope);
                inst!(self, "set_local {{_:{local_idx}}}");
            },

            StmtKind::VarDeclAssign { name, expr } => {
                self.compile_expr(&expr.kind, scope);
                let local_idx = self.get_type_var(name, scope);
                inst!(self, "set_local {{_:{local_idx}}}");
            },

            StmtKind::ReturnVal(expr) => {
                self.compile_expr(&expr.kind, scope);
                inst!(self, "set_local {{_:0}}");
                self.jmp_label(Self::RET_JMP_LABEL);
            },

            StmtKind::Return => {
                self.jmp_label(Self::RET_JMP_LABEL);
            },

            StmtKind::FnCall { name, args } => {
                cmd!(self, "scoreboard players add sp redvm.regs 1");
                for arg in args { self.compile_expr(&arg.kind, scope); }
                self.call_label(name);
                cmd!(self, "scoreboard players remove sp redvm.regs {}", args.len()+1);
            },

            StmtKind::BuilinFnCall { arg, name } => {
                match *name {
                    "log" => {
                        inst!(self, "log {{_:{}}}", self.get_type_var(arg, scope));
                    },

                    "cmd" => {
                        cmd!(self, "{arg}");
                    },

                    _ => unreachable!()
                }
            },

            StmtKind::If { cond, then, elzeifs, elze } => {
                let mut then_label = self.new_jmp_label();
                let mut else_label = self.new_jmp_label();
                let end_label      = self.new_jmp_label();

                self.compile_expr(&cond.kind, scope);
                self.jmpif_label(then_label);
                self.jmp_label(else_label);
                self.set_jmp_label(then_label);

                self.curr_scope_idx += 1;
                self.compile_block(then, self.curr_scope_idx, lup);

                self.jmp_label(end_label);

                for elzeif in elzeifs {
                    self.set_jmp_label(else_label);

                    then_label = self.new_jmp_label();
                    else_label = self.new_jmp_label();

                    self.compile_expr(&elzeif.cond.kind, scope);
                    self.jmpif_label(then_label);
                    self.jmp_label(else_label);
                    self.set_jmp_label(then_label);

                    self.curr_scope_idx += 1;
                    self.compile_block(&elzeif.then, self.curr_scope_idx, lup);

                    self.jmp_label(end_label);
                }

                self.set_jmp_label(else_label);

                self.curr_scope_idx += 1;
                self.compile_block(elze, self.curr_scope_idx, lup);

                self.set_jmp_label(end_label);
            },

            StmtKind::For { body, init, cond, post } => {
                let forlup = Loop {
                    start: self.new_jmp_label(),
                    end:   self.new_jmp_label(),
                };

                if let Some(s) = init {
                    self.compile_stmt(s, scope, &lup);
                }

                self.set_jmp_label(forlup.start);
                if let Some(e) = cond {
                    let forloop_body = self.new_jmp_label();
                    self.compile_expr(&e.kind, scope);
                    self.jmpif_label(forloop_body);
                    self.jmp_label(forlup.end);
                    self.set_jmp_label(forloop_body);
                }

                self.curr_scope_idx += 1;
                self.compile_block(body, self.curr_scope_idx, &forlup);

                if let Some(s) = post {
                    self.compile_stmt(s, scope, &forlup);
                }

                self.jmp_label(forlup.start);
                self.set_jmp_label(forlup.end);
            },

            StmtKind::Break => {
                self.jmp_label(lup.end);
            },

            StmtKind::Continue => {
                self.jmp_label(lup.start);
            },

            StmtKind::FnDecl(data) => {
                self.set_call_label(data.name);

                let ret_label = self.new_jmp_label();
                assert_eq!(ret_label, 0);

                let local_len = if let Type::FnDecl(info) = self.scopes[0].items.get(data.name).unwrap() {
                    info.local_count
                } else {
                    unreachable!()
                };

                // creating stack frame
                inst!(self, "get_reg {{_:sp2}}");
                cmd!(self, "scoreboard players operation sp2 redvm.regs = sp redvm.regs");
                cmd!(self, "scoreboard players remove sp2 redvm.regs {}", data.params.len()+data.has_result as usize + 2);
                cmd!(self, "scoreboard players add sp redvm.regs {}", local_len);

                self.curr_scope_idx += 1;
                self.compile_block(&data.body, self.curr_scope_idx, &Loop { start: 0, end: 0 });

                self.set_jmp_label(ret_label);
                cmd!(self, "scoreboard players remove sp redvm.regs {}", local_len);
                inst!(self, "set_reg {{_:sp2}}");
                inst!(self, "set_reg {{_:ip}}");

                self.write_jmp_labels();
            },
        }
    }

    fn compile_block(&mut self, block: &Block<'a>, scope: ScopeIdx, lup: &Loop) {
        for stmt in block { self.compile_stmt(stmt, scope, lup); }
    }

    fn curr_pos(&mut self) -> FilePos {
        self.file.stream_position().unwrap_or_else(|err| {
            compilation_err!("Could not get current file position: {err}");
        })
    }

    fn seek(&mut self, pos: FilePos) {
        self.file.seek(SeekFrom::Start(pos)).unwrap_or_else(|err| {
            compilation_err!("Could not seek file: {err}");
        });
    }

    fn binop_to_inst(binop: BinOpKind) -> &'static str {
        match binop {
            BinOpKind::Add => "add",
            BinOpKind::Sub => "sub",
            BinOpKind::Mul => "mul",
            BinOpKind::Div => "div",
            BinOpKind::Mod => "mod",
            BinOpKind::Gt  => "gt",
            BinOpKind::Ge  => "ge",
            BinOpKind::Lt  => "lt",
            BinOpKind::Le  => "le",
            BinOpKind::Eq  => "eq",
            BinOpKind::Ne  => "ne",
            BinOpKind::And => "and",
            BinOpKind::Or  => "or",
        }
    }
}

pub fn compile(file: File, ast: &Ast, semdata: Vec<Scope>) {
    println!("{semdata:#?}");
    let mut comp = Compiler {
        call_labels: HashMap::new(),
        call_label_usages: Vec::new(),
        jmp_labels: Vec::new(),
        jmp_label_usages: Vec::new(),
        curr_scope_idx: 0,
        scopes: semdata,
        file,
        ip: 0
    };

    comp.call_label("main");
    cmd!(comp, "scoreboard players set ip redvm.regs 1000");
    comp.compile_block(&ast.stmts, 0, &Loop { start: 0, end: 0 });

    comp.write_call_labels();

    //println!("{ast:#?}");
}
