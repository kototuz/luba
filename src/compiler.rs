////
////struct SetFromCmd<'a>(SetTarget<'a>);
////enum SetTarget<'a> {
////    LocalVar(&'a str),
////    GlobalVar(&'a str),
////    Stack,
////}
////
////
////
////impl<'a> fmt::Display for SetTarget<'a> {
////    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
////        match self {
////            SetTarget::LocalVar(name) => {
////                write!(f, "data modify storage mcs local[-1].{name} set")
////            },
////            SetTarget::GlobalVar(name) => {
////                write!(f, "data modify storage mcs {name} set")
////            },
////            SetTarget::Stack => {
////                write!(f, "data modify storage mcs stack append")
////            },
////        }
////    }
////}
////
////impl<'a> fmt::Display for SetFromCmd<'a> {
////    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
////        match self.0 {
////            SetTarget::LocalVar(name) => {
////                write!(f, "execute store result storage mcs local[-1].{name} int 1 run")
////            },
////            SetTarget::GlobalVar(name) => {
////                write!(f, "execute store result storage mcs {name} int 1 run")
////            },
////            SetTarget::Stack => {
////                write_ln!(f, "data modify storage mcs stack append value 0");
////                write!(f, "execute store result storage mcs stack[-1] int 1 run")
////            },
////        }
////    }
////}
////
////struct Compiler<'a> {
////    prog: Program<'a>,
////    fn_dir_path: std::path::PathBuf,
////}
////
////impl<'a> Compiler<'a> {
////    pub fn new(prog: Program<'a>) -> Self {
////        let mut fn_dir_path = std::env::current_dir().unwrap_or_else(|err| {
////            compilation_err!("Could not get the current direction: {err}");
////        }).join("function");
////
////        if std::path::Path::exists(&fn_dir_path) {
////            let _ = std::fs::remove_dir_all(&fn_dir_path).unwrap_or_else(|err| {
////                compilation_err!("Could not clean the `function` directory: {err}");
////            });
////        } 
////
////        let _ = std::fs::create_dir_all(&fn_dir_path).inspect_err(|err| {
////            compilation_err!("Could not create the `function` directory: {err}");
////        });
////
////        Self { prog, fn_dir_path }
////    }
////
////    pub fn compile(self) {
////        for fn_decl in &self.prog.fns {
////            let mut file = self.create_fn_file(fn_decl.name);
////
////            for (i, param) in fn_decl.params.iter().enumerate() {
////                write_ln!(file, "data modify storage mcs local[-1].{param} set from storage mcs local[-1].{i}");
////            }
////
////            if fn_decl.name == "main" {
////                Self::write_premain(&mut file);
////                self.write_block(&mut file, fn_decl.blocks.start);
////                Self::write_postmain(&mut file);
////            } else {
////                self.write_block(&mut file, fn_decl.blocks.start);
////            }
////
////            let mut block_idx = fn_decl.blocks.start+1;
////            while block_idx < fn_decl.blocks.end {
////                self.compile_block(block_idx);
////                block_idx += 1;
////            }
////        }
////    }
////
////    fn compile_block(&self, block_idx: usize) {
////        let mut file = self.create_fn_file(block_idx.to_string().as_str());
////        self.write_block(&mut file, block_idx);
////    }
////
////    fn write_block(&self, file: &mut File, block_idx: usize) {
////        let mut range = self.prog.blocks[block_idx].range.clone();
////        while range.start < range.end {
////            match &self.prog.stmts[range.start] {
////                Stmt::VarAssign { name, expr } => {
////                    write_ln!(file, "\n# assign var `{name}`");
////                    self.compile_expr_range(file, expr.clone(), SetTarget::LocalVar(name));
////                },
////
////                Stmt::Return(expr_range) => {
////                    write_ln!(file, "\n# return");
////                    self.compile_expr_range(file, expr_range.clone(), SetTarget::GlobalVar("return"));
////                    write_ln!(file, "return 1");
////                },
////
////                Stmt::If { cond, body } => {
////                    write_ln!(file, "\n# if block");
////                    self.compile_expr_range(file, cond.clone(), SetTarget::Stack);
////                    write_ln!(file, "execute store result score accum r0 run data get storage mcs stack[-1]");
////                    write_ln!(file, "data remove storage mcs stack[-1]");
////                    write_ln!(file, "execute if score accum r0 matches 1.. store result score accum r0 run function test:{body}");
////                    write_ln!(file, "execute if score accum r0 matches 1 run return 1");
////                    range.start = self.prog.blocks[*body].range.end;
////                    continue;
////                }
////            }
////
////            range.start += 1;
////        }
////    }
////
////    fn write_premain(main: &mut File) {
////        write_ln!(main, "# premain");
////        write_ln!(main, "scoreboard objectives add r0 dummy");
////        write_ln!(main, "scoreboard objectives add r1 dummy");
////        write_ln!(main, "data modify storage mcs local append value {{}}");
////    }
////
////    fn write_postmain(main: &mut File) {
////        write_ln!(main, "\n# postmain");
////        write_ln!(main, "data remove storage mcs local");
////        write_ln!(main, "data remove storage mcs stack");
////        write_ln!(main, "data remove storage mcs return");
////    }
////
////    fn compile_expr_range(
////        &self,
////        file: &mut File,
////        mut range: ExprRange,
////        target: SetTarget
////    ) {
////        range.end -= 1;
////        let mut local_scope_offset: usize = 1;
////        while range.start < range.end {
////            if let Expr::SetArg(idx) = self.prog.exprs[range.start+1] {
////                if let Expr::FnCall(_) = self.prog.exprs[range.start] {
////                    if idx == 0 {
////                        write_ln!(file, "data modify storage mcs local insert -2 value {{}}");
////                        local_scope_offset += 1;
////                    }
////                    self.compile_expr(file, range.start, local_scope_offset, SetTarget::LocalVar(idx.to_string().as_str()));
////                    local_scope_offset -= 1;
////                } else {
////                    if idx == 0 {
////                        write_ln!(file, "data modify storage mcs local append value {{}}");
////                        local_scope_offset += 1;
////                    }
////                    self.compile_expr(file, range.start, local_scope_offset, SetTarget::LocalVar(idx.to_string().as_str()));
////                }
////                range.start += 1;
////            } else if let Expr::FnCall(_) = self.prog.exprs[range.start+1] {
////                write_ln!(file, "data modify storage mcs local append value {{}}");
////            } else {
////                self.compile_expr(file, range.start, local_scope_offset, SetTarget::Stack);
////            }
////            range.start += 1;
////        }
////        self.compile_expr(file, range.start, 1, target);
////    }
////
////    fn compile_expr(
////        &self,
////        file: &mut File,
////        expr_idx: usize,
////        local_scope_offset: usize,
////        target: SetTarget
////    ) {
////        match &self.prog.exprs[expr_idx] {
////            Expr::Var(var_name) => {
////                write_ln!(file, "{target} from storage mcs local[-{local_scope_offset}].{var_name}");
////            },
////
////            Expr::Num(number) => {
////                write_ln!(file, "{target} value {number}");
////            },
////
////            Expr::BinOp(op) => {
////                write_ln!(file, "execute store result score accum r0 run data get storage mcs stack[-2]");
////                write_ln!(file, "execute store result score accum r1 run data get storage mcs stack[-1]");
////                write_ln!(file, "data remove storage mcs stack[-1]");
////                write_ln!(file, "data remove storage mcs stack[-1]");
////                match op {
////                    x @ (BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div) => {
////                        write_ln!(file, "{} scoreboard players operation accum r0 {x}= accum r1", SetFromCmd(target));
////                    },
////                    x @ (BinOpKind::Gt | BinOpKind::Ge | BinOpKind::Lt | BinOpKind::Le) => {
////                        write_ln!(file, "{} execute if score accum r0 {x} accum r1", SetFromCmd(target));
////                    },
////                    BinOpKind::Eq => {
////                        write_ln!(file, "{} execute if score accum r0 = accum r1", SetFromCmd(target));
////                    },
////                    BinOpKind::Ne => {
////                        write_ln!(file, "{} execute unless score accum r0 = accum r1", SetFromCmd(target));
////                    },
////                    BinOpKind::And => {
////                        write_ln!(file, "{} execute if score accum r0 matches 1.. if score accum r1 matches 1..", SetFromCmd(target));
////                    },
////                    BinOpKind::Or => {
////                        write_ln!(file, "scoreboard players operation accum r0 > accum r1");
////                        write_ln!(file, "{} execute if score accum r0 matches 1..", SetFromCmd(target));
////                    },
////                }
////            },
////
////            Expr::FnCall(name) => {
////                // TODO: custom namespaces
////                write_ln!(file, "function test:{name}");
////
////                write_ln!(file, "data remove storage mcs local[-1]");
////                write_ln!(file, "{target} from storage mcs return");
////            },
////
////            Expr::SetArg(_) => panic!("Expr::SetArg must be unreachable"),
////            Expr::OpenParen => panic!("Expr::OpenParen must be unreachable"),
////        }
////    }
////
////    fn create_fn_file(&self, name: &str) -> File {
////        File::create(self.fn_dir_path.join(name).with_extension("mcfunction"))
////            .unwrap_or_else(|err| {
////                compilation_err!("Could not create a `mcfunction` file: {err}");
////            })
////    }
////}
////
////
//#[derive(Debug)]
//enum Reg { SP, SP2, IP }
//
//#[derive(Debug)]
//enum Inst {
//    Nop,
//    Add,
//    Sub,
//    Mul,
//    Div,
//    Mod,
//    Eq,
//    Ne,
//    Gt,
//    Ge,
//    Lt,
//    Le,
//    And,
//    Or,
//    RegAdd(Reg, usize),
//    RegSub(Reg, usize),
//    RegCp(Reg, Reg),
//    RegGet(Reg),
//    RegSet(Reg),
//    GetLocal(usize),
//    SetLocal(usize),
//    Const(i32),
//    JmpIf(usize),
//    Jmp(usize),
//    Call(usize),
//    Log(usize),
//}
//
//impl fmt::Display for Reg {
//    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//        match self {
//            Reg::SP  => write!(f, "sp"),
//            Reg::SP2 => write!(f, "sp2"),
//            Reg::IP  => write!(f, "ip"),
//        }
//    }
//}
//
//fn compile_expr(
//    st: &semantic::Analyzer,
//    insts: &mut Vec<Inst>,
//    expr: &Expr
//) {
//    match &expr.kind {
//        ExprKind::Var(name) => insts.push(Inst::GetLocal(st.var_sp2_offset(name))),
//        ExprKind::Num(n)    => insts.push(Inst::Const(*n)),
//        ExprKind::FnCall(data) => {
//            let fn_decl = st.global_st.get(data.name).unwrap();
//            insts.push(Inst::RegAdd(Reg::SP, 1));
//            for arg in &data.args {
//                compile_expr(st, insts, arg);
//            }
//
//            insts.push(Inst::Call(fn_decl.ip));
//            insts.push(Inst::RegSub(Reg::SP, fn_decl.param_count));
//        },
//        ExprKind::BinOp(data) => {
//            compile_expr(st, insts, &data.lhs);
//            compile_expr(st, insts, &data.rhs);
//            match data.op {
//                BinOpKind::And => insts.push(Inst::And),
//                BinOpKind::Or  => insts.push(Inst::Or),
//                BinOpKind::Add => insts.push(Inst::Add),
//                BinOpKind::Sub => insts.push(Inst::Sub),
//                BinOpKind::Mul => insts.push(Inst::Mul),
//                BinOpKind::Div => insts.push(Inst::Div),
//                //BinOpKind::Mod => insts.push(Inst::Mod),
//                BinOpKind::Eq  => insts.push(Inst::Eq),
//                BinOpKind::Ne  => insts.push(Inst::Ne),
//                BinOpKind::Gt  => insts.push(Inst::Gt),
//                BinOpKind::Ge  => insts.push(Inst::Ge),
//                BinOpKind::Lt  => insts.push(Inst::Lt),
//                BinOpKind::Le  => insts.push(Inst::Le),
//            }
//        }
//    }
//}
//
//fn compile_block(
//    st: &semantic::Analyzer,
//    insts: &mut Vec<Inst>,
//    block: &Block
//) {
//    let mut jmpbuf = [0; 2];
//    for stmt in block {
//        match &stmt.kind {
//            StmtKind::VarAssign { name, expr } => {
//                compile_expr(st, insts, expr);
//                insts.push(Inst::SetLocal(st.var_sp2_offset(name)));
//            },
//
//            StmtKind::If { cond, then, elze } => {
//                compile_expr(st, insts, cond);
//                insts.push(Inst::JmpIf(insts.len()+2));
//                jmpbuf[0] = insts.len();
//                insts.push(Inst::Nop);
//                compile_block(st, insts, then);
//
//                if !elze.is_empty() {
//                    jmpbuf[1] = insts.len();
//                    insts.push(Inst::Nop);
//                    insts[jmpbuf[0]] = Inst::Jmp(insts.len());
//                    compile_block(st, insts, elze);
//                    insts[jmpbuf[1]] = Inst::Jmp(insts.len());
//                } else {
//                    insts[jmpbuf[0]] = Inst::Jmp(insts.len());
//                }
//            },
//
//            StmtKind::For { cond, body } => {
//                // condition
//                jmpbuf[0] = insts.len();
//                compile_expr(st, insts, cond);
//                insts.push(Inst::JmpIf(insts.len()+2));
//                jmpbuf[1] = insts.len();
//                insts.push(Inst::Nop);
//
//                // body
//                compile_block(st, insts, body);
//                insts.push(Inst::Jmp(jmpbuf[0])); // repeat
//                insts[jmpbuf[1]] = Inst::Jmp(insts.len()); // end
//            },
//
//            StmtKind::ReturnVal(expr) => {
//                compile_expr(st, insts, expr);
//                insts.push(Inst::SetLocal(0));
//                insts.push(Inst::RegSub(Reg::SP, st.local_count));
//                insts.push(Inst::RegSet(Reg::SP2));
//                insts.push(Inst::RegSet(Reg::IP));
//            },
//
//            StmtKind::Return => {
//                insts.push(Inst::RegSub(Reg::SP, st.local_count));
//                insts.push(Inst::RegSet(Reg::SP2));
//                insts.push(Inst::RegSet(Reg::IP));
//            },
//
//            StmtKind::FnCall { name, args } => {
//                if *name == "log" {
//                    assert_eq!(args.len(), 1);
//                    if let ExprKind::Var(name) = args[0].kind {
//                        insts.push(Inst::Log(st.var_sp2_offset(name)));
//                    } else { panic!(); }
//                    return;
//                }
//
//                for arg in args {
//                    compile_expr(st, insts, arg);
//                }
//                let fn_decl = st.global_st.get(name).unwrap();
//                insts.push(Inst::Call(fn_decl.ip));
//            },
//
//            StmtKind::VarDecl(_) => {}, // skip
//        }
//    }
//}
//
//pub fn compile(ast: Ast, file: &mut File) {
//    let mut insts: Vec<Inst> = Vec::new();
//    let mut analyzer = semantic::Analyzer::new();
//
//    self.write_inst(Inst::Add)
//    insts.push(Inst::Nop);
//    insts.push(Inst::Nop);
//
//    for fn_decl in &ast.fn_decls {
//        analyzer.analyze_fn_decl(fn_decl, insts.len());
//        insts.push(Inst::RegGet(Reg::SP2));
//        insts.push(Inst::RegCp(Reg::SP2, Reg::SP));
//        if fn_decl.has_result {
//            insts.push(Inst::RegSub(Reg::SP2, analyzer.param_count+3));
//        } else {
//            insts.push(Inst::RegSub(Reg::SP2, analyzer.param_count+2));
//        }
//        insts.push(Inst::RegAdd(Reg::SP, analyzer.local_count));
//        compile_block(
//            &analyzer,
//            &mut insts,
//            &fn_decl.body
//        );
//    }
//
//
//    let entry_point = analyzer.global_st.get("main").
//        unwrap_or_else(|| {
//            compilation_err!("The main function is not defined");
//        }).ip;
//
//    insts[0] = Inst::Call(entry_point);
//    insts[1] = Inst::Jmp(1000);
//
//    for inst in &insts {
//        match inst {
//            Inst::Nop               => {},                                                          
//            Inst::Log(local)        => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/log {{_:{local}}}'"); }
//            Inst::Call(ip)          => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/call {{_:{ip}}}'"); },
//            Inst::Add               => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/add'"); },
//            Inst::Sub               => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/sub'"); },
//            Inst::Mul               => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/mul'"); },
//            Inst::Div               => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/div'"); },
//            Inst::Mod               => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/mod'"); },
//            Inst::Eq                => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/eq'"); },
//            Inst::Ne                => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/ne'"); },
//            Inst::Gt                => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/gt'"); },
//            Inst::Ge                => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/ge'"); },
//            Inst::Lt                => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/lt'"); },
//            Inst::Le                => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/le'"); },
//            Inst::And               => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/and'"); },
//            Inst::Or                => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/or'"); },
//            Inst::RegGet(reg)       => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/get_reg {{_:{reg}}}'"); },
//            Inst::RegSet(reg)       => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/set_reg {{_:{reg}}}'"); },
//            Inst::GetLocal(n)       => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/get_local {{_:{n}}}'"); },
//            Inst::SetLocal(n)       => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/set_local {{_:{n}}}'"); },
//            Inst::Const(n)          => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/const {{_:{n}}}'"); },
//            Inst::JmpIf(n)          => { write_ln!(file, "data modify storage redvm insts append value 'function redvm:insts/jmp_if {{_:{n}}}'"); },
//            Inst::RegAdd(reg, n)    => { write_ln!(file, "data modify storage redvm insts append value 'scoreboard players add {reg} redvm.regs {n}'"); },
//            Inst::RegSub(reg, n)    => { write_ln!(file, "data modify storage redvm insts append value 'scoreboard players remove {reg} redvm.regs {n}'"); },
//            Inst::RegCp(reg1, reg2) => { write_ln!(file, "data modify storage redvm insts append value 'scoreboard players operation {reg1} redvm.regs = {reg2} redvm.regs'"); },
//            Inst::Jmp(n)            => { write_ln!(file, "data modify storage redvm insts append value 'scoreboard players set ip redvm.regs {n}'"); },
//        }
//    }
//
//    for (i, inst) in insts.iter().enumerate() {
//        println!("{i}: {inst:?}");
//    }
//
//    println!("{:?}", ast.fn_decls[0].body);
//}

use std::{collections::HashMap, fmt::format, fs::File, io::{Seek, SeekFrom, Write}};

use crate::{compilation_err, exit_failure, lexer::BinOpKind, parser::{Ast, Block, Expr, ExprKind, StmtKind}, semantic::{Analyzer, LocalScope}};

type IP = usize;
type FilePos = u64;
type Label = IP;
type JmpLabel = usize;

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

    fn compile_expr(&mut self, expr: &ExprKind, scope: &LocalScope) {
        match expr {
            ExprKind::Num(n) => {
                inst!(self, "const {{_:{n}}}");
            },

            ExprKind::Var(name) => {
                inst!(
                    self, "get_local {{_:{}}}",
                    scope.get(name).unwrap()
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

    fn compile_block(&mut self, block: &Block<'a>, scope: &LocalScope, loopend: JmpLabel) {
        for stmt in block {
            match &stmt.kind {
                StmtKind::VarDecl(_) => {},
                StmtKind::VarAssign { name, expr } => {
                    self.compile_expr(&expr.kind, scope);
                    let local_idx = scope.get(name).unwrap();
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

                StmtKind::If { cond, then } => {
                    self.compile_expr(&cond.kind, scope);

                    let then_label = self.new_jmp_label();
                    let end_label = self.new_jmp_label();

                    self.jmpif_label(then_label);
                    self.jmp_label(end_label);

                    self.set_jmp_label(then_label);
                    self.compile_block(then, scope, loopend);
                    self.set_jmp_label(end_label);
                },

                StmtKind::IfElse { cond, then, elze } => {
                    self.compile_expr(&cond.kind, scope);

                    let then_label = self.new_jmp_label();
                    let else_label = self.new_jmp_label();
                    let end_label = self.new_jmp_label();

                    self.jmpif_label(then_label);
                    self.jmp_label(else_label);

                    self.set_jmp_label(then_label);
                    self.compile_block(then, scope, loopend);
                    self.jmp_label(end_label);

                    self.set_jmp_label(else_label);
                    self.compile_block(elze, scope, loopend);

                    self.set_jmp_label(end_label);
                },

                StmtKind::For { body } => {
                    let loop_label = self.new_jmp_label();
                    let loop_end_label = self.new_jmp_label();

                    self.set_jmp_label(loop_label);
                    self.compile_block(body, scope, loop_end_label);
                    self.jmp_label(loop_label);
                    self.set_jmp_label(loop_end_label);
                },

                StmtKind::Break => {
                    self.jmp_label(loopend);
                },
            }
        }
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

pub fn compile(file: File, ast: &Ast, semdata: Vec<LocalScope>) {
    let mut comp = Compiler {
        call_labels: HashMap::new(),
        call_label_usages: Vec::new(),
        jmp_labels: Vec::new(),
        jmp_label_usages: Vec::new(),
        file,
        ip: 0
    };

    comp.call_label("main");
    cmd!(comp, "scoreboard players set ip redvm.regs 1000");

    for i in 0..ast.fn_decls.len() {
        let fndecl = &ast.fn_decls[i];
        comp.set_call_label(fndecl.name);

        let ret_label = comp.new_jmp_label();
        assert_eq!(ret_label, 0);

        // creating stack frame
        inst!(comp, "get_reg {{_:sp2}}");
        cmd!(comp, "scoreboard players operation sp2 redvm.regs = sp redvm.regs");
        cmd!(comp, "scoreboard players remove sp2 redvm.regs {}", fndecl.params.len()+fndecl.has_result as usize + 2);
        cmd!(comp, "scoreboard players add sp redvm.regs {}", semdata[i].len()-fndecl.params.len());

        comp.compile_block(&fndecl.body, &semdata[i], 0);

        comp.set_jmp_label(ret_label);
        cmd!(comp, "scoreboard players remove sp redvm.regs {}", semdata[i].len()-fndecl.params.len());
        inst!(comp, "set_reg {{_:sp2}}");
        inst!(comp, "set_reg {{_:ip}}");

        comp.write_jmp_labels();
    }

    comp.write_call_labels();

    //println!("{ast:#?}");
    //println!("{semdata:#?}");
}
