use crate::ir::*;
use std::fmt;
use std::slice::{from_mut, from_ref};

#[derive(Debug, Clone, Copy)]
pub enum Cond {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

impl fmt::Display for Cond {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let cond = match self {
            Cond::Eq => "eq",
            Cond::Ne => "ne",
            Cond::Lt => "lt",
            Cond::Gt => "gt",
            Cond::Le => "le",
            Cond::Ge => "ge",
        };
        write!(f, "{}", cond)
    }
}

#[derive(Debug)]
pub enum InstData {
    Nop(),
    Element(),
    Count(),
    Iconst(i32),
    Fconst(f32),
    Bconst(bool),
    Ftoi(Value),
    Itof(Value),
    Not(Value),
    Clz(Value),
    Add([Value; 2]),
    Sub([Value; 2]),
    Shl([Value; 2]),
    Shr([Value; 2]),
    Asr([Value; 2]),
    Ror([Value; 2]),
    Min([Value; 2]),
    Max([Value; 2]),
    And([Value; 2]),
    Or([Value; 2]),
    Xor([Value; 2]),
    Fadd([Value; 2]),
    Fsub([Value; 2]),
    Fmul([Value; 2]),
    Fmin([Value; 2]),
    Fmax([Value; 2]),
    Fminabs([Value; 2]),
    Fmaxabs([Value; 2]),
    Select([Value; 3]),
    Icmp(Cond, [Value; 2]),
    Fcmp(Cond, [Value; 2]),
    Alloc(u8),
    Fetch(Value),
    Read(Value),
    Write([Value; 2]),
    Load([Value; 2]),
    Store([Value; 2]),
    Jump(Block, Vec<Value>),
    Brallz(Block, Vec<Value>),
    Brallnz(Block, Vec<Value>),
    Branyz(Block, Vec<Value>),
    Branynz(Block, Vec<Value>),
    Return(),
}

impl InstData {
    pub fn args(&self) -> &[Value] {
        match self {
            InstData::Nop()
            | InstData::Element()
            | InstData::Count()
            | InstData::Iconst(_)
            | InstData::Fconst(_)
            | InstData::Bconst(_)
            | InstData::Alloc(_)
            | InstData::Return() => &[],
            InstData::Ftoi(arg)
            | InstData::Itof(arg)
            | InstData::Not(arg)
            | InstData::Clz(arg)
            | InstData::Fetch(arg)
            | InstData::Read(arg) => from_ref(arg),
            InstData::Add(args)
            | InstData::Sub(args)
            | InstData::Shl(args)
            | InstData::Shr(args)
            | InstData::Asr(args)
            | InstData::Ror(args)
            | InstData::Min(args)
            | InstData::Max(args)
            | InstData::And(args)
            | InstData::Or(args)
            | InstData::Xor(args)
            | InstData::Fadd(args)
            | InstData::Fsub(args)
            | InstData::Fmul(args)
            | InstData::Fmin(args)
            | InstData::Fmax(args)
            | InstData::Fminabs(args)
            | InstData::Fmaxabs(args)
            | InstData::Icmp(_, args)
            | InstData::Fcmp(_, args)
            | InstData::Write(args)
            | InstData::Load(args)
            | InstData::Store(args) => args,
            InstData::Select(args) => args,
            InstData::Jump(_, args)
            | InstData::Brallz(_, args)
            | InstData::Brallnz(_, args)
            | InstData::Branyz(_, args)
            | InstData::Branynz(_, args) => args,
        }
    }

    pub fn args_mut(&mut self) -> &mut [Value] {
        match self {
            InstData::Nop()
            | InstData::Element()
            | InstData::Count()
            | InstData::Iconst(_)
            | InstData::Fconst(_)
            | InstData::Bconst(_)
            | InstData::Alloc(_)
            | InstData::Return() => &mut [],
            InstData::Ftoi(arg)
            | InstData::Itof(arg)
            | InstData::Not(arg)
            | InstData::Clz(arg)
            | InstData::Fetch(arg)
            | InstData::Read(arg) => from_mut(arg),
            InstData::Add(args)
            | InstData::Sub(args)
            | InstData::Shl(args)
            | InstData::Shr(args)
            | InstData::Asr(args)
            | InstData::Ror(args)
            | InstData::Min(args)
            | InstData::Max(args)
            | InstData::And(args)
            | InstData::Or(args)
            | InstData::Xor(args)
            | InstData::Fadd(args)
            | InstData::Fsub(args)
            | InstData::Fmul(args)
            | InstData::Fmin(args)
            | InstData::Fmax(args)
            | InstData::Fminabs(args)
            | InstData::Fmaxabs(args)
            | InstData::Icmp(_, args)
            | InstData::Fcmp(_, args)
            | InstData::Write(args)
            | InstData::Load(args)
            | InstData::Store(args) => args,
            InstData::Select(args) => args,
            InstData::Jump(_, args)
            | InstData::Brallz(_, args)
            | InstData::Brallnz(_, args)
            | InstData::Branyz(_, args)
            | InstData::Branynz(_, args) => args,
        }
    }

    pub fn is_branch(&self) -> bool {
        match self {
            InstData::Brallz(..)
            | InstData::Brallnz(..)
            | InstData::Branyz(..)
            | InstData::Branynz(..) => true,
            _ => false,
        }
    }

    pub fn is_jump(&self) -> bool {
        match self {
            InstData::Jump(..) | InstData::Return(..) => true,
            _ => false,
        }
    }

    pub fn is_terminator(&self) -> bool {
        match self {
            InstData::Jump(..) | InstData::Return(..) => true,
            _ => false,
        }
    }

    pub fn target(&self) -> Option<Block> {
        match *self {
            InstData::Jump(block, ..)
            | InstData::Brallz(block, ..)
            | InstData::Brallnz(block, ..)
            | InstData::Branyz(block, ..)
            | InstData::Branynz(block, ..) => Some(block),
            _ => None,
        }
    }

    pub fn push_target_arg(&mut self, arg: Value) {
        match self {
            InstData::Jump(_, args)
            | InstData::Brallz(_, args)
            | InstData::Brallnz(_, args)
            | InstData::Branyz(_, args)
            | InstData::Branynz(_, args) => {
                args.push(arg);
            }
            _ => {}
        }
    }
}

impl fmt::Display for InstData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InstData::Nop() => write!(f, "nop"),
            InstData::Element() => write!(f, "element"),
            InstData::Count() => write!(f, "count"),
            InstData::Iconst(imm) => write!(f, "iconst {}", imm),
            InstData::Fconst(imm) => write!(f, "fconst {}", imm),
            InstData::Bconst(imm) => write!(f, "bconst {}", imm),
            InstData::Alloc(len) => write!(f, "alloc {}", len),
            InstData::Ftoi(arg) => write!(f, "ftoi {}", arg),
            InstData::Itof(arg) => write!(f, "itof {}", arg),
            InstData::Not(arg) => write!(f, "not {}", arg),
            InstData::Clz(arg) => write!(f, "clz {}", arg),
            InstData::Add(args) => write!(f, "add {}, {}", args[0], args[1]),
            InstData::Sub(args) => write!(f, "sub {}, {}", args[0], args[1]),
            InstData::Shl(args) => write!(f, "shl {}, {}", args[0], args[1]),
            InstData::Shr(args) => write!(f, "shr {}, {}", args[0], args[1]),
            InstData::Asr(args) => write!(f, "asr {}, {}", args[0], args[1]),
            InstData::Ror(args) => write!(f, "ror {}, {}", args[0], args[1]),
            InstData::Min(args) => write!(f, "min {}, {}", args[0], args[1]),
            InstData::Max(args) => write!(f, "max {}, {}", args[0], args[1]),
            InstData::And(args) => write!(f, "and {}, {}", args[0], args[1]),
            InstData::Or(args) => write!(f, "or {}, {}", args[0], args[1]),
            InstData::Xor(args) => write!(f, "xor {}, {}", args[0], args[1]),
            InstData::Fadd(args) => write!(f, "fadd {}, {}", args[0], args[1]),
            InstData::Fsub(args) => write!(f, "fsub {}, {}", args[0], args[1]),
            InstData::Fmul(args) => write!(f, "fmul {}, {}", args[0], args[1]),
            InstData::Fmin(args) => write!(f, "fmin {}, {}", args[0], args[1]),
            InstData::Fmax(args) => write!(f, "fmax {}, {}", args[0], args[1]),
            InstData::Fminabs(args) => write!(f, "fminabs {}, {}", args[0], args[1]),
            InstData::Fmaxabs(args) => write!(f, "fmaxabs {}, {}", args[0], args[1]),
            InstData::Icmp(cond, args) => write!(f, "icmp {} {}, {}", cond, args[0], args[1]),
            InstData::Fcmp(cond, args) => write!(f, "fcmp {} {}, {}", cond, args[0], args[1]),
            InstData::Fetch(arg) => write!(f, "fetch {}", arg),
            InstData::Read(arg) => write!(f, "read {}", arg),
            InstData::Write(args) => write!(f, "write {}, {}", args[0], args[1]),
            InstData::Load(args) => write!(f, "load {}, {}", args[0], args[1]),
            InstData::Store(args) => write!(f, "store {}, {}", args[0], args[1]),
            InstData::Select(args) => write!(f, "select {}, {}, {}", args[0], args[1], args[2]),
            InstData::Jump(block, args) => write!(f, "jmp {}({})", block, DisplaySlice(args)),
            InstData::Brallz(block, args) => write!(
                f,
                "brallz {} {}({})",
                args[0],
                block,
                DisplaySlice(&args[1..])
            ),
            InstData::Brallnz(block, args) => write!(
                f,
                "brallnz {} {}({})",
                args[0],
                block,
                DisplaySlice(&args[1..])
            ),
            InstData::Branyz(block, args) => write!(
                f,
                "branyz {} {}({})",
                args[0],
                block,
                DisplaySlice(&args[1..])
            ),
            InstData::Branynz(block, args) => write!(
                f,
                "branynz {} {}({})",
                args[0],
                block,
                DisplaySlice(&args[1..])
            ),
            InstData::Return() => write!(f, "ret"),
        }
    }
}
