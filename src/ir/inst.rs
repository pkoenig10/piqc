use std::fmt;

use collections::*;
use ir::*;

#[derive(Debug, Clone, Copy)]
pub struct IntConstInst {
    dest: Value,
    immediate: IntImmediate,
}

impl IntConstInst {
    pub fn new(dest: Value, immediate: IntImmediate) -> IntConstInst {
        IntConstInst { dest, immediate }
    }

    pub fn dest(&self) -> Value {
        self.dest
    }
}

impl fmt::Display for IntConstInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = iconst {}", self.dest, self.immediate)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FloatConstInst {
    dest: Value,
    immediate: FloatImmediate,
}

impl FloatConstInst {
    pub fn new(dest: Value, immediate: FloatImmediate) -> FloatConstInst {
        FloatConstInst { dest, immediate }
    }

    pub fn dest(&self) -> Value {
        self.dest
    }
}

impl fmt::Display for FloatConstInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = fconst {}", self.dest, self.immediate)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BoolConstInst {
    dest: Value,
    immediate: BoolImmediate,
}

impl BoolConstInst {
    pub fn new(dest: Value, immediate: BoolImmediate) -> BoolConstInst {
        BoolConstInst { dest, immediate }
    }

    pub fn dest(&self) -> Value {
        self.dest
    }
}

impl fmt::Display for BoolConstInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = bconst {}", self.dest, self.immediate)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IndexInst {
    dest: Value,
}

impl IndexInst {
    pub fn new(dest: Value) -> IndexInst {
        IndexInst { dest }
    }

    pub fn dest(&self) -> Value {
        self.dest
    }
}

impl fmt::Display for IndexInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = index", self.dest)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CountInst {
    dest: Value,
}

impl CountInst {
    pub fn new(dest: Value) -> CountInst {
        CountInst { dest }
    }

    pub fn dest(&self) -> Value {
        self.dest
    }
}

impl fmt::Display for CountInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = count", self.dest)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match *self {
            Not => "not",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Clone)]
pub struct UnaryInst {
    op: UnaryOp,
    dest: Value,
    src: Operand,
}

impl UnaryInst {
    pub fn new(op: UnaryOp, dest: Value, src: Operand) -> UnaryInst {
        UnaryInst { op, dest, src }
    }

    pub fn dest(&self) -> Value {
        self.dest
    }

    pub fn src(&self) -> Operand {
        self.src
    }
}

impl fmt::Display for UnaryInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {} {}", self.dest, self.op, self.src)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Asr,
    Shl,
    Min,
    Max,
    And,
    Or,
    Xor,
    Fadd,
    Fsub,
    Fmul,
    Fmin,
    Fmax,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match *self {
            Add => "add",
            Sub => "sub",
            Asr => "asr",
            Shl => "shl",
            Min => "min",
            Max => "max",
            And => "and",
            Or => "or",
            Xor => "xor",
            Fadd => "fadd",
            Fsub => "fsub",
            Fmul => "fmul",
            Fmin => "fmin",
            Fmax => "fmax",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Clone)]
pub struct BinaryInst {
    op: BinaryOp,
    dest: Value,
    left: Operand,
    right: Operand,
}

impl BinaryInst {
    pub fn new(op: BinaryOp, dest: Value, left: Operand, right: Operand) -> BinaryInst {
        BinaryInst {
            op,
            dest,
            left,
            right,
        }
    }

    pub fn dest(&self) -> Value {
        self.dest
    }

    pub fn left(&self) -> Operand {
        self.left
    }

    pub fn right(&self) -> Operand {
        self.right
    }
}

impl fmt::Display for BinaryInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} = {} {}, {}",
            self.dest,
            self.op,
            self.left,
            self.right
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CompOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

impl fmt::Display for CompOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match *self {
            Eq => "eq",
            Ne => "ne",
            Lt => "lt",
            Gt => "gt",
            Le => "le",
            Ge => "ge",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Clone)]
pub struct IntCompInst {
    op: CompOp,
    dest: Value,
    left: Operand,
    right: Operand,
}

impl IntCompInst {
    pub fn new(op: CompOp, dest: Value, left: Operand, right: Operand) -> IntCompInst {
        IntCompInst {
            op,
            dest,
            left,
            right,
        }
    }

    pub fn dest(&self) -> Value {
        self.dest
    }

    pub fn left(&self) -> Operand {
        self.left
    }

    pub fn right(&self) -> Operand {
        self.right
    }
}

impl fmt::Display for IntCompInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} = icmp {} {}, {}",
            self.dest,
            self.op,
            self.left,
            self.right
        )
    }
}

#[derive(Debug, Clone)]
pub struct FloatCompInst {
    op: CompOp,
    dest: Value,
    left: Operand,
    right: Operand,
}

impl FloatCompInst {
    pub fn new(op: CompOp, dest: Value, left: Operand, right: Operand) -> FloatCompInst {
        FloatCompInst {
            op,
            dest,
            left,
            right,
        }
    }

    pub fn dest(&self) -> Value {
        self.dest
    }

    pub fn left(&self) -> Operand {
        self.left
    }

    pub fn right(&self) -> Operand {
        self.right
    }
}

impl fmt::Display for FloatCompInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} = fcmp {} {}, {}",
            self.dest,
            self.op,
            self.left,
            self.right
        )
    }
}

#[derive(Debug, Clone)]
pub struct SelectInst {
    dest: Value,
    cond: Value,
    left: Operand,
    right: Operand,
}

impl SelectInst {
    pub fn new(dest: Value, cond: Value, left: Operand, right: Operand) -> SelectInst {
        SelectInst {
            dest,
            cond,
            left,
            right,
        }
    }

    pub fn dest(&self) -> Value {
        self.dest
    }

    pub fn cond(&self) -> Value {
        self.cond
    }

    pub fn left(&self) -> Operand {
        self.left
    }

    pub fn right(&self) -> Operand {
        self.right
    }
}

impl fmt::Display for SelectInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} = select {}, {}, {}",
            self.dest,
            self.cond,
            self.left,
            self.right,
        )
    }
}

#[derive(Debug, Clone)]
pub struct JumpInst {
    target: Target,
}

impl JumpInst {
    pub fn new(target: Target) -> JumpInst {
        JumpInst { target }
    }

    pub fn target(&self) -> &Target {
        &self.target
    }

    pub fn target_mut(&mut self) -> &mut Target {
        &mut self.target
    }
}

impl fmt::Display for JumpInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "jmp {}",
            self.target,
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BranchOp {
    AnyFalse,
    AnyTrue,
    AllFalse,
    AllTrue,
}

impl fmt::Display for BranchOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match *self {
            AnyFalse => "anyf",
            AnyTrue => "anyt",
            AllFalse => "allf",
            AllTrue => "allt",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Clone)]
pub struct BranchInst {
    op: BranchOp,
    cond: Value,
    target: Target,
}

impl BranchInst {
    pub fn new(op: BranchOp, cond: Value, target: Target) -> BranchInst {
        BranchInst { op, cond, target }
    }

    pub fn target(&self) -> &Target {
        &self.target
    }

    pub fn target_mut(&mut self) -> &mut Target {
        &mut self.target
    }
}

impl fmt::Display for BranchInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "br {} {} {}",
            self.op,
            self.cond,
            self.target,
        )
    }
}

#[derive(Debug, Clone)]
pub struct ReturnInst {}

impl ReturnInst {
    pub fn new() -> ReturnInst {
        ReturnInst {}
    }
}

impl fmt::Display for ReturnInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ret")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Inst {
    id: usize,
}

impl Key for Inst {
    fn new(id: usize) -> Self {
        Inst { id }
    }

    fn get(&self) -> usize {
        self.id
    }
}

impl fmt::Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

#[derive(Debug, Clone)]
pub enum InstData {
    IntConstInst(IntConstInst),
    FloatConstInst(FloatConstInst),
    BoolConstInst(BoolConstInst),
    IndexInst(IndexInst),
    CountInst(CountInst),
    UnaryInst(UnaryInst),
    BinaryInst(BinaryInst),
    IntCompInst(IntCompInst),
    FloatCompInst(FloatCompInst),
    SelectInst(SelectInst),
    JumpInst(JumpInst),
    BranchInst(BranchInst),
    ReturnInst(ReturnInst),
}

impl InstData {
    pub fn is_terminator(&self) -> bool {
        match *self {
            InstData::JumpInst(_) |
            InstData::ReturnInst(_) => true,
            _ => false,
        }
    }

    pub fn target(&self) -> Option<&Target> {
        match *self {
            InstData::JumpInst(ref inst) => Some(inst.target()),
            InstData::BranchInst(ref inst) => Some(inst.target()),
            _ => None,
        }
    }

    pub fn target_mut(&mut self) -> Option<&mut Target> {
        match *self {
            InstData::JumpInst(ref mut inst) => Some(inst.target_mut()),
            InstData::BranchInst(ref mut inst) => Some(inst.target_mut()),
            _ => None,
        }
    }
}

impl fmt::Display for InstData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            InstData::IntConstInst(ref inst) => write!(f, "{}", inst),
            InstData::FloatConstInst(ref inst) => write!(f, "{}", inst),
            InstData::BoolConstInst(ref inst) => write!(f, "{}", inst),
            InstData::IndexInst(ref inst) => write!(f, "{}", inst),
            InstData::CountInst(ref inst) => write!(f, "{}", inst),
            InstData::UnaryInst(ref inst) => write!(f, "{}", inst),
            InstData::BinaryInst(ref inst) => write!(f, "{}", inst),
            InstData::IntCompInst(ref inst) => write!(f, "{}", inst),
            InstData::FloatCompInst(ref inst) => write!(f, "{}", inst),
            InstData::SelectInst(ref inst) => write!(f, "{}", inst),
            InstData::JumpInst(ref inst) => write!(f, "{}", inst),
            InstData::BranchInst(ref inst) => write!(f, "{}", inst),
            InstData::ReturnInst(ref inst) => write!(f, "{}", inst),
        }
    }
}
