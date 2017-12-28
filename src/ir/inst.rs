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

#[derive(Debug)]
pub struct UnaryInst {
    op: UnaryOp,
    dest: Value,
    src: Operand,
}

impl UnaryInst {
    pub fn new(op: UnaryOp, dest: Value, src: Operand) -> UnaryInst {
        UnaryInst { op, dest, src }
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

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct BranchInst {
    cond: Value,
    true_target: Target,
    false_target: Target,
}

impl BranchInst {
    pub fn new(cond: Value, true_target: Target, false_target: Target) -> BranchInst {
        BranchInst {
            cond,
            true_target,
            false_target,
        }
    }

    pub fn true_target(&self) -> &Target {
        &self.true_target
    }

    pub fn true_target_mut(&mut self) -> &mut Target {
        &mut self.true_target
    }

    pub fn false_target(&self) -> &Target {
        &self.false_target
    }

    pub fn false_target_mut(&mut self) -> &mut Target {
        &mut self.false_target
    }
}

impl fmt::Display for BranchInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "br {} {}, {}",
            self.cond,
            self.true_target,
            self.false_target
        )
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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
    JumpInst(JumpInst),
    BranchInst(BranchInst),
    ReturnInst(ReturnInst),
}

impl InstData {
    pub fn is_terminator(&self) -> bool {
        match *self {
            InstData::IntConstInst(_) |
            InstData::FloatConstInst(_) |
            InstData::BoolConstInst(_) |
            InstData::IndexInst(_) |
            InstData::CountInst(_) |
            InstData::UnaryInst(_) |
            InstData::BinaryInst(_) |
            InstData::IntCompInst(_) |
            InstData::FloatCompInst(_) => false,
            InstData::JumpInst(_) |
            InstData::BranchInst(_) |
            InstData::ReturnInst(_) => true,
        }
    }

    pub fn get_target_mut(&mut self, block: Block) -> &mut Target {
        let target = match *self {
            InstData::IntConstInst(_) |
            InstData::FloatConstInst(_) |
            InstData::BoolConstInst(_) |
            InstData::IndexInst(_) |
            InstData::CountInst(_) |
            InstData::UnaryInst(_) |
            InstData::BinaryInst(_) |
            InstData::IntCompInst(_) |
            InstData::FloatCompInst(_) |
            InstData::ReturnInst(_) => None,
            InstData::JumpInst(ref mut inst) => {
                if inst.target().block() == block {
                    Some(inst.target_mut())
                } else {
                    None
                }
            }
            InstData::BranchInst(ref mut inst) => {
                if inst.true_target().block() == block {
                    Some(inst.true_target_mut())
                } else if inst.false_target().block() == block {
                    Some(inst.false_target_mut())
                } else {
                    None
                }
            }
        };
        target.unwrap()
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
            InstData::JumpInst(ref inst) => write!(f, "{}", inst),
            InstData::BranchInst(ref inst) => write!(f, "{}", inst),
            InstData::ReturnInst(ref inst) => write!(f, "{}", inst),
        }
    }
}
