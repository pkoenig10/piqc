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
    Fadd,
    Sub,
    Fsub,
    Fmul,
    Asr,
    Shl,
    And,
    Or,
    Xor,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match *self {
            Add => "add",
            Fadd => "fadd",
            Sub => "sub",
            Fsub => "fsub",
            Fmul => "fmul",
            Asr => "asr",
            Shl => "shl",
            And => "and",
            Or => "or",
            Xor => "xor",
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
pub struct InstId {
    id: usize,
}

impl Key for InstId {
    fn new(id: usize) -> Self {
        InstId { id }
    }

    fn get(&self) -> usize {
        self.id
    }
}

impl fmt::Display for InstId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

#[derive(Debug)]
pub enum Inst {
    IntConstInst(IntConstInst),
    FloatConstInst(FloatConstInst),
    BoolConstInst(BoolConstInst),
    UnaryInst(UnaryInst),
    BinaryInst(BinaryInst),
    IntCompInst(IntCompInst),
    FloatCompInst(FloatCompInst),
    JumpInst(JumpInst),
    BranchInst(BranchInst),
    ReturnInst(ReturnInst),
}

impl Inst {
    pub fn is_terminator(&self) -> bool {
        match *self {
            Inst::IntConstInst(_) |
            Inst::FloatConstInst(_) |
            Inst::BoolConstInst(_) |
            Inst::UnaryInst(_) |
            Inst::BinaryInst(_) |
            Inst::IntCompInst(_) |
            Inst::FloatCompInst(_) => false,
            Inst::JumpInst(_) |
            Inst::BranchInst(_) |
            Inst::ReturnInst(_) => true,
        }
    }

    pub fn get_target_mut(&mut self, block_id: BlockId) -> Option<&mut Target> {
        match *self {
            Inst::IntConstInst(_) |
            Inst::FloatConstInst(_) |
            Inst::BoolConstInst(_) |
            Inst::UnaryInst(_) |
            Inst::BinaryInst(_) |
            Inst::IntCompInst(_) |
            Inst::FloatCompInst(_) |
            Inst::ReturnInst(_) => None,
            Inst::JumpInst(ref mut inst) => {
                if inst.target().block() == block_id {
                    Some(inst.target_mut())
                } else {
                    None
                }
            }
            Inst::BranchInst(ref mut inst) => {
                if inst.true_target().block() == block_id {
                    Some(inst.true_target_mut())
                } else if inst.false_target().block() == block_id {
                    Some(inst.false_target_mut())
                } else {
                    None
                }
            }
        }
    }
}

impl fmt::Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Inst::IntConstInst(ref inst) => write!(f, "{}", inst),
            Inst::FloatConstInst(ref inst) => write!(f, "{}", inst),
            Inst::BoolConstInst(ref inst) => write!(f, "{}", inst),
            Inst::UnaryInst(ref inst) => write!(f, "{}", inst),
            Inst::BinaryInst(ref inst) => write!(f, "{}", inst),
            Inst::IntCompInst(ref inst) => write!(f, "{}", inst),
            Inst::FloatCompInst(ref inst) => write!(f, "{}", inst),
            Inst::JumpInst(ref inst) => write!(f, "{}", inst),
            Inst::BranchInst(ref inst) => write!(f, "{}", inst),
            Inst::ReturnInst(ref inst) => write!(f, "{}", inst),
        }
    }
}
