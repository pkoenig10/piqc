use std::fmt;

use ir::*;

#[derive(Debug, Clone, Copy)]
pub struct IntConstInst {
    dest: Value,
    immediate: IntImmediate,
}

impl IntConstInst {
    pub fn new(dest: Value, immediate: IntImmediate) -> Inst {
        Inst::IntConstInst(IntConstInst { dest, immediate })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FloatConstInst {
    dest: Value,
    immediate: FloatImmediate,
}

impl FloatConstInst {
    pub fn new(dest: Value, immediate: FloatImmediate) -> Inst {
        Inst::FloatConstInst(FloatConstInst { dest, immediate })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BoolConstInst {
    dest: Value,
    immediate: BoolImmediate,
}

impl BoolConstInst {
    pub fn new(dest: Value, immediate: BoolImmediate) -> Inst {
        Inst::BoolConstInst(BoolConstInst { dest, immediate })
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
    pub fn new(op: UnaryOp, dest: Value, src: Operand) -> Inst {
        Inst::UnaryInst(UnaryInst { op, dest, src })
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
    pub fn new(op: BinaryOp, dest: Value, left: Operand, right: Operand) -> Inst {
        Inst::BinaryInst(BinaryInst {
            op,
            dest,
            left,
            right,
        })
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
    pub fn new(op: CompOp, dest: Value, left: Operand, right: Operand) -> Inst {
        Inst::IntCompInst(IntCompInst {
            op,
            dest,
            left,
            right,
        })
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
    pub fn new(op: CompOp, dest: Value, left: Operand, right: Operand) -> Inst {
        Inst::FloatCompInst(FloatCompInst {
            op,
            dest,
            left,
            right,
        })
    }
}

#[derive(Debug)]
pub struct ReturnInst {}

impl ReturnInst {
    pub fn new() -> Inst {
        Inst::ReturnInst(ReturnInst {})
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
    ReturnInst(ReturnInst),
}
