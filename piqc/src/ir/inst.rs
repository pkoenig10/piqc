use std::fmt;

use crate::ir::*;

#[derive(Debug, Clone)]
pub struct Target {
    ebb: Ebb,
    args: Vec<Value>,
}

impl Target {
    pub fn new(ebb: Ebb) -> Target {
        Target {
            ebb,
            args: Vec::new(),
        }
    }

    pub fn ebb(&self) -> Ebb {
        self.ebb
    }

    pub fn args(&self) -> &[Value] {
        &self.args
    }

    pub fn push_arg(&mut self, value: Value) {
        self.args.push(value);
    }

    pub fn swap_remove_arg(&mut self, index: usize) -> Value {
        self.args.swap_remove(index)
    }

    pub fn replace_arg(&mut self, old: Value, new: Value) {
        for arg in self.args.iter_mut() {
            replace_value(arg, old, new);
        }
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.ebb, DisplayList::new(&self.args))
    }
}

#[derive(Debug, Clone)]
pub struct IntConstInst {
    pub dest: Value,
    pub value: i32,
}

impl IntConstInst {
    pub fn new(dest: Value, value: i32) -> IntConstInst {
        IntConstInst { dest, value }
    }
}

impl fmt::Display for IntConstInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = iconst {}", self.dest, self.value)
    }
}

#[derive(Debug, Clone)]
pub struct FloatConstInst {
    pub dest: Value,
    pub value: f32,
}

impl FloatConstInst {
    pub fn new(dest: Value, value: f32) -> FloatConstInst {
        FloatConstInst { dest, value }
    }
}

impl fmt::Display for FloatConstInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = fconst {}", self.dest, self.value)
    }
}

#[derive(Debug, Clone)]
pub struct BoolConstInst {
    pub dest: Value,
    pub value: bool,
}

impl BoolConstInst {
    pub fn new(dest: Value, value: bool) -> BoolConstInst {
        BoolConstInst { dest, value }
    }
}

impl fmt::Display for BoolConstInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = bconst {}", self.dest, self.value)
    }
}

#[derive(Debug, Clone)]
pub struct ElementInst {
    pub dest: Value,
}

impl ElementInst {
    pub fn new(dest: Value) -> ElementInst {
        ElementInst { dest }
    }
}

impl fmt::Display for ElementInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = element", self.dest)
    }
}

#[derive(Debug, Clone)]
pub struct CountInst {
    pub dest: Value,
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
        let op = match self {
            UnaryOp::Not => "not",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Clone)]
pub struct UnaryInst {
    pub op: UnaryOp,
    pub dest: Value,
    pub src: Value,
}

impl UnaryInst {
    pub fn new(op: UnaryOp, dest: Value, src: Value) -> UnaryInst {
        UnaryInst { op, dest, src }
    }

    pub fn replace_value(&mut self, old: Value, new: Value) {
        replace_value(&mut self.dest, old, new);
        replace_value(&mut self.src, old, new);
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
        let op = match self {
            BinaryOp::Add => "add",
            BinaryOp::Sub => "sub",
            BinaryOp::Asr => "asr",
            BinaryOp::Shl => "shl",
            BinaryOp::Min => "min",
            BinaryOp::Max => "max",
            BinaryOp::And => "and",
            BinaryOp::Or => "or",
            BinaryOp::Xor => "xor",
            BinaryOp::Fadd => "fadd",
            BinaryOp::Fsub => "fsub",
            BinaryOp::Fmul => "fmul",
            BinaryOp::Fmin => "fmin",
            BinaryOp::Fmax => "fmax",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Clone)]
pub struct BinaryInst {
    pub op: BinaryOp,
    pub dest: Value,
    pub left: Value,
    pub right: Value,
}

impl BinaryInst {
    pub fn new(op: BinaryOp, dest: Value, left: Value, right: Value) -> BinaryInst {
        BinaryInst {
            op,
            dest,
            left,
            right,
        }
    }

    pub fn replace_value(&mut self, old: Value, new: Value) {
        replace_value(&mut self.dest, old, new);
        replace_value(&mut self.left, old, new);
        replace_value(&mut self.right, old, new);
    }
}

impl fmt::Display for BinaryInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} = {} {}, {}",
            self.dest, self.op, self.left, self.right
        )
    }
}

#[derive(Debug, Clone)]
pub struct FetchInst {
    pub dest: Value,
    pub addr: Value,
}

impl FetchInst {
    pub fn new(dest: Value, addr: Value) -> FetchInst {
        FetchInst { dest, addr }
    }

    pub fn replace_value(&mut self, old: Value, new: Value) {
        replace_value(&mut self.dest, old, new);
        replace_value(&mut self.addr, old, new);
    }
}

impl fmt::Display for FetchInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = fetch {}", self.dest, self.addr)
    }
}

#[derive(Debug, Clone)]
pub struct StoreInst {
    pub src: Value,
    pub addr: Value,
}

impl StoreInst {
    pub fn new(src: Value, addr: Value) -> StoreInst {
        StoreInst { src, addr }
    }

    pub fn replace_value(&mut self, old: Value, new: Value) {
        replace_value(&mut self.src, old, new);
        replace_value(&mut self.addr, old, new);
    }
}

impl fmt::Display for StoreInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "store {}, {}", self.src, self.addr)
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
        let op = match self {
            CompOp::Eq => "eq",
            CompOp::Ne => "ne",
            CompOp::Lt => "lt",
            CompOp::Gt => "gt",
            CompOp::Le => "le",
            CompOp::Ge => "ge",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Clone)]
pub struct IntCompInst {
    pub op: CompOp,
    pub dest: Value,
    pub left: Value,
    pub right: Value,
}

impl IntCompInst {
    pub fn new(op: CompOp, dest: Value, left: Value, right: Value) -> IntCompInst {
        IntCompInst {
            op,
            dest,
            left,
            right,
        }
    }

    pub fn replace_value(&mut self, old: Value, new: Value) {
        replace_value(&mut self.dest, old, new);
        replace_value(&mut self.left, old, new);
        replace_value(&mut self.right, old, new);
    }
}

impl fmt::Display for IntCompInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} = icmp {} {}, {}",
            self.dest, self.op, self.left, self.right
        )
    }
}

#[derive(Debug, Clone)]
pub struct FloatCompInst {
    pub op: CompOp,
    pub dest: Value,
    pub left: Value,
    pub right: Value,
}

impl FloatCompInst {
    pub fn new(op: CompOp, dest: Value, left: Value, right: Value) -> FloatCompInst {
        FloatCompInst {
            op,
            dest,
            left,
            right,
        }
    }

    pub fn replace_value(&mut self, old: Value, new: Value) {
        replace_value(&mut self.dest, old, new);
        replace_value(&mut self.left, old, new);
        replace_value(&mut self.right, old, new);
    }
}

impl fmt::Display for FloatCompInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} = fcmp {} {}, {}",
            self.dest, self.op, self.left, self.right
        )
    }
}

#[derive(Debug, Clone)]
pub struct SelectInst {
    pub dest: Value,
    pub cond: Value,
    pub left: Value,
    pub right: Value,
}

impl SelectInst {
    pub fn new(dest: Value, cond: Value, left: Value, right: Value) -> SelectInst {
        SelectInst {
            dest,
            cond,
            left,
            right,
        }
    }

    pub fn replace_value(&mut self, old: Value, new: Value) {
        replace_value(&mut self.dest, old, new);
        replace_value(&mut self.cond, old, new);
        replace_value(&mut self.left, old, new);
        replace_value(&mut self.right, old, new);
    }
}

impl fmt::Display for SelectInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} = select {}, {}, {}",
            self.dest, self.cond, self.left, self.right,
        )
    }
}

#[derive(Debug, Clone)]
pub struct JumpInst {
    pub target: Target,
}

impl JumpInst {
    pub fn new(target: Target) -> JumpInst {
        JumpInst { target }
    }

    pub fn replace_value(&mut self, old: Value, new: Value) {
        self.target.replace_arg(old, new);
    }
}

impl fmt::Display for JumpInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "jmp {}", self.target,)
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
        let op = match self {
            BranchOp::AnyFalse => "anyf",
            BranchOp::AnyTrue => "anyt",
            BranchOp::AllFalse => "allf",
            BranchOp::AllTrue => "allt",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Clone)]
pub struct BranchInst {
    pub op: BranchOp,
    pub cond: Value,
    pub target: Target,
}

impl BranchInst {
    pub fn new(op: BranchOp, cond: Value, target: Target) -> BranchInst {
        BranchInst { op, cond, target }
    }

    pub fn replace_value(&mut self, old: Value, new: Value) {
        self.target.replace_arg(old, new);
    }
}

impl fmt::Display for BranchInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "br {} {} {}", self.op, self.cond, self.target,)
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

#[derive(Debug, Clone)]
pub enum InstData {
    IntConst(IntConstInst),
    FloatConst(FloatConstInst),
    BoolConst(BoolConstInst),
    Element(ElementInst),
    Count(CountInst),
    Unary(UnaryInst),
    Binary(BinaryInst),
    Fetch(FetchInst),
    Store(StoreInst),
    IntComp(IntCompInst),
    FloatComp(FloatCompInst),
    Select(SelectInst),
    Jump(JumpInst),
    Branch(BranchInst),
    Return(ReturnInst),
}

impl InstData {
    pub fn is_terminator(&self) -> bool {
        match self {
            InstData::Jump(_) | InstData::Return(_) => true,
            _ => false,
        }
    }

    pub fn target(&self) -> Option<&Target> {
        match self {
            InstData::Jump(ref inst) => Some(&inst.target),
            InstData::Branch(ref inst) => Some(&inst.target),
            _ => None,
        }
    }

    pub fn target_mut(&mut self) -> Option<&mut Target> {
        match self {
            InstData::Jump(ref mut inst) => Some(&mut inst.target),
            InstData::Branch(ref mut inst) => Some(&mut inst.target),
            _ => None,
        }
    }

    pub fn replace_value(&mut self, old: Value, new: Value) {
        match self {
            InstData::IntConst(_) => {}
            InstData::FloatConst(_) => {}
            InstData::BoolConst(_) => {}
            InstData::Element(_) => {}
            InstData::Count(_) => {}
            InstData::Unary(ref mut inst) => inst.replace_value(old, new),
            InstData::Binary(ref mut inst) => inst.replace_value(old, new),
            InstData::Fetch(ref mut inst) => inst.replace_value(old, new),
            InstData::Store(ref mut inst) => inst.replace_value(old, new),
            InstData::IntComp(ref mut inst) => inst.replace_value(old, new),
            InstData::FloatComp(ref mut inst) => inst.replace_value(old, new),
            InstData::Select(ref mut inst) => inst.replace_value(old, new),
            InstData::Jump(ref mut inst) => inst.replace_value(old, new),
            InstData::Branch(ref mut inst) => inst.replace_value(old, new),
            InstData::Return(_) => {}
        }
    }
}

impl fmt::Display for InstData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InstData::IntConst(ref inst) => write!(f, "{}", inst),
            InstData::FloatConst(ref inst) => write!(f, "{}", inst),
            InstData::BoolConst(ref inst) => write!(f, "{}", inst),
            InstData::Element(ref inst) => write!(f, "{}", inst),
            InstData::Count(ref inst) => write!(f, "{}", inst),
            InstData::Unary(ref inst) => write!(f, "{}", inst),
            InstData::Binary(ref inst) => write!(f, "{}", inst),
            InstData::Fetch(ref inst) => write!(f, "{}", inst),
            InstData::Store(ref inst) => write!(f, "{}", inst),
            InstData::IntComp(ref inst) => write!(f, "{}", inst),
            InstData::FloatComp(ref inst) => write!(f, "{}", inst),
            InstData::Select(ref inst) => write!(f, "{}", inst),
            InstData::Jump(ref inst) => write!(f, "{}", inst),
            InstData::Branch(ref inst) => write!(f, "{}", inst),
            InstData::Return(ref inst) => write!(f, "{}", inst),
        }
    }
}

fn replace_value(value: &mut Value, old: Value, new: Value) {
    if *value == old {
        *value = new;
    }
}
