use enum_dispatch::enum_dispatch;
use enum_display_derive::Display;
use std::fmt;
use std::fmt::Display;

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

impl InstTrait for IntConstInst {}

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

impl InstTrait for FloatConstInst {}

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

impl InstTrait for BoolConstInst {}

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

impl InstTrait for ElementInst {}

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

impl InstTrait for CountInst {}

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

impl InstTrait for UnaryInst {
    fn replace_value(&mut self, old: Value, new: Value) {
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
}

impl InstTrait for BinaryInst {
    fn replace_value(&mut self, old: Value, new: Value) {
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
pub struct AllocInst {
    pub dest: Value,
    pub len: u8,
}

impl AllocInst {
    pub fn new(dest: Value, len: u8) -> AllocInst {
        AllocInst { dest, len }
    }
}

impl InstTrait for AllocInst {
    fn replace_value(&mut self, old: Value, new: Value) {
        replace_value(&mut self.dest, old, new);
    }
}

impl fmt::Display for AllocInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = alloc {}", self.dest, self.len)
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
}

impl InstTrait for FetchInst {
    fn replace_value(&mut self, old: Value, new: Value) {
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
pub struct ReadInst {
    pub dest: Value,
    pub index: Value,
}

impl ReadInst {
    pub fn new(dest: Value, index: Value) -> ReadInst {
        ReadInst { dest, index }
    }
}

impl InstTrait for ReadInst {
    fn replace_value(&mut self, old: Value, new: Value) {
        replace_value(&mut self.dest, old, new);
        replace_value(&mut self.index, old, new);
    }
}

impl fmt::Display for ReadInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = read {}", self.dest, self.index)
    }
}

#[derive(Debug, Clone)]
pub struct WriteInst {
    pub cond: Option<Value>,
    pub src: Value,
    pub index: Value,
}

impl WriteInst {
    pub fn new(cond: Option<Value>, src: Value, index: Value) -> WriteInst {
        WriteInst { cond, src, index }
    }
}

impl InstTrait for WriteInst {
    fn replace_value(&mut self, old: Value, new: Value) {
        if let Some(ref mut cond) = self.cond {
            replace_value(cond, old, new);
        }
        replace_value(&mut self.src, old, new);
        replace_value(&mut self.index, old, new);
    }
}

impl fmt::Display for WriteInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.cond {
            Some(cond) => write!(f, "write {}, {}, {}", cond, self.src, self.index),
            None => write!(f, "write _, {}, {}", self.src, self.index),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StoreInst {
    pub index: Value,
    pub addr: Value,
}

impl StoreInst {
    pub fn new(index: Value, addr: Value) -> StoreInst {
        StoreInst { index, addr }
    }
}

impl InstTrait for StoreInst {
    fn replace_value(&mut self, old: Value, new: Value) {
        replace_value(&mut self.index, old, new);
        replace_value(&mut self.addr, old, new);
    }
}

impl fmt::Display for StoreInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "store {}, {}", self.index, self.addr)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

impl fmt::Display for CmpOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match self {
            CmpOp::Eq => "eq",
            CmpOp::Ne => "ne",
            CmpOp::Lt => "lt",
            CmpOp::Gt => "gt",
            CmpOp::Le => "le",
            CmpOp::Ge => "ge",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Clone)]
pub struct IntCmpInst {
    pub op: CmpOp,
    pub dest: Value,
    pub left: Value,
    pub right: Value,
}

impl IntCmpInst {
    pub fn new(op: CmpOp, dest: Value, left: Value, right: Value) -> IntCmpInst {
        IntCmpInst {
            op,
            dest,
            left,
            right,
        }
    }
}

impl InstTrait for IntCmpInst {
    fn replace_value(&mut self, old: Value, new: Value) {
        replace_value(&mut self.dest, old, new);
        replace_value(&mut self.left, old, new);
        replace_value(&mut self.right, old, new);
    }
}

impl fmt::Display for IntCmpInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} = icmp {} {}, {}",
            self.dest, self.op, self.left, self.right
        )
    }
}

#[derive(Debug, Clone)]
pub struct FloatCmpInst {
    pub op: CmpOp,
    pub dest: Value,
    pub left: Value,
    pub right: Value,
}

impl FloatCmpInst {
    pub fn new(op: CmpOp, dest: Value, left: Value, right: Value) -> FloatCmpInst {
        FloatCmpInst {
            op,
            dest,
            left,
            right,
        }
    }
}

impl InstTrait for FloatCmpInst {
    fn replace_value(&mut self, old: Value, new: Value) {
        replace_value(&mut self.dest, old, new);
        replace_value(&mut self.left, old, new);
        replace_value(&mut self.right, old, new);
    }
}

impl fmt::Display for FloatCmpInst {
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
}

impl InstTrait for SelectInst {
    fn replace_value(&mut self, old: Value, new: Value) {
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
}

impl InstTrait for JumpInst {
    fn is_terminator(&self) -> bool {
        true
    }

    fn target(&self) -> Option<&Target> {
        Some(&self.target)
    }

    fn target_mut(&mut self) -> Option<&mut Target> {
        Some(&mut self.target)
    }

    fn replace_value(&mut self, old: Value, new: Value) {
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
}

impl InstTrait for BranchInst {
    fn target(&self) -> Option<&Target> {
        Some(&self.target)
    }

    fn target_mut(&mut self) -> Option<&mut Target> {
        Some(&mut self.target)
    }

    fn replace_value(&mut self, old: Value, new: Value) {
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

impl InstTrait for ReturnInst {
    fn is_terminator(&self) -> bool {
        true
    }
}

impl fmt::Display for ReturnInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ret")
    }
}

#[enum_dispatch]
#[derive(Debug, Display, Clone)]
pub enum InstData {
    IntConst(IntConstInst),
    FloatConst(FloatConstInst),
    BoolConst(BoolConstInst),
    Element(ElementInst),
    Count(CountInst),
    Unary(UnaryInst),
    Binary(BinaryInst),
    Alloc(AllocInst),
    Fetch(FetchInst),
    Read(ReadInst),
    Write(WriteInst),
    Store(StoreInst),
    IntCmp(IntCmpInst),
    FloatCmp(FloatCmpInst),
    Select(SelectInst),
    Jump(JumpInst),
    Branch(BranchInst),
    Return(ReturnInst),
}

#[enum_dispatch(InstData)]
pub trait InstTrait: fmt::Display {
    fn is_terminator(&self) -> bool {
        false
    }

    fn target(&self) -> Option<&Target> {
        None
    }

    fn target_mut(&mut self) -> Option<&mut Target> {
        None
    }

    fn replace_value(&mut self, _old: Value, _new: Value) {}
}

fn replace_value(value: &mut Value, old: Value, new: Value) {
    if *value == old {
        *value = new;
    }
}
