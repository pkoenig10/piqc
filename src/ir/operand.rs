#[derive(Debug, Clone, Copy)]
pub struct IntConstant {
    value: i32,
}

impl IntConstant {
    pub fn new(value: i32) -> Operand {
        Operand::IntConstant(IntConstant { value })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FloatConstant {
    value: f32,
}


impl FloatConstant {
    pub fn new(value: f32) -> Operand {
        Operand::FloatConstant(FloatConstant { value })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BoolConstant {
    value: bool,
}

impl BoolConstant {
    pub fn new(value: bool) -> Operand {
        Operand::BoolConstant(BoolConstant { value })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Register {
    id: u32,
}

impl Register {
    pub fn new(id: u32) -> Register {
        Register { id }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    IntConstant(IntConstant),
    FloatConstant(FloatConstant),
    BoolConstant(BoolConstant),
    Register(Register),
}
