use collections::*;

#[derive(Debug, Clone, Copy)]
pub struct IntImmediate {
    value: i32,
}

impl IntImmediate {
    pub fn new(value: i32) -> IntImmediate {
        IntImmediate { value }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FloatImmediate {
    value: f32,
}


impl FloatImmediate {
    pub fn new(value: f32) -> FloatImmediate {
        FloatImmediate { value }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BoolImmediate {
    value: bool,
}

impl BoolImmediate {
    pub fn new(value: bool) -> BoolImmediate {
        BoolImmediate { value }
    }
}

pub type Value = Id;

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    IntImmediate(IntImmediate),
    FloatImmediate(FloatImmediate),
    BoolImmediate(BoolImmediate),
    Value(Value),
}
