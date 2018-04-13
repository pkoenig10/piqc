use std::fmt;

use collections::*;
use ir::*;

#[derive(Debug, Clone, Copy)]
pub struct IntImmediate {
    value: i32,
}

impl IntImmediate {
    pub fn new(value: i32) -> IntImmediate {
        IntImmediate { value }
    }
}

impl fmt::Display for IntImmediate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
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

impl fmt::Display for FloatImmediate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
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

impl fmt::Display for BoolImmediate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value {
    id: usize,
}

impl Key for Value {
    fn new(id: usize) -> Self {
        Value { id }
    }

    fn get(&self) -> usize {
        self.id
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "%{}", self.id)
    }
}

#[derive(Debug)]
pub struct ValueValueData {
    type_: Type,
}

impl ValueValueData {
    pub fn new(type_: Type) -> ValueValueData {
        ValueValueData { type_ }
    }

    pub fn type_(&self) -> Type {
        self.type_
    }
}

#[derive(Debug)]
pub struct AliasValueData {
    value: Option<Value>,
}

impl AliasValueData {
    pub fn new(value: Option<Value>) -> AliasValueData {
        AliasValueData { value }
    }

    pub fn value(&self) -> Option<Value> {
        self.value
    }
}

#[derive(Debug)]
pub enum ValueData {
    Value(ValueValueData),
    Alias(AliasValueData),
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    IntImmediate(IntImmediate),
    FloatImmediate(FloatImmediate),
    BoolImmediate(BoolImmediate),
    Value(Value),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Operand::IntImmediate(ref int_immediate) => write!(f, "{}", int_immediate),
            Operand::FloatImmediate(ref float_immediate) => write!(f, "{}", float_immediate),
            Operand::BoolImmediate(ref bool_immediate) => write!(f, "{}", bool_immediate),
            Operand::Value(ref value) => write!(f, "{}", value),
        }
    }
}
