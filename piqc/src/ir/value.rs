use crate::ir::*;

#[derive(Debug)]
pub struct ValueData {
    pub ty: Type,
}

impl ValueData {
    pub fn new(ty: Type) -> ValueData {
        ValueData { ty }
    }
}
