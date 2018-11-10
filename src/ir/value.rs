use ir::*;

#[derive(Debug)]
pub struct ValueData {
    type_: Type,
}

impl ValueData {
    pub fn new(type_: Type) -> ValueData {
        ValueData { type_ }
    }

    pub fn type_(&self) -> Type {
        self.type_
    }
}
