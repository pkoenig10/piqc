use crate::ir::*;

#[derive(Debug)]
pub struct ValueData {
    ty: Type,
}

impl ValueData {
    pub fn new(ty: Type) -> ValueData {
        ValueData { ty }
    }

    pub fn ty(&self) -> Type {
        self.ty
    }
}
