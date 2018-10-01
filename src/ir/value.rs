use std::fmt;

use ir::*;

id!(Value, "%");

#[derive(Debug)]
pub struct InstValue {
    type_: Type,
}

impl InstValue {
    pub fn new(type_: Type) -> InstValue {
        InstValue { type_ }
    }

    pub fn type_(&self) -> Type {
        self.type_
    }
}

#[derive(Debug)]
pub struct ParamValue {
    type_: Type,
}

impl ParamValue {
    pub fn new(type_: Type) -> ParamValue {
        ParamValue { type_ }
    }

    pub fn type_(&self) -> Type {
        self.type_
    }
}

#[derive(Debug)]
pub enum ValueData {
    Inst(InstValue),
    Param(ParamValue),
}
