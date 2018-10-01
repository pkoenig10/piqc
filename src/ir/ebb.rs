use std::fmt;

use ir::*;

id!(Ebb, "ebb");

#[derive(Debug)]
pub struct EbbData {
    params: Vec<Value>,
}

impl EbbData {
    pub fn new() -> EbbData {
        EbbData { params: Vec::new() }
    }

    pub fn params(&self) -> &[Value] {
        &self.params
    }

    pub fn push_param(&mut self, value: Value) {
        self.params.push(value);
    }

    pub fn swap_remove_param(&mut self, value: Value) -> usize {
        let index = self
            .params
            .iter()
            .position(|&param| param == value)
            .unwrap();
        self.params.swap_remove(index);
        index
    }
}
