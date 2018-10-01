use std::fmt;

use collections::*;
use ir::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ebb {
    id: usize,
}

impl Key for Ebb {
    fn new(id: usize) -> Self {
        Ebb { id }
    }

    fn get(&self) -> usize {
        self.id
    }
}

impl fmt::Display for Ebb {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ebb{}", self.id)
    }
}

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
