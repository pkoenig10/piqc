use std::fmt;

use crate::ir::*;
use crate::util::Map;

#[derive(Debug)]
struct EbbNode {
    params: Vec<Value>,
    prev_ebb: Option<Ebb>,
    next_ebb: Option<Ebb>,
    first_inst: Option<Inst>,
    last_inst: Option<Inst>,
}

impl EbbNode {
    pub fn new() -> EbbNode {
        EbbNode {
            params: Vec::new(),
            prev_ebb: None,
            next_ebb: None,
            first_inst: None,
            last_inst: None,
        }
    }
}

#[derive(Debug)]
struct InstNode {
    data: InstData,
    ebb: Option<Ebb>,
    prev_inst: Option<Inst>,
    next_inst: Option<Inst>,
}

impl InstNode {
    pub fn new(data: InstData) -> InstNode {
        InstNode {
            data,
            ebb: None,
            prev_inst: None,
            next_inst: None,
        }
    }
}

#[derive(Debug)]
pub struct Func {
    params: Vec<Type>,
    ebbs: Map<Ebb, EbbNode>,
    insts: Map<Inst, InstNode>,
    values: Map<Value, ValueData>,
    first_ebb: Option<Ebb>,
    last_ebb: Option<Ebb>,
}

impl Func {
    pub fn new() -> Func {
        Func {
            params: Vec::new(),
            values: Map::new(),
            ebbs: Map::new(),
            insts: Map::new(),
            first_ebb: None,
            last_ebb: None,
        }
    }

    pub fn push_param(&mut self, type_: Type) {
        self.params.push(type_);
    }

    pub fn create_value(&mut self, data: ValueData) -> Value {
        self.values.create(data)
    }

    pub fn value(&self, value: Value) -> &ValueData {
        &self.values[value]
    }

    pub fn create_ebb(&mut self) -> Ebb {
        self.ebbs.create(EbbNode::new())
    }

    pub fn ebbs(&self) -> Ebbs {
        Ebbs::new(self)
    }

    pub fn ebb_params(&self, ebb: Ebb) -> &[Value] {
        &self.ebbs[ebb].params
    }

    pub fn push_ebb_param(&mut self, ebb: Ebb, value: Value) {
        self.ebbs[ebb].params.push(value);
    }

    pub fn swap_remove_ebb_param(&mut self, ebb: Ebb, value: Value) -> usize {
        let index = self.ebbs[ebb]
            .params
            .iter()
            .position(|&param| param == value)
            .unwrap();
        self.ebbs[ebb].params.swap_remove(index);
        index
    }

    pub fn push_ebb(&mut self, ebb: Ebb) {
        match self.last_ebb {
            Some(last_ebb) => {
                self.ebbs[last_ebb].next_ebb = Some(ebb);
                self.ebbs[ebb].prev_ebb = Some(last_ebb);
            }
            None => {
                self.first_ebb = Some(ebb);
            }
        }
        self.last_ebb = Some(ebb);
    }

    pub fn remove_ebb(&mut self, ebb: Ebb) {
        let (prev_ebb, next_ebb) = {
            let ebb_node = &self.ebbs[ebb];
            (ebb_node.prev_ebb, ebb_node.next_ebb)
        };

        match prev_ebb {
            Some(prev_ebb) => {
                self.ebbs[prev_ebb].next_ebb = next_ebb;
            }
            None => {
                self.first_ebb = next_ebb;
            }
        }

        match next_ebb {
            Some(next_ebb) => {
                self.ebbs[next_ebb].prev_ebb = prev_ebb;
            }
            None => {
                self.last_ebb = prev_ebb;
            }
        }
    }

    pub fn create_inst(&mut self, data: InstData) -> Inst {
        self.insts.create(InstNode::new(data))
    }

    pub fn inst(&self, inst: Inst) -> &InstData {
        &self.insts[inst].data
    }

    pub fn inst_mut(&mut self, inst: Inst) -> &mut InstData {
        &mut self.insts[inst].data
    }

    pub fn insts(&self, ebb: Ebb) -> Insts {
        Insts::new(self, ebb)
    }

    pub fn first_inst(&self, ebb: Ebb) -> Option<Inst> {
        self.ebbs[ebb].first_inst
    }

    pub fn last_inst(&self, ebb: Ebb) -> Option<Inst> {
        self.ebbs[ebb].last_inst
    }

    pub fn push_inst(&mut self, ebb: Ebb, inst: Inst) {
        self.insts[inst].ebb = Some(ebb);

        let ebb_node = &mut self.ebbs[ebb];
        match ebb_node.last_inst {
            Some(last_inst) => {
                self.insts[last_inst].next_inst = Some(inst);
                self.insts[inst].prev_inst = Some(last_inst);
            }
            None => {
                ebb_node.first_inst = Some(inst);
            }
        }
        ebb_node.last_inst = Some(inst);
    }

    pub fn remove_inst(&mut self, inst: Inst) {
        let (ebb, prev_inst, next_inst) = {
            let inst_node = &self.insts[inst];
            (inst_node.ebb, inst_node.prev_inst, inst_node.next_inst)
        };

        if let Some(ebb) = ebb {
            match prev_inst {
                Some(prev_inst) => {
                    self.insts[prev_inst].next_inst = next_inst;
                }
                None => {
                    self.ebbs[ebb].first_inst = next_inst;
                }
            }

            match next_inst {
                Some(next_inst) => {
                    self.insts[next_inst].prev_inst = prev_inst;
                }
                None => {
                    self.ebbs[ebb].last_inst = prev_inst;
                }
            }
        }
    }
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "func({}):", DisplayList::new(&self.params))?;
        for ebb in self.ebbs() {
            writeln!(f)?;
            writeln!(f, "{}({}):", ebb, DisplayList::new(self.ebb_params(ebb)))?;
            for inst in self.insts(ebb) {
                let inst_node = self.inst(inst);
                writeln!(f, "    {}", inst_node)?;
            }
        }
        Ok(())
    }
}

pub struct Ebbs<'a> {
    func: &'a Func,
    next: Option<Ebb>,
}

impl<'a> Ebbs<'a> {
    fn new(func: &Func) -> Ebbs {
        Ebbs {
            func,
            next: func.first_ebb,
        }
    }
}

impl<'a> Iterator for Ebbs<'a> {
    type Item = Ebb;

    fn next(&mut self) -> Option<Ebb> {
        match self.next {
            Some(ebb) => {
                self.next = self.func.ebbs[ebb].next_ebb;
                Some(ebb)
            }
            None => None,
        }
    }
}

pub struct Insts<'a> {
    func: &'a Func,
    next: Option<Inst>,
}

impl<'a> Insts<'a> {
    fn new(func: &Func, ebb: Ebb) -> Insts {
        Insts {
            func,
            next: func.ebbs[ebb].first_inst,
        }
    }
}

impl<'a> Iterator for Insts<'a> {
    type Item = Inst;

    fn next(&mut self) -> Option<Inst> {
        match self.next {
            Some(inst) => {
                self.next = self.func.insts[inst].next_inst;
                Some(inst)
            }
            None => None,
        }
    }
}
