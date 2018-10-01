use std::fmt;

use ir::*;

#[derive(Debug)]
struct EbbNode {
    data: EbbData,
    prev_ebb: Option<Ebb>,
    next_ebb: Option<Ebb>,
    first_inst: Option<Inst>,
    last_inst: Option<Inst>,
}

impl EbbNode {
    pub fn new(data: EbbData) -> EbbNode {
        EbbNode {
            data,
            prev_ebb: None,
            next_ebb: None,
            first_inst: None,
            last_inst: None,
        }
    }

    pub fn data(&self) -> &EbbData {
        &self.data
    }

    pub fn data_mut(&mut self) -> &mut EbbData {
        &mut self.data
    }

    pub fn prev_ebb(&self) -> Option<Ebb> {
        self.prev_ebb
    }

    pub fn set_prev_ebb(&mut self, prev_ebb: Ebb) {
        self.prev_ebb = Some(prev_ebb);
    }

    pub fn next_ebb(&self) -> Option<Ebb> {
        self.next_ebb
    }

    pub fn set_next_ebb(&mut self, next_ebb: Ebb) {
        self.next_ebb = Some(next_ebb);
    }

    pub fn first_inst(&self) -> Option<Inst> {
        self.first_inst
    }

    pub fn set_first_inst(&mut self, first_inst: Option<Inst>) {
        self.first_inst = first_inst;
    }

    pub fn last_inst(&self) -> Option<Inst> {
        self.last_inst
    }

    pub fn set_last_inst(&mut self, last_inst: Option<Inst>) {
        self.last_inst = last_inst;
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

    pub fn data(&self) -> &InstData {
        &self.data
    }

    pub fn data_mut(&mut self) -> &mut InstData {
        &mut self.data
    }

    pub fn ebb(&self) -> Ebb {
        self.ebb.unwrap()
    }

    pub fn set_ebb(&mut self, ebb: Ebb) {
        self.ebb = Some(ebb)
    }

    pub fn prev_inst(&self) -> Option<Inst> {
        self.prev_inst
    }

    pub fn set_prev_inst(&mut self, prev_inst: Option<Inst>) {
        self.prev_inst = prev_inst;
    }

    pub fn next_inst(&self) -> Option<Inst> {
        self.next_inst
    }

    pub fn set_next_inst(&mut self, next_inst: Option<Inst>) {
        self.next_inst = next_inst;
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

    pub fn create_ebb(&mut self) -> Ebb {
        self.ebbs.create(EbbNode::new(EbbData::new()))
    }

    pub fn create_inst(&mut self, data: InstData) -> Inst {
        self.insts.create(InstNode::new(data))
    }

    pub fn create_value(&mut self, data: ValueData) -> Value {
        self.values.create(data)
    }

    pub fn ebb(&self, ebb: Ebb) -> &EbbData {
        self.ebbs.get(ebb).data()
    }

    pub fn ebb_mut(&mut self, ebb: Ebb) -> &mut EbbData {
        self.ebbs.get_mut(ebb).data_mut()
    }

    pub fn last_inst(&self, ebb: Ebb) -> Inst {
        self.ebbs.get(ebb).last_inst().unwrap()
    }

    pub fn ebbs(&self) -> EbbIterator {
        EbbIterator::new(self)
    }

    pub fn inst(&self, inst: Inst) -> &InstData {
        self.insts.get(inst).data()
    }

    pub fn inst_mut(&mut self, inst: Inst) -> &mut InstData {
        self.insts.get_mut(inst).data_mut()
    }

    pub fn insts(&self, ebb: Ebb) -> InstIterator {
        InstIterator::new(self, ebb)
    }

    pub fn value(&self, value: Value) -> &ValueData {
        self.values.get(value)
    }

    pub fn push_param(&mut self, type_: Type) {
        self.params.push(type_);
    }

    pub fn push_ebb_param(&mut self, ebb: Ebb, value: Value) {
        self.ebbs.get_mut(ebb).data_mut().push_param(value);
    }

    pub fn push_ebb(&mut self, ebb: Ebb) {
        match self.last_ebb {
            Some(last_ebb) => {
                self.ebbs.get_mut(last_ebb).set_next_ebb(ebb);
                self.ebbs.get_mut(ebb).set_prev_ebb(last_ebb);
            }
            None => {
                self.first_ebb = Some(ebb);
            }
        }
        self.last_ebb = Some(ebb);
    }

    pub fn push_inst(&mut self, ebb: Ebb, inst: Inst) {
        self.insts.get_mut(inst).set_ebb(ebb);

        let ebb_node = self.ebbs.get_mut(ebb);
        match ebb_node.last_inst() {
            Some(last_inst) => {
                self.insts.get_mut(last_inst).set_next_inst(Some(inst));
                self.insts.get_mut(inst).set_prev_inst(Some(last_inst));
            }
            None => {
                ebb_node.set_first_inst(Some(inst));
            }
        }
        ebb_node.set_last_inst(Some(inst));
    }

    pub fn remove_inst(&mut self, inst: Inst) {
        let (ebb, prev_inst, next_inst) = {
            let inst_node = self.insts.get(inst);
            (
                inst_node.ebb(),
                inst_node.prev_inst(),
                inst_node.next_inst(),
            )
        };

        match prev_inst {
            Some(prev_inst) => {
                self.insts.get_mut(prev_inst).set_next_inst(next_inst);
            }
            None => {
                self.ebbs.get_mut(ebb).set_first_inst(next_inst);
            }
        }

        match next_inst {
            Some(next_inst) => {
                self.insts.get_mut(next_inst).set_prev_inst(prev_inst);
            }
            None => {
                self.ebbs.get_mut(ebb).set_last_inst(prev_inst);
            }
        }
    }
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "func({}):", DisplayList::new(&self.params))?;
        for ebb in self.ebbs() {
            let ebb_node = self.ebb(ebb);
            writeln!(f)?;
            writeln!(f, "{}({}):", ebb, DisplayList::new(ebb_node.params()))?;
            for inst in self.insts(ebb) {
                let inst_node = self.inst(inst);
                writeln!(f, "    {}", inst_node)?;
            }
        }
        Ok(())
    }
}

pub struct EbbIterator<'a> {
    func: &'a Func,
    next: Option<Ebb>,
}

impl<'a> EbbIterator<'a> {
    fn new(func: &Func) -> EbbIterator {
        EbbIterator {
            func,
            next: func.first_ebb,
        }
    }
}

impl<'a> Iterator for EbbIterator<'a> {
    type Item = Ebb;

    fn next(&mut self) -> Option<Ebb> {
        match self.next {
            Some(ebb) => {
                self.next = self.func.ebbs.get(ebb).next_ebb();
                Some(ebb)
            }
            None => None,
        }
    }
}

pub struct InstIterator<'a> {
    func: &'a Func,
    next: Option<Inst>,
}

impl<'a> InstIterator<'a> {
    fn new(func: &Func, ebb: Ebb) -> InstIterator {
        InstIterator {
            func,
            next: func.ebbs.get(ebb).first_inst(),
        }
    }
}

impl<'a> Iterator for InstIterator<'a> {
    type Item = Inst;

    fn next(&mut self) -> Option<Inst> {
        match self.next {
            Some(inst) => {
                self.next = self.func.insts.get(inst).next_inst();
                Some(inst)
            }
            None => None,
        }
    }
}
