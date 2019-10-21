use std::fmt;

use crate::collections::{PrimaryMap, SecondaryMap};
use crate::ir::*;

#[derive(Debug)]
struct EbbData {
    params: Vec<Value>,
}

#[derive(Debug)]
enum ValueData {
    Inst(Type, Inst),
    Param(Type, Ebb, usize),
    Alias(Type, Value),
}

impl PrimaryMap<Value, ValueData> {
    pub fn resolve_alias(&self, value: Value) -> Value {
        let mut current_value = value;
        for _ in 0..=self.len() {
            match self[current_value] {
                ValueData::Inst(..) | ValueData::Param(..) => return current_value,
                ValueData::Alias(_, value) => current_value = value,
            }
        }

        panic!("Alias loop detected for {}", value);
    }
}

#[derive(Debug)]
pub struct FuncData {
    params: Vec<Type>,
    ebbs: PrimaryMap<Ebb, EbbData>,
    insts: PrimaryMap<Inst, InstData>,
    values: PrimaryMap<Value, ValueData>,
    results: SecondaryMap<Inst, Option<Value>>,
}

impl FuncData {
    fn new() -> FuncData {
        FuncData {
            params: Vec::new(),
            ebbs: PrimaryMap::new(),
            insts: PrimaryMap::new(),
            values: PrimaryMap::new(),
            results: SecondaryMap::new(),
        }
    }

    pub fn create_ebb(&mut self) -> Ebb {
        let data = EbbData { params: Vec::new() };
        self.ebbs.create(data)
    }

    pub fn create_inst(&mut self, data: InstData) -> Inst {
        self.insts.create(data)
    }

    pub fn create_inst_result(&mut self, inst: Inst, ty: Type) -> Value {
        debug_assert!(
            self.inst_result(inst).is_none(),
            "Instruction {} already has result",
        );

        let value = self.values.create(ValueData::Inst(ty, inst));
        self.results.insert(inst, Some(value));
        value
    }

    pub fn params(&self) -> &[Type] {
        &self.params
    }

    pub fn push_param(&mut self, ty: Type) {
        self.params.push(ty);
    }

    pub fn ebb_params(&self, ebb: Ebb) -> &[Value] {
        &self.ebbs[ebb].params
    }

    pub fn push_ebb_param(&mut self, ebb: Ebb, ty: Type) -> Value {
        let idx = self.ebbs[ebb].params.len();
        let value = self.values.create(ValueData::Param(ty, ebb, idx));
        self.ebbs[ebb].params.push(value);
        value
    }

    pub fn swap_remove_ebb_param(&mut self, value: Value) {
        let (ebb, idx) = match self.values[value] {
            ValueData::Param(_, ebb, idx) => (ebb, idx),
            _ => panic!("Value {} is not a block parameter", value),
        };

        let params = &mut self.ebbs[ebb].params;

        params.swap_remove(idx);

        if let Some(&swapped_value) = params.get(idx) {
            match &mut self.values[swapped_value] {
                ValueData::Param(_, _, swapped_idx) => {
                    *swapped_idx = idx;
                }
                _ => panic!("Value {} is not a block parameter", swapped_value),
            }
        }
    }

    pub fn inst(&self, inst: Inst) -> &InstData {
        &self.insts[inst]
    }

    pub fn inst_mut(&mut self, inst: Inst) -> &mut InstData {
        &mut self.insts[inst]
    }

    pub fn inst_result(&self, inst: Inst) -> Option<Value> {
        self.results[inst]
    }

    pub fn value_type(&self, value: Value) -> Type {
        match self.values[value] {
            ValueData::Inst(ty, ..) | ValueData::Param(ty, ..) | ValueData::Alias(ty, ..) => ty,
        }
    }

    pub fn value_to_alias(&mut self, value: Value, dest: Value) {
        debug_assert!(
            !self.is_value_attached(value),
            "Value {} is attached",
            value
        );

        let resolved_dest = self.resolve_alias(dest);
        debug_assert_ne!(
            value, resolved_dest,
            "Aliasing {} for {} would create a loop",
            value, dest,
        );

        let value_type = self.value_type(value);
        let dest_type = self.value_type(resolved_dest);
        debug_assert_eq!(
            value_type, dest_type,
            "Aliasing {} to {} would change type from {} to {}",
            value, dest, value_type, dest_type,
        );

        let ty = self.value_type(value);
        self.values[value] = ValueData::Alias(ty, dest);
    }

    pub fn resolve_alias(&self, value: Value) -> Value {
        self.values.resolve_alias(value)
    }

    pub fn resolve_aliases_in_inst(&mut self, inst: Inst) {
        for arg in self.insts[inst].args_mut() {
            let resolved_arg = self.values.resolve_alias(*arg);
            if resolved_arg != *arg {
                *arg = resolved_arg
            }
        }
    }

    fn is_value_attached(&self, value: Value) -> bool {
        match self.values[value] {
            ValueData::Inst(_, inst) => self.inst_result(inst) == Some(value),
            ValueData::Param(_, ebb, idx) => self.ebb_params(ebb).get(idx) == Some(&value),
            ValueData::Alias(..) => false,
        }
    }
}

#[derive(Debug, Default)]
struct EbbNode {
    prev_ebb: Option<Ebb>,
    next_ebb: Option<Ebb>,
    first_inst: Option<Inst>,
    last_inst: Option<Inst>,
}

#[derive(Debug, Default)]
struct InstNode {
    ebb: Option<Ebb>,
    prev_inst: Option<Inst>,
    next_inst: Option<Inst>,
}

#[derive(Debug)]
pub struct FuncLayout {
    ebbs: SecondaryMap<Ebb, EbbNode>,
    insts: SecondaryMap<Inst, InstNode>,
    first_ebb: Option<Ebb>,
    last_ebb: Option<Ebb>,
}

impl FuncLayout {
    pub fn new() -> FuncLayout {
        FuncLayout {
            ebbs: SecondaryMap::new(),
            insts: SecondaryMap::new(),
            first_ebb: None,
            last_ebb: None,
        }
    }

    pub fn ebbs(&self) -> EbbIter<'_> {
        EbbIter {
            ebbs: &self.ebbs,
            next: self.first_ebb,
        }
    }

    pub fn first_ebb(&self) -> Option<Ebb> {
        self.first_ebb
    }

    pub fn last_ebb(&self) -> Option<Ebb> {
        self.last_ebb
    }

    pub fn is_ebb_inserted(&self, ebb: Ebb) -> bool {
        self.first_ebb == Some(ebb) || self.ebbs[ebb].prev_ebb.is_some()
    }

    pub fn push_ebb(&mut self, ebb: Ebb) {
        let ebb_node = &mut self.ebbs[ebb];

        ebb_node.prev_ebb = self.last_ebb;

        match self.last_ebb {
            Some(prev_ebb) => {
                self.ebbs[prev_ebb].next_ebb = Some(ebb);
            }
            None => {
                self.first_ebb = Some(ebb);
            }
        }
        self.last_ebb = Some(ebb);
    }

    pub fn remove_ebb(&mut self, ebb: Ebb) {
        let ebb_node = &mut self.ebbs[ebb];

        let prev_ebb = ebb_node.prev_ebb;
        let next_ebb = ebb_node.next_ebb;
        ebb_node.prev_ebb = None;
        ebb_node.next_ebb = None;

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

    pub fn insts(&self, ebb: Ebb) -> InstIter<'_> {
        InstIter {
            insts: &self.insts,
            next: self.ebbs[ebb].first_inst,
        }
    }

    pub fn first_inst(&self, ebb: Ebb) -> Option<Inst> {
        self.ebbs[ebb].first_inst
    }

    pub fn last_inst(&self, ebb: Ebb) -> Option<Inst> {
        self.ebbs[ebb].last_inst
    }

    pub fn inst_ebb(&self, inst: Inst) -> Option<Ebb> {
        self.insts[inst].ebb
    }

    pub fn prev_inst(&self, inst: Inst) -> Option<Inst> {
        self.insts[inst].prev_inst
    }

    pub fn next_inst(&self, inst: Inst) -> Option<Inst> {
        self.insts[inst].next_inst
    }

    pub fn push_inst(&mut self, ebb: Ebb, inst: Inst) {
        let ebb_node = &mut self.ebbs[ebb];
        let inst_node = &mut self.insts[inst];

        inst_node.ebb = Some(ebb);
        inst_node.prev_inst = ebb_node.last_inst;

        match ebb_node.last_inst {
            Some(prev_inst) => {
                self.insts[prev_inst].next_inst = Some(inst);
            }
            None => {
                ebb_node.first_inst = Some(inst);
            }
        }
        ebb_node.last_inst = Some(inst);
    }

    pub fn remove_inst(&mut self, inst: Inst) {
        let inst_node = &mut self.insts[inst];

        let ebb = inst_node.ebb;
        let prev_inst = inst_node.prev_inst;
        let next_inst = inst_node.next_inst;
        inst_node.ebb = None;
        inst_node.prev_inst = None;
        inst_node.next_inst = None;

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

pub struct EbbIter<'a> {
    ebbs: &'a SecondaryMap<Ebb, EbbNode>,
    next: Option<Ebb>,
}

impl Iterator for EbbIter<'_> {
    type Item = Ebb;

    fn next(&mut self) -> Option<Ebb> {
        match self.next {
            Some(ebb) => {
                self.next = self.ebbs[ebb].next_ebb;
                Some(ebb)
            }
            None => None,
        }
    }
}

pub struct InstIter<'a> {
    insts: &'a SecondaryMap<Inst, InstNode>,
    next: Option<Inst>,
}

impl Iterator for InstIter<'_> {
    type Item = Inst;

    fn next(&mut self) -> Option<Inst> {
        match self.next {
            Some(inst) => {
                self.next = self.insts[inst].next_inst;
                Some(inst)
            }
            None => None,
        }
    }
}

#[derive(Debug)]
pub struct Func {
    pub data: FuncData,
    pub layout: FuncLayout,
}

impl Func {
    pub fn new() -> Func {
        Func {
            data: FuncData::new(),
            layout: FuncLayout::new(),
        }
    }

    pub fn resolve_aliases(&mut self) {
        for ebb in self.layout.ebbs() {
            for inst in self.layout.insts(ebb) {
                self.data.resolve_aliases_in_inst(inst);
            }
        }
    }
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "func({}):", DisplaySlice(&self.data.params()))?;
        for ebb in self.layout.ebbs() {
            writeln!(f)?;
            writeln!(f, "{}({}):", ebb, DisplaySlice(self.data.ebb_params(ebb)))?;
            for inst in self.layout.insts(ebb) {
                write!(f, "    ")?;
                if let Some(result) = self.data.inst_result(inst) {
                    write!(f, "{} = ", result)?;
                }
                let data = self.data.inst(inst);
                writeln!(f, "{}", data)?;
            }
        }
        Ok(())
    }
}
