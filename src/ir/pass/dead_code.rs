use std::collections::HashMap;
use std::collections::HashSet;

use ir::*;

pub fn run_dead_code(prog: &mut Prog) {
    let pass = DeadCodePass::new();
    pass.run(prog.func_mut());
}

struct DeadCodePass {
    defs: HashMap<Inst, Value>,
    uses: HashMap<Value, HashSet<Inst>>,
}

impl DeadCodePass {
    fn new() -> DeadCodePass {
        DeadCodePass {
            defs: HashMap::new(),
            uses: HashMap::new(),
        }
    }

    pub fn run(mut self, func: &mut Func) {
        let mut insts = Vec::new();

        self.analyze_func(func);

        loop {
            for (inst, value) in &self.defs {
                if !self.has_uses(*value) {
                    // insts.push(*inst);
                }
            }

            if insts.is_empty() {
                break;
            }

            for inst in insts.drain(..) {
                func.remove_inst(inst);

                self.defs.remove(&inst);

                for insts in self.uses.values_mut() {
                    insts.remove(&inst);
                }
            }
        }
    }

    fn analyze_func(&mut self, func: &Func) {
        for ebb in func.ebbs() {
            self.analyze_ebb(func, ebb);
        }
    }

    fn analyze_ebb(&mut self, func: &Func, ebb: Ebb) {
        for inst in func.insts(ebb) {
            self.analyze_inst(func, inst);
        }
    }

    fn analyze_inst(&mut self, func: &Func, inst: Inst) {
        match *func.inst(inst) {
            InstData::IntConstInst(ref data) => {
                self.insert_def(inst, data.dest());
            }
            InstData::FloatConstInst(ref data) => {
                self.insert_def(inst, data.dest());
            }
            InstData::BoolConstInst(ref data) => {
                self.insert_def(inst, data.dest());
            }
            InstData::IndexInst(ref data) => {
                self.insert_def(inst, data.dest());
            }
            InstData::CountInst(ref data) => {
                self.insert_def(inst, data.dest());
            }
            InstData::UnaryInst(ref data) => {
                self.insert_use_operand(inst, data.src());
                self.insert_def(inst, data.dest());
            }
            InstData::BinaryInst(ref data) => {
                self.insert_use_operand(inst, data.left());
                self.insert_use_operand(inst, data.right());
                self.insert_def(inst, data.dest());
            }
            InstData::IntCompInst(ref data) => {
                self.insert_use_operand(inst, data.left());
                self.insert_use_operand(inst, data.right());
                self.insert_def(inst, data.dest());
            }
            InstData::FloatCompInst(ref data) => {
                self.insert_use_operand(inst, data.left());
                self.insert_use_operand(inst, data.right());
                self.insert_def(inst, data.dest());
            }
            InstData::SelectInst(ref data) => {
                self.insert_use(inst, data.cond());
                self.insert_use_operand(inst, data.left());
                self.insert_use_operand(inst, data.right());
                self.insert_def(inst, data.dest());
            }
            InstData::JumpInst(ref data) => {
                for arg in data.target().args() {
                    self.insert_use(inst, *arg)
                }
            }
            InstData::BranchInst(ref data) => {
                for arg in data.target().args() {
                    self.insert_use(inst, *arg)
                }
            }
            _ => {}
        }
    }

    fn insert_def(&mut self, inst: Inst, value: Value) {
        self.defs.insert(inst, value);
    }

    fn insert_use(&mut self, inst: Inst, value: Value) {
        self.uses.entry(value).or_insert_with(HashSet::new).insert(
            inst,
        );
    }

    fn insert_use_operand(&mut self, inst: Inst, operand: Operand) {
        if let Operand::Value(value) = operand {
            self.insert_use(inst, value);
        }
    }

    fn has_uses(&self, value: Value) -> bool {
        match self.uses.get(&value) {
            Some(uses) => !uses.is_empty(),
            None => false,
        }
    }
}
