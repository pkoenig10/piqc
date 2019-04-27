use std::collections::HashMap;
use std::collections::HashSet;

use crate::ir::*;

pub fn run_dead_code(func: &mut Func) {
    DeadCodePass::new(func).run();
}

#[derive(Debug)]
enum Args {
    Zero,
    One(Value),
    More,
}

impl Args {
    fn insert(&self, value: Value) -> Args {
        match *self {
            Args::Zero => Args::One(value),
            Args::One(one) => {
                if value == one {
                    Args::One(one)
                } else {
                    Args::More
                }
            }
            Args::More => Args::More,
        }
    }
}

struct Analysis {
    defs: HashMap<Value, Inst>,
    uses: HashMap<Value, HashSet<Inst>>,
    predecessors: HashMap<Ebb, HashSet<Inst>>,
}

impl Analysis {
    fn new() -> Analysis {
        Analysis {
            defs: HashMap::new(),
            uses: HashMap::new(),
            predecessors: HashMap::new(),
        }
    }

    fn def(&self, value: Value) -> Option<Inst> {
        self.defs.get(&value).cloned()
    }

    fn defs(&self) -> &HashMap<Value, Inst> {
        &self.defs
    }

    fn insert_def(&mut self, value: Value, inst: Inst) {
        self.defs.insert(value, inst);
    }

    fn uses(&self, value: Value) -> Option<&HashSet<Inst>> {
        self.uses.get(&value)
    }

    fn insert_use(&mut self, value: Value, inst: Inst) {
        self.uses
            .entry(value)
            .or_insert_with(HashSet::new)
            .insert(inst);
    }

    fn predecessors(&self, ebb: Ebb) -> Option<&HashSet<Inst>> {
        self.predecessors.get(&ebb)
    }

    fn insert_predecessor(&mut self, target: &Target, inst: Inst) {
        for &arg in target.args() {
            self.insert_use(arg, inst)
        }

        self.predecessors
            .entry(target.ebb())
            .or_insert_with(HashSet::new)
            .insert(inst);
    }

    fn remove_value(&mut self, value: Value) {
        self.defs.remove(&value);
        self.uses.remove(&value);
    }

    fn replace_value(&mut self, old: Value, new: Value) {
        self.defs.remove(&old);
        if let Some(insts) = self.uses.remove(&old) {
            for inst in insts {
                self.insert_use(new, inst);
            }
        }
    }
}

struct DeadCodePass<'a> {
    func: &'a mut Func,
}

impl<'a> DeadCodePass<'a> {
    fn new(func: &'a mut Func) -> DeadCodePass {
        DeadCodePass { func }
    }

    pub fn run(self) {
        let mut analysis = Analysis::new();
        self.analyze_func(&mut analysis);

        let mut defs_to_remove = Vec::new();
        let mut params_to_remove = Vec::new();
        let mut ebbs_to_remove = Vec::new();
        let mut replaced_params = HashMap::new();

        loop {
            for &value in analysis.defs().keys() {
                let has_uses = match analysis.uses(value) {
                    Some(insts) => !insts.is_empty(),
                    None => false,
                };
                if !has_uses {
                    defs_to_remove.push(value);
                }
            }

            for ebb in self.func.ebbs().skip(1) {
                for (i, &param) in self.func.ebb_params(ebb).iter().enumerate() {
                    let mut args = Args::Zero;

                    match analysis.predecessors(ebb) {
                        Some(predecessors) => {
                            for &inst in predecessors {
                                let target = self.func.inst(inst).target().unwrap();
                                let arg = target.args()[i];
                                if param != arg {
                                    args = args.insert(arg);
                                }
                            }
                        }
                        None => {
                            ebbs_to_remove.push(ebb);
                        }
                    }

                    if let Args::One(arg) = args {
                        params_to_remove.push((ebb, param, arg));
                    }
                }
            }

            let mut changed = false;

            for value in defs_to_remove.drain(..) {
                let inst = analysis.def(value).unwrap();
                self.func.remove_inst(inst);

                analysis.remove_value(value);

                changed = true;
            }

            for (ebb, param, mut arg) in params_to_remove.drain(..) {
                let index = self.func.swap_remove_ebb_param(ebb, param);

                while let Some(&value) = replaced_params.get(&arg) {
                    arg = value;
                }

                for &inst in analysis.uses(param).unwrap() {
                    self.func.inst_mut(inst).replace_value(param, arg);
                }

                for &inst in analysis.predecessors(ebb).unwrap() {
                    let target = self.func.inst_mut(inst).target_mut().unwrap();
                    target.swap_remove_arg(index);
                }

                replaced_params.insert(param, arg);
                analysis.replace_value(param, arg);

                changed = true;
            }

            for ebb in ebbs_to_remove.drain(..) {
                self.func.remove_ebb(ebb);

                changed = true;
            }

            if !changed {
                break;
            }

            replaced_params.clear();
        }
    }

    fn analyze_func(&self, analysis: &mut Analysis) {
        for ebb in self.func.ebbs() {
            self.analyze_ebb(analysis, ebb);
        }
    }

    fn analyze_ebb(&self, analysis: &mut Analysis, ebb: Ebb) {
        for inst in self.func.insts(ebb) {
            self.analyze_inst(analysis, inst);
        }
    }

    fn analyze_inst(&self, analysis: &mut Analysis, inst: Inst) {
        let inst_data = self.func.inst(inst);

        match *inst_data {
            InstData::IntConst(ref data) => {
                analysis.insert_def(data.dest, inst);
            }
            InstData::FloatConst(ref data) => {
                analysis.insert_def(data.dest, inst);
            }
            InstData::BoolConst(ref data) => {
                analysis.insert_def(data.dest, inst);
            }
            InstData::Element(ref data) => {
                analysis.insert_def(data.dest, inst);
            }
            InstData::Count(ref data) => {
                analysis.insert_def(data.dest, inst);
            }
            InstData::Unary(ref data) => {
                analysis.insert_use(data.src, inst);
                analysis.insert_def(data.dest, inst);
            }
            InstData::Binary(ref data) => {
                analysis.insert_use(data.left, inst);
                analysis.insert_use(data.right, inst);
                analysis.insert_def(data.dest, inst);
            }
            InstData::Fetch(ref data) => {
                analysis.insert_use(data.addr, inst);
                analysis.insert_use(data.offset, inst);
                analysis.insert_def(data.dest, inst);
            }
            InstData::Store(ref data) => {
                analysis.insert_use(data.src, inst);
                analysis.insert_use(data.addr, inst);
                analysis.insert_use(data.offset, inst);
            }
            InstData::IntComp(ref data) => {
                analysis.insert_use(data.left, inst);
                analysis.insert_use(data.right, inst);
                analysis.insert_def(data.dest, inst);
            }
            InstData::FloatComp(ref data) => {
                analysis.insert_use(data.left, inst);
                analysis.insert_use(data.right, inst);
                analysis.insert_def(data.dest, inst);
            }
            InstData::Select(ref data) => {
                analysis.insert_use(data.cond, inst);
                analysis.insert_use(data.left, inst);
                analysis.insert_use(data.right, inst);
                analysis.insert_def(data.dest, inst);
            }
            InstData::Jump(ref data) => {
                analysis.insert_predecessor(&data.target, inst);
            }
            InstData::Branch(ref data) => {
                analysis.insert_use(data.cond, inst);
                analysis.insert_predecessor(&data.target, inst);
            }
            InstData::Return(_) => {}
        }
    }
}
