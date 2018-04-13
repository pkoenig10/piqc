use std::cell::Cell;
use std::collections::HashMap;

use ir::*;

pub fn run_ebb_params(prog: &mut Prog) {
    let pass = EbbParamsPass::new();
    pass.run(prog.func_mut());
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParamArgs {
    Zero,
    One(Value),
    More,
}

impl ParamArgs {
    fn insert(self, value: Value) -> ParamArgs {
        match self {
            ParamArgs::Zero => ParamArgs::One(value),
            ParamArgs::One(one) => {
                if value == one {
                    ParamArgs::One(one)
                } else {
                    ParamArgs::More
                }
            }
            ParamArgs::More => ParamArgs::More,
        }
    }
}

struct EbbParamsPass {
    args: HashMap<Value, Cell<ParamArgs>>,
}

impl EbbParamsPass {
    fn new() -> EbbParamsPass {
        EbbParamsPass {
            args: HashMap::new(),
        }
    }

    pub fn run(mut self, func: &mut Func) {
        let mut values = Vec::new();

        self.analyze_func(func);

        loop {
            for (param, args) in &self.args {
                let value = match args.get() {
                    ParamArgs::Zero => Some(None),
                    ParamArgs::One(value) => Some(Some(value)),
                    ParamArgs::More => None,
                };
                if let Some(value) = value {
                    values.push((*param, value));
                }
            }

            if values.is_empty() {
                break;
            }

            for (param, value) in values.drain(..) {
                func.insert_value(param, ValueData::Alias(AliasValueData::new(value)));

                self.args.remove(&param);
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
        if let Some(target) = func.inst(inst).target() {
            self.analyze_target(func, target);
        }
    }

    fn analyze_target(&mut self, func: &Func, target: &Target) {
        let params = func.ebb(target.ebb()).params();
        let args = target.args();
        for (param, arg) in params.iter().zip(args) {
            self.insert_arg(*param, *arg)
        }
    }

    fn insert_arg(&mut self, param: Value, arg: Value) {
        let args = self.args
            .entry(param)
            .or_insert_with(|| Cell::new(ParamArgs::Zero));
        if arg != param {
            args.set(args.get().insert(arg));
        }
    }
}
