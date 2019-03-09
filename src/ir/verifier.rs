use crate::ir::*;

pub fn verify_ir(func: &Func) {
    Verifier::new(func).verify_func();
}

pub struct Verifier<'a> {
    func: &'a Func,
}

impl<'a> Verifier<'a> {
    pub fn new(func: &'a Func) -> Verifier<'a> {
        Verifier { func }
    }

    pub fn verify_func(self) {
        for ebb in self.func.ebbs() {
            for inst in self.func.insts(ebb) {
                self.verify_inst(ebb, inst);
            }
        }
    }

    pub fn verify_inst(&self, ebb: Ebb, inst: Inst) {
        let is_last_inst = self.func.last_inst(ebb).unwrap() == inst;
        let is_terminator = self.func.inst(inst).is_terminator();

        if !is_last_inst && is_terminator {
            panic!(
                "Instruction is not the last instruction in ebb but is a terminator instruction"
            );
        }

        if is_last_inst && !is_terminator {
            panic!(
                "Instruction is the last instruction in ebb but is not a terminator instruction"
            );
        }
    }
}
