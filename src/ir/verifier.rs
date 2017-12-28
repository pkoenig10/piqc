use ir::*;

pub fn verify_ir(prog: &Prog) {
    let verifier = Verifier::new(prog.func());
    verifier.verify_func();
}

pub struct Verifier<'a> {
    func: &'a Func,
}

impl<'a> Verifier<'a> {
    pub fn new(func: &'a Func) -> Verifier<'a> {
        Verifier { func }
    }

    pub fn verify_func(self) {
        for block in self.func.blocks() {
            for inst in self.func.insts(block) {
                self.verify_inst(block, inst);
            }
        }
    }

    pub fn verify_inst(&self, block: Block, inst: Inst) {
        let is_last_inst = self.func.last_inst(block) == inst;
        let is_terminator = self.func.inst(inst).is_terminator();

        if !is_last_inst && is_terminator {
            panic!(
                "Instruction is not the last instruction in block but is a terminator instruction"
            );
        }

        if is_last_inst && !is_terminator {
            panic!(
                "Instruction is the last instruction in block but is not a terminator instruction"
            );
        }
    }
}
