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
        Verifier {
            func,
        }
    }

    pub fn verify_func(self) {
        for block_id in self.func.blocks() {
            for inst_id in self.func.insts(block_id) {
                self.verify_inst(block_id, inst_id);
            }
        }
    }

    pub fn verify_inst(&self, block_id: BlockId, inst_id: InstId) {
        let is_last_inst = self.func.block(block_id).last_inst() == Some(inst_id);
        let is_terminator = self.func.inst(inst_id).inst().is_terminator();

        if !is_last_inst && is_terminator {
            panic!("Instruction is not the last instruction in block but is a terminator instruction");
        }

        if is_last_inst && !is_terminator {
            panic!("Instruction is the last instruction in block but is not a terminator instruction");
        }
    }
}
