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
        for block in self.func.layout.blocks() {
            let last_inst = self.func.layout.last_inst(block);

            let mut is_prev_inst_branch = false;

            for inst in self.func.layout.insts(block) {
                let inst_data = self.func.data.inst(inst);

                match (last_inst == Some(inst), inst_data.is_terminator()) {
                    (true, false) => {
                        panic!(
                            "Instruction is not the last instruction in block but is a terminator instruction"
                        );
                    }
                    (false, true) => {
                        panic!(
                            "Instruction is the last instruction in block but is not a terminator instruction"
                        );
                    }
                    _ => {}
                }

                if is_prev_inst_branch && !inst_data.is_jump() {
                    panic!("Branch instruction is not followed by a jump instruction");
                }

                is_prev_inst_branch = inst_data.is_branch();
            }
        }
    }
}
