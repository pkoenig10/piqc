use crate::ir::*;

#[derive(Debug, Clone, Copy)]
pub enum ProgramPoint {
    Ebb(Ebb),
    Inst(Inst),
}

impl From<Inst> for ProgramPoint {
    fn from(inst: Inst) -> ProgramPoint {
        ProgramPoint::Inst(inst)
    }
}

impl From<Ebb> for ProgramPoint {
    fn from(ebb: Ebb) -> ProgramPoint {
        ProgramPoint::Ebb(ebb)
    }
}
