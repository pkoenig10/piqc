#[macro_use]
extern crate lalrpop_util;

use crate::piqc::FuncParser;

pub mod ast;
pub mod ir;

lalrpop_mod!(
    #[allow(warnings)]
    piqc
);

pub fn compile(s: &str) -> String {
    let func = parse(s);
    ast::type_check(&func);

    let mut func = ast::generate_ir(&func);

    ir::verify_ir(&func);

    ir::run_dead_code(&mut func);

    format!("{}", func)
}

pub fn parse(input: &str) -> ast::Func {
    FuncParser::new().parse(input).unwrap()
}
