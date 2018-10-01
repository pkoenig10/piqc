#[macro_use]
extern crate lalrpop_util;

mod ast;
mod collections;
mod ir;

lalrpop_mod!(#[allow(clippy)] pub piqc);

use piqc::ProgParser;

pub fn compile(s: &str) -> String {
    let prog = parse(s);
    ast::type_check(&prog);

    let mut prog = ir::generate_ir(&prog);

    ir::verify_ir(&prog);

    ir::run_dead_code(&mut prog);

    format!("{}", prog)
}

pub fn parse<'input>(input: &'input str) -> ast::Prog<'input> {
    ProgParser::new().parse(input).unwrap()
}
