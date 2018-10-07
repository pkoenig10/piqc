#[macro_use]
extern crate lalrpop_util;

use piqc::ProgParser;

mod ast;
mod ir;

lalrpop_mod!(#[allow(clippy)] pub piqc);

pub fn compile(s: &str) -> String {
    let prog = parse(s);
    ast::type_check(&prog);

    let mut prog = ir::generate_ir(&prog);

    ir::verify_ir(&prog);

    ir::run_dead_code(&mut prog);

    format!("{}", prog)
}

pub fn parse(input: &str) -> ast::Prog {
    ProgParser::new().parse(input).unwrap()
}
