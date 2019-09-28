#[macro_use]
mod collections;

pub mod ast;
pub mod ir;

mod parser;

pub fn compile(s: &str) -> String {
    let func = parser::parse(s);

    let mut func = ast::generate_ir(&func);

    ir::verify_ir(&func);

    func.resolve_aliases();

    format!("{}", func)
}
