#[macro_use]
mod util;

pub mod ast;
pub mod ir;

mod parser;

pub fn compile(s: &str) -> String {
    let func = parser::parse(s);

    let mut func = ast::generate_ir(&func);

    ir::verify_ir(&func);

    ir::run_dead_code(&mut func);

    format!("{}", func)
}
