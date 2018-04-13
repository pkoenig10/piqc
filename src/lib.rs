mod ast;
mod collections;
mod ir;
mod parser;

pub fn compile(s: &str) -> String {
    let prog = parser::parse(s);
    ast::type_check(&prog);

    let mut prog = ir::generate_ir(&prog);

    ir::verify_ir(&prog);

    ir::run_dead_code(&mut prog);
    ir::run_ebb_params(&mut prog);

    format!("{}", prog)
}
