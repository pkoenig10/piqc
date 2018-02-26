mod ast;
mod collections;
mod ir;
mod parser;

pub fn compile(s: &str) {
    let prog = parser::parse(s).unwrap();
    ast::type_check(&prog);
    // println!("{:#?}", prog);

    let mut prog = ir::generate_ir(&prog);
    println!("{}", prog);

    ir::verify_ir(&prog);

    ir::run_ebb_params(&mut prog);
}
