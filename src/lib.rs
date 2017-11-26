mod ast;
mod collections;
mod ir;
mod parser;

pub fn compile(s: &str) {
    let mut prog = parser::parse(s).unwrap();
    ast::type_check(&mut prog);
    // println!("{:#?}", prog);

    let prog = ir::generate_ir(&mut prog);
    println!("{}", prog);
}
