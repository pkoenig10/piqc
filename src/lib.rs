mod ast;
mod ir;
mod parser;
mod sym;

pub fn compile(s: &str) {
    for (i, c) in s.char_indices() {
        println!("{}: {:?}", i, c);
    }

    let mut prog = parser::parse(s).unwrap();
    println!("{:#?}", prog);

    ast::type_check(&mut prog);

    let prog = ir::generate_ir(&mut prog);
    println!("{:#?}", prog);
}
