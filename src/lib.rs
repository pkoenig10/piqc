mod ast;
mod ir;
mod parser;
mod sym;

pub fn parse(s: &str) {
    for (i, c) in s.char_indices() {
        println!("{}: {:?}", i, c);
    }

    let mut prog = parser::parse(s).unwrap();
    println!("{:#?}", prog);

    ast::type_check(&mut prog);

    let insts = ir::generate_ir(&mut prog);
    println!("{:#?}", insts);
}
