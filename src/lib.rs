mod ast;
mod parser;

use ast::type_checker::type_check;

pub fn parse(s: &str) {
    for (i, c) in s.char_indices() {
        println!("{}: {:?}", i, c);
    }

    let mut prog = parser::parse(s).unwrap();

    type_check(&mut prog);

    println!("{:#?}", prog);
}
