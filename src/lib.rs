mod ast;
mod parser;

pub fn parse(s: &str) {
    for (i, c) in s.char_indices() {
        println!("{}: {:?}", i, c);
    }

    let mut prog = parser::parse(s).unwrap();

    ast::type_check(&mut prog);

    println!("{:#?}", prog);
}
