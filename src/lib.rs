mod ast;
mod parser;

pub fn parse(s: &str) {
    for (i, c) in s.char_indices() {
        println!("{}: {:?}", i, c);
    }
    println!("{:#?}", parser::parse(s).unwrap());
}
