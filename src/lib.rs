mod ast;
mod piqc;

pub fn parse(s: &str) {
    for (i, c) in s.char_indices() {
        println!("{}: {:?}", i, c);
    }
    println!("{:#?}", piqc::parse_Prog(s).unwrap());
}
