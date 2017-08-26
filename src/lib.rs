mod ast;
mod piqc;

pub fn parse(s: &str) {
    println!("{:#?}", piqc::parse_Prog(s).unwrap());
}
