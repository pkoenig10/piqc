mod ast;
mod piqc;

pub fn parse(s: &str) {
    println!("{:#?}", piqc::parse_Stmt(s).unwrap());
}
