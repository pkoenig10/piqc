mod ast;
mod piqc;

pub fn parse(s: &str) {
    println!("{:#?}", piqc::parse_Expr(s).unwrap());
}
