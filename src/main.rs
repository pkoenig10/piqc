mod ast;
mod piqc;

fn main() {
    println!("{:#?}", piqc::parse_Expr("42").unwrap());
}
