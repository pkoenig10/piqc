extern crate lalrpop;

fn main() {
    println!("cargo:rerun-if-changed=src/parser/piqc.lalrpop");
    lalrpop::process_root().unwrap();
}
