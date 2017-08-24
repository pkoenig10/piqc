extern crate lalrpop;

fn main() {
    println!("cargo:rerun-if-changed=src/piqc.lalrpop");
    lalrpop::process_root().unwrap();
}
