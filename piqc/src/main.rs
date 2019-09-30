extern crate piqc;

use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let filename = env::args().nth(1).expect("filename not given");
    let mut file = File::open(filename).expect("file not found");
    let mut input = String::new();
    file.read_to_string(&mut input)
        .expect("unable to read file");

    println!("{}", piqc::compile(&input).unwrap());
}
