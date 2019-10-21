use piqc::SourceMap;
use std::env;
use std::fs;
use std::path::Path;
use std::process;

fn main() {
    let file_path = env::args().nth(1).unwrap();
    let file_path = Path::new(&file_path);

    let input = fs::read_to_string(file_path).unwrap();

    match piqc::compile(&input) {
        Ok(func) => {
            print!("{}", func);
            process::exit(0);
        }
        Err(errors) => {
            let name = file_path.to_string_lossy();
            let source_map = SourceMap::new(name, &input);
            for error in errors {
                eprintln!("{}", error.display(&source_map));
            }
            process::exit(1);
        }
    };
}
