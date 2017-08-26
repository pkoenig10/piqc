extern crate piqc;

fn main() {
    piqc::parse(
        "
fn main(int i, float f) {
    int foo = 42;
    bool bar = true;
}
",
    );
}
