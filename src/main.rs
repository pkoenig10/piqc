extern crate piqc;

fn main() {
    piqc::parse(
        "
fn main(int i, float f) {
    int foo = -i + 42;
    int bar = 2 + 2;
    bool baz = !true;
}
",
    );
}
