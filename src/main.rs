extern crate piqc;

fn main() {
    piqc::compile(
        "
fn main(int i, float f) {
    int foo = -i + 42;
    float bar = f * 42.0;
    bool baz = !true;

    if (baz) {
        bar = bar + f;
    }

    bar = bar + 1.0;
    return;
}
",
    );
}
