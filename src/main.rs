extern crate piqc;

fn main() {
    piqc::compile(
        "
fn main(int i, float f) {
    int foo = -i + @index;
    float bar = f <? 42.0;
    bool baz = !true;

    if (baz) {
        bar = bar + f;
    }

    while (baz) {
        bar = bar + f;
    }

    bar = bar + f;
    return;
}
",
    );
}
