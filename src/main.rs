extern crate piqc;

fn main() {
    piqc::parse(
        "
{
    int foo = 42;
    bool bar = true;
}
",
    );
}
