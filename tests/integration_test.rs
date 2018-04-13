extern crate piqc;

macro_rules! test_compile {
    ($piq_file:expr, $ir_file:expr) => {
        assert_eq!(
            piqc::compile(include_str!($piq_file)),
            include_str!($ir_file)
        );
    };
}

#[test]
fn expr() {
    test_compile!("resources/piq/expr.piq", "resources/ir/expr.ir");
}

#[test]
fn if_uniform() {
    test_compile!("resources/piq/if_uniform.piq", "resources/ir/if_uniform.ir");
}

#[test]
fn if_varying() {
    test_compile!("resources/piq/if_varying.piq", "resources/ir/if_varying.ir");
}

#[test]
fn if_else_uniform() {
    test_compile!(
        "resources/piq/if_else_uniform.piq",
        "resources/ir/if_else_uniform.ir"
    );
}

#[test]
fn if_else_varying() {
    test_compile!(
        "resources/piq/if_else_varying.piq",
        "resources/ir/if_else_varying.ir"
    );
}

#[test]
fn while_uniform() {
    test_compile!(
        "resources/piq/while_uniform.piq",
        "resources/ir/while_uniform.ir"
    );
}

#[test]
fn while_varying() {
    test_compile!(
        "resources/piq/while_varying.piq",
        "resources/ir/while_varying.ir"
    );
}
