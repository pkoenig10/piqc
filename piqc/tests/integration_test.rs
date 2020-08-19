#[macro_use(assert_diff)]
extern crate difference;
extern crate piqc;

use piqc::ir::cfg::ControlFlowGraph;
use piqc::ir::liveness::{Intervals, Order, RegisterAllocator};

macro_rules! test_compile {
    ($piq_file:expr, $ir_file:expr) => {
        let func = piqc::compile(include_str!($piq_file)).unwrap();
        // assert_diff!(include_str!($ir_file), &format!("{}", func), "\n", 0);

        println!("{}", func);

        let mut cfg = ControlFlowGraph::new();
        cfg.compute(&func);
        println!("{:#?}", cfg);

        let mut order = Order::new();
        order.compute(&func, &cfg);
        println!("{:#?}", order);

        let mut intervals = Intervals::new();
        intervals.compute(&func, &cfg, &order);
        println!("{:#?}", intervals);

        let mut allocator = RegisterAllocator::new();
        allocator.run(&intervals);
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
fn if_return_uniform() {
    test_compile!(
        "resources/piq/if_return_uniform.piq",
        "resources/ir/if_return_uniform.ir"
    );
}

#[test]
fn if_return_varying() {
    test_compile!(
        "resources/piq/if_return_varying.piq",
        "resources/ir/if_return_varying.ir"
    );
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
