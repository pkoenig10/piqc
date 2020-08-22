#[macro_use(assert_diff)]
extern crate difference;
extern crate piqc;

use piqc::ir::cfg::ControlFlowGraph;
use piqc::ir::liveness::{Intervals, Order, OrderIndex, RegisterAllocator};

macro_rules! test_compile {
    ($piq_file:expr, $ir_file:expr) => {
        let func = piqc::compile(include_str!($piq_file)).unwrap();
        // assert_diff!(include_str!($ir_file), &format!("{}", func), "\n", 0);

        let mut cfg = ControlFlowGraph::new();
        cfg.compute(&func);

        let mut order = Order::new();
        order.compute(&func, &cfg);

        let mut intervals = Intervals::new();
        intervals.compute(&func, &cfg, &order);

        // println!("{:#?}", cfg);
        // println!("{:#?}", order);
        // println!("{:#?}", intervals);

        // println!("{}", func);
        for block in func.layout.blocks() {
            println!();
            print!("{:2}: {}(", block.use_point(&order).0, block);
            let mut first = true;
            for param in func.data.block_params(block) {
                if first {
                    print!("{}", param);
                    first = false;
                } else {
                    print!(", {}", param);
                }
            }
            println!(")");
            for inst in func.layout.insts(block) {
                print!("{:2}:     ", inst.use_point(&order).0);
                if let Some(result) = func.data.inst_result(inst) {
                    print!("{} = ", result);
                }
                let data = func.data.inst(inst);
                println!("{}", data);
            }
        }
        println!("");

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
