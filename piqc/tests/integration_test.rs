#[macro_use(assert_diff)]
extern crate difference;
extern crate piqc;

use piqc::ir::cfg::ControlFlowGraph;
use piqc::ir::liveness::{Liveness, Order, OrderIndex, RegisterHints, VirtualRegisters};

macro_rules! test_compile {
    ($piq_file:expr, $ir_file:expr) => {
        let func = piqc::compile(include_str!($piq_file)).unwrap();
        assert_diff!(include_str!($ir_file), &format!("{}", func), "\n", 0);

        // println!("{}", func);

        let mut cfg = ControlFlowGraph::new();
        cfg.compute(&func);

        let mut order = Order::new();
        order.compute(&func, &cfg);

        let mut liveness = Liveness::new();
        liveness.compute(&func, &cfg, &order);

        let mut hints = RegisterHints::new();
        hints.compute(&func, &cfg, &order, &liveness);

        // println!("{}", func);
        for block in cfg.blocks() {
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

        // println!("{:#?}", cfg);
        // println!("{:#?}", order);
        // liveness.print();
        println!("{:#?}", liveness);

        VirtualRegisters::compute(&func, &cfg, &order, &liveness);

        // let mut allocator = RegisterAllocator::new();
        // allocator.run(&intervals);
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

#[test]
fn arg_at_param() {
    test_compile!(
        "resources/piq/arg_at_param.piq",
        "resources/ir/arg_at_param.ir"
    );
}

#[test]
fn param_at_arg() {
    test_compile!(
        "resources/piq/param_at_arg.piq",
        "resources/ir/param_at_arg.ir"
    );
}

#[test]
fn transitive() {
    test_compile!("resources/piq/transitive.piq", "resources/ir/transitive.ir");
}
