use std::mem;

use ast;
use ast::Type::*;
use ast::AstVisitor;
use ir::*;
use ir::BinaryOp::*;
use ir::CompOp::*;
use sym::*;

pub fn generate_ir<'input>(prog: &mut ast::Prog) -> Prog {
    let mut ir_generator = IrGenerator::new();
    ir_generator.visit_prog(prog)
}

struct RegisterSupplier {
    next_id: u32,
}

impl RegisterSupplier {
    pub fn new() -> RegisterSupplier {
        RegisterSupplier { next_id: 0 }
    }

    pub fn get(&mut self) -> Register {
        let register = Register::new(self.next_id);
        self.next_id += 1;
        register
    }
}

struct BlockSupplier {
    next_id: u32,
}

impl BlockSupplier {
    pub fn new() -> BlockSupplier {
        BlockSupplier { next_id: 0 }
    }

    pub fn get(&mut self) -> Block {
        let block = Block::new(self.next_id);
        self.next_id += 1;
        block
    }
}

struct IrBuilder {
    blocks: Vec<Block>,
    register_supplier: RegisterSupplier,
    block_supplier: BlockSupplier,
}

impl IrBuilder {
    pub fn new() -> IrBuilder {
        IrBuilder {
            blocks: Vec::new(),
            block_supplier: BlockSupplier::new(),
            register_supplier: RegisterSupplier::new(),
        }
    }

    pub fn take_blocks(&mut self) -> Vec<Block> {
        mem::replace(&mut self.blocks, Vec::new())
    }

    pub fn get_register(&mut self) -> Register {
        self.register_supplier.get()
    }

    pub fn get_block(&mut self) -> Block {
        self.block_supplier.get()
    }

    pub fn push_block(&mut self, block: Block) {
        self.blocks.push(block);
    }

    fn push_inst(&mut self, inst: Inst) {
        self.blocks.last_mut().unwrap().push_inst(inst);
    }

    pub fn push_binary_inst(&mut self, op: BinaryOp, left: Operand, right: Operand) -> Operand {
        let dest = self.register_supplier.get();
        let inst = BinaryInst::new(op, dest, left, right);
        self.push_inst(inst);
        Operand::Register(dest)
    }

    pub fn push_int_comp_inst(&mut self, op: CompOp, left: Operand, right: Operand) -> Operand {
        let dest = self.register_supplier.get();
        let inst = IntCompInst::new(op, dest, left, right);
        self.push_inst(inst);
        Operand::Register(dest)
    }

    pub fn push_float_comp_inst(&mut self, op: CompOp, left: Operand, right: Operand) -> Operand {
        let dest = self.register_supplier.get();
        let inst = FloatCompInst::new(op, dest, left, right);
        self.push_inst(inst);
        Operand::Register(dest)
    }

    pub fn push_return_inst(&mut self) {
        let inst = ReturnInst::new();
        self.push_inst(inst);
    }
}

struct IrGenerator<'input> {
    builder: IrBuilder,
    symbol_table: SymbolTable<'input, Operand>,
}

impl<'input> IrGenerator<'input> {
    pub fn new() -> IrGenerator<'input> {
        IrGenerator {
            builder: IrBuilder::new(),
            symbol_table: SymbolTable::new(),
        }
    }
}

impl<'input> AstVisitor<'input, Operand, (), Func, Prog> for IrGenerator<'input> {
    fn visit_prog(&mut self, prog: &mut ast::Prog<'input>) -> Prog {
        let func = self.visit_func(prog.func());
        Prog::new(func)
    }

    fn visit_func(&mut self, func: &mut ast::Func<'input>) -> Func {
        self.symbol_table.push_scope();

        let mut params = Vec::new();
        for param in func.params() {
            let register = self.builder.get_register();
            params.push(register);

            self.symbol_table.insert(
                param.identifier().name(),
                Operand::Register(register),
            );
        }

        let block = self.builder.get_block();
        self.builder.push_block(block);

        self.visit_stmt(func.stmt());

        self.symbol_table.pop_scope();

        let blocks = self.builder.take_blocks();
        Func::new(params, blocks)
    }

    fn visit_decl_stmt(&mut self, stmt: &mut ast::DeclStmt<'input>) {
        let operand = self.visit_expr(stmt.expr());

        self.symbol_table.insert(stmt.identifier().name(), operand);
    }

    fn visit_assign_stmt(&mut self, stmt: &mut ast::AssignStmt<'input>) {
        let operand = self.visit_expr(stmt.expr());

        self.symbol_table.insert(stmt.identifier().name(), operand);
    }

    fn visit_return_stmt(&mut self, _stmt: &ast::ReturnStmt) {
        self.builder.push_return_inst();
    }

    fn visit_block_stmt(&mut self, stmt: &mut ast::BlockStmt<'input>) {
        self.symbol_table.push_scope();

        for stmt in stmt.stmts() {
            self.visit_stmt(stmt);
        }

        self.symbol_table.pop_scope();
    }

    fn visit_int_literal(&mut self, int_literal: &ast::IntLiteral) -> Operand {
        IntConstant::new(int_literal.value())
    }

    fn visit_float_literal(&mut self, float_literal: &ast::FloatLiteral) -> Operand {
        FloatConstant::new(float_literal.value())
    }

    fn visit_bool_literal(&mut self, bool_literal: &ast::BoolLiteral) -> Operand {
        BoolConstant::new(bool_literal.value())
    }

    fn visit_identifier(&mut self, identifier: &mut ast::Identifier) -> Operand {
        *self.symbol_table.get(identifier.name()).unwrap()
    }

    fn visit_unary_expr(&mut self, expr: &mut ast::UnaryExpr) -> Operand {
        let src = self.visit_expr(expr.expr());

        let op = expr.op();
        let type_ = expr.type_().unwrap();

        let builder = &mut self.builder;

        match (op, type_) {
            (ast::Negate, Int) => builder.push_binary_inst(Sub, IntConstant::new(0), src),
            (ast::Negate, Float) => builder.push_binary_inst(Fsub, FloatConstant::new(0.), src),
            (ast::BitNot, _) => builder.push_binary_inst(Xor, src, IntConstant::new(-1)),
            (ast::LogicalNot, _) => builder.push_binary_inst(Xor, src, IntConstant::new(1)),
            _ => {
                panic!(
                    "Invalid unary expression '{}' with operand type '{}'",
                    op,
                    type_
                )
            }
        }
    }

    fn visit_binary_expr(&mut self, expr: &mut ast::BinaryExpr) -> Operand {
        let left = self.visit_expr(expr.left());
        let right = self.visit_expr(expr.right());

        let op = expr.op();
        let left_type = expr.left().type_().unwrap();
        let right_type = expr.left().type_().unwrap();

        let builder = &mut self.builder;

        match (op, left_type) {
            (ast::Mul, Float) => builder.push_binary_inst(Fmul, left, right),
            (ast::Add, Int) => builder.push_binary_inst(Add, left, right),
            (ast::Add, Float) => builder.push_binary_inst(Fadd, left, right),
            (ast::Sub, Int) => builder.push_binary_inst(Sub, left, right),
            (ast::Sub, Float) => builder.push_binary_inst(Fsub, left, right),
            (ast::Shl, _) => builder.push_binary_inst(Shl, left, right),
            (ast::Shr, _) => builder.push_binary_inst(Asr, left, right),
            (ast::BitAnd, _) |
            (ast::LogicalAnd, _) => builder.push_binary_inst(And, left, right),
            (ast::BitOr, _) |
            (ast::LogicalOr, _) => builder.push_binary_inst(Or, left, right),
            (ast::BitXor, _) => builder.push_binary_inst(Xor, left, right),
            (ast::Eq, Int) | (ast::Eq, Bool) => builder.push_int_comp_inst(Eq, left, right),
            (ast::Eq, Float) => builder.push_float_comp_inst(Eq, left, right),
            (ast::Ne, Int) | (ast::Ne, Bool) => builder.push_int_comp_inst(Ne, left, right),
            (ast::Ne, Float) => builder.push_float_comp_inst(Ne, left, right),
            (ast::Lt, Int) => builder.push_int_comp_inst(Lt, left, right),
            (ast::Lt, Float) => builder.push_float_comp_inst(Lt, left, right),
            (ast::Gt, Int) => builder.push_int_comp_inst(Gt, left, right),
            (ast::Gt, Float) => builder.push_float_comp_inst(Gt, left, right),
            (ast::Le, Int) => builder.push_int_comp_inst(Le, left, right),
            (ast::Le, Float) => builder.push_float_comp_inst(Le, left, right),
            (ast::Ge, Int) => builder.push_int_comp_inst(Ge, left, right),
            (ast::Ge, Float) => builder.push_float_comp_inst(Ge, left, right),
            _ => {
                panic!(
                    "Invalid binary expression '{}' with operand types '{}' and '{}'",
                    op,
                    left_type,
                    right_type
                )
            }
        }
    }
}
