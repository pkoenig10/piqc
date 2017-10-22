use ast;
use ast::Type::*;
use ast::AstVisitor;
use ir::*;
use ir::BinaryOp::*;
use ir::CompOp::*;
use sym::*;

pub fn generate_ir(prog: &mut ast::Prog) -> Prog {
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

struct IrGenerator<'input> {
    register_supplier: RegisterSupplier,
    symbol_table: SymbolTable<'input, Operand>,
    insts: Vec<Inst>,
}

impl<'input> IrGenerator<'input> {
    pub fn new() -> IrGenerator<'input> {
        IrGenerator {
            register_supplier: RegisterSupplier::new(),
            symbol_table: SymbolTable::new(),
            insts: Vec::new(),
        }
    }
}

#[allow(unused_variables)]
impl<'input> AstVisitor<'input, Operand, (), Func, Prog> for IrGenerator<'input> {
    fn visit_prog(&mut self, prog: &mut ast::Prog<'input>) -> Prog {
        let func = self.visit_func(prog.func());
        Prog::new(func)
    }

    fn visit_func(&mut self, func: &mut ast::Func<'input>) -> Func {
        self.symbol_table.push_scope();

        let mut params = Vec::new();
        for param in func.params() {
            let register = self.register_supplier.get();
            params.push(register);

            self.symbol_table.insert(
                param.identifier().name(),
                Operand::Register(register),
            );
        }

        self.visit_stmt(func.stmt());

        self.symbol_table.pop_scope();

        let mut insts = Vec::new();
        insts.append(&mut self.insts);

        Func::new(params, insts)
    }

    fn visit_decl_stmt(&mut self, stmt: &mut ast::DeclStmt<'input>) {
        let operand = self.visit_expr(stmt.expr());

        self.symbol_table.insert(stmt.identifier().name(), operand);
    }

    fn visit_assign_stmt(&mut self, stmt: &mut ast::AssignStmt<'input>) {
        let operand = self.visit_expr(stmt.expr());

        self.symbol_table.insert(stmt.identifier().name(), operand);
    }

    fn visit_return_stmt(&mut self, stmt: &ast::ReturnStmt) {
        let inst = ReturnInst::new();
        self.insts.push(inst);
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
        let dest = self.register_supplier.get();

        let op = expr.op();
        let type_ = expr.type_().unwrap();

        let binary_inst = |op, left, right| BinaryInst::new(op, dest, left, right);

        let inst = match (op, type_) {
            (ast::Negate, Int) => binary_inst(Sub, IntConstant::new(0), src),
            (ast::Negate, Float) => binary_inst(Fsub, FloatConstant::new(0.), src),
            (ast::BitNot, _) => binary_inst(Xor, src, IntConstant::new(-1)),
            (ast::LogicalNot, _) => binary_inst(Xor, src, IntConstant::new(1)),
            _ => {
                panic!(
                    "Invalid unary expression '{}' with operand type '{}'",
                    op,
                    type_
                )
            }
        };
        self.insts.push(inst);

        Operand::Register(dest)
    }

    fn visit_binary_expr(&mut self, expr: &mut ast::BinaryExpr) -> Operand {
        let left = self.visit_expr(expr.left());
        let right = self.visit_expr(expr.right());
        let dest = self.register_supplier.get();

        let op = expr.op();
        let left_type = expr.left().type_().unwrap();
        let right_type = expr.left().type_().unwrap();

        let binary_inst = |op| BinaryInst::new(op, dest, left, right);
        let int_comp_inst = |op| IntCompInst::new(op, dest, left, right);
        let float_comp_inst = |op| FloatCompInst::new(op, dest, left, right);

        let inst = match (op, left_type) {
            (ast::Mul, Float) => binary_inst(Fmul),
            (ast::Add, Int) => binary_inst(Add),
            (ast::Add, Float) => binary_inst(Fadd),
            (ast::Sub, Int) => binary_inst(Sub),
            (ast::Sub, Float) => binary_inst(Fsub),
            (ast::Shl, _) => binary_inst(Shl),
            (ast::Shr, _) => binary_inst(Asr),
            (ast::BitAnd, _) |
            (ast::LogicalAnd, _) => binary_inst(And),
            (ast::BitOr, _) |
            (ast::LogicalOr, _) => binary_inst(Or),
            (ast::BitXor, _) => binary_inst(Xor),
            (ast::Eq, Int) | (ast::Eq, Bool) => int_comp_inst(Eq),
            (ast::Eq, Float) => float_comp_inst(Eq),
            (ast::Ne, Int) | (ast::Ne, Bool) => int_comp_inst(Ne),
            (ast::Ne, Float) => float_comp_inst(Ne),
            (ast::Lt, Int) => int_comp_inst(Lt),
            (ast::Lt, Float) => float_comp_inst(Lt),
            (ast::Gt, Int) => int_comp_inst(Gt),
            (ast::Gt, Float) => float_comp_inst(Gt),
            (ast::Le, Int) => int_comp_inst(Le),
            (ast::Le, Float) => float_comp_inst(Le),
            (ast::Ge, Int) => int_comp_inst(Ge),
            (ast::Ge, Float) => float_comp_inst(Ge),
            _ => {
                panic!(
                    "Invalid binary expression '{}' with operand types '{}' and '{}'",
                    op,
                    left_type,
                    right_type
                )
            }
        };
        self.insts.push(inst);

        Operand::Register(dest)
    }
}
