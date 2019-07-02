use crate::ast::*;
use crate::ir;

pub fn generate_ir(func: &Func) -> ir::Func {
    IrBuilder::new().func(func)
}

struct IrBuilder {
    builder: ir::FuncBuilder,
    predicate: Option<ir::Value>,
    not_returned: Option<ir::Value>,
}

impl IrBuilder {
    fn new() -> IrBuilder {
        IrBuilder {
            builder: ir::FuncBuilder::new(),
            predicate: None,
            not_returned: None,
        }
    }

    fn func(mut self, func: &Func) -> ir::Func {
        let entry_ebb = self.builder.create_ebb();
        self.builder.set_position(entry_ebb);

        for param in &func.params {
            let variable = param.identifier.variable.into();
            let type_ = param.type_;
            self.builder.push_param(type_);
            self.builder.push_ebb_param(variable, entry_ebb, type_);
        }

        self.stmt(&func.stmt);

        if !self.builder.is_filled() {
            self.builder.push_return_inst();
        }

        self.builder.build()
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block(ref stmt) => self.block_stmt(stmt),
            Stmt::Decl(ref stmt) => self.decl_stmt(stmt),
            Stmt::Assign(ref stmt) => self.assign_stmt(stmt),
            Stmt::If(ref stmt) => self.if_stmt(stmt),
            Stmt::While(ref stmt) => self.while_stmt(stmt),
            Stmt::Return(ref stmt) => self.return_stmt(stmt),
        }
    }

    fn block_stmt(&mut self, stmt: &BlockStmt) {
        for stmt in &stmt.stmts {
            self.stmt(stmt);
        }
    }

    fn decl_stmt(&mut self, stmt: &DeclStmt) {
        let expr_value = self.expr(&stmt.expr);

        let variable = stmt.identifier.variable.into();

        self.builder.def_var(variable, expr_value);
    }

    fn assign_stmt(&mut self, stmt: &AssignStmt) {
        let src_value = self.expr(&stmt.src);

        macro_rules! value {
            ($prev_value:expr) => {
                match self.predicate {
                    Some(predicate) => {
                        let prev_value = $prev_value;
                        self.builder
                            .push_select_inst(predicate, src_value, prev_value)
                    }
                    None => src_value,
                }
            };
        }

        match stmt.dest {
            Expr::Identifier(ref identifier) => {
                let variable = identifier.variable.into();

                let value = value!(self.builder.use_var(variable));

                self.builder.def_var(variable, value);
            }
            Expr::Index(ref expr) => {
                let expr_value = self.expr(&expr.expr);
                let index_value = self.expr(&expr.index);

                let value = value!(self.builder.push_fetch_inst(expr_value, index_value));

                self.builder.push_store_inst(value, expr_value, index_value);
            }
            _ => panic!("Can't assign to left-hand side of assign expression"),
        }
    }

    fn if_stmt(&mut self, stmt: &IfStmt) {
        let condition_value = self.expr(&stmt.expr);

        let qualifier = self.builder.get_value_type(condition_value).qualifier;
        match qualifier {
            TypeQualifier::Uniform => {
                let else_ebb = self.builder.create_ebb();
                let merge_ebb = match stmt.else_stmt {
                    Some(_) => self.builder.create_ebb(),
                    None => else_ebb,
                };

                self.builder
                    .push_branch_inst(ir::BranchOp::AllFalse, condition_value, else_ebb);

                self.stmt(&stmt.if_stmt);
                self.builder.push_jump_inst(merge_ebb);

                if let Some(ref else_stmt) = stmt.else_stmt {
                    self.builder.set_position(else_ebb);

                    self.stmt(else_stmt);
                    self.builder.push_jump_inst(merge_ebb);
                }

                self.builder.set_position(merge_ebb);
            }
            TypeQualifier::Varying => {
                let prev_predicate = self.set_predicate_and(condition_value);

                self.stmt(&stmt.if_stmt);
                self.reset_predicate(prev_predicate);

                if let Some(ref else_stmt) = stmt.else_stmt {
                    self.set_predicate_and_not(condition_value);

                    self.stmt(else_stmt);
                    self.reset_predicate(prev_predicate);
                }
            }
        }
    }

    fn while_stmt(&mut self, stmt: &WhileStmt) {
        let header_ebb = self.builder.create_ebb();
        let after_ebb = self.builder.create_ebb();

        self.builder.push_jump_inst(header_ebb);
        self.builder.set_position(header_ebb);

        let condition_value = self.expr(&stmt.expr);

        let ty = self.builder.get_value_type(condition_value);
        match ty.qualifier {
            TypeQualifier::Uniform => {
                self.builder
                    .push_branch_inst(ir::BranchOp::AllFalse, condition_value, after_ebb);

                self.stmt(&stmt.stmt);
                self.builder.push_jump_inst(header_ebb);

                self.builder.set_position(after_ebb);
            }
            TypeQualifier::Varying => {
                let prev_predicate = self.set_predicate_and(condition_value);

                if let Some(predicate) = self.predicate {
                    self.builder
                        .push_branch_inst(ir::BranchOp::AnyFalse, predicate, after_ebb);
                }

                self.stmt(&stmt.stmt);
                self.builder.push_jump_inst(header_ebb);
                self.builder.set_position(after_ebb);

                self.reset_predicate(prev_predicate);
            }
        };
    }

    fn return_stmt(&mut self, _stmt: &ReturnStmt) {
        match self.predicate {
            Some(predicate) => {
                let not_predicate = self.builder.push_unary_inst(ir::UnaryOp::Not, predicate);
                let not_returned = match self.not_returned {
                    Some(not_returned) => self.builder.push_binary_inst(
                        ir::BinaryOp::Add,
                        not_returned,
                        not_predicate,
                    ),
                    None => not_predicate,
                };
                self.not_returned = Some(not_returned);
            }
            None => {
                self.builder.push_return_inst();

                let ebb = self.builder.create_ebb();
                self.builder.set_position(ebb);
            }
        };
    }

    fn expr(&mut self, expr: &Expr) -> ir::Value {
        match expr {
            Expr::IntLiteral(ref int_literal) => self.int_literal(int_literal),
            Expr::FloatLiteral(ref float_literal) => self.float_literal(float_literal),
            Expr::BoolLiteral(ref bool_literal) => self.bool_literal(bool_literal),
            Expr::Element(_) => self.element(),
            Expr::Count(_) => self.count(),
            Expr::Identifier(ref identifier) => self.identifier(identifier),
            Expr::Unary(ref expr) => self.unary_expr(expr),
            Expr::Binary(ref expr) => self.binary_expr(expr),
            Expr::Index(ref expr) => self.index_expr(expr),
        }
    }

    fn int_literal(&mut self, int_literal: &IntLiteral) -> ir::Value {
        self.builder.push_int_const_inst(int_literal.value)
    }

    fn float_literal(&mut self, float_literal: &FloatLiteral) -> ir::Value {
        self.builder.push_float_const_inst(float_literal.value)
    }

    fn bool_literal(&mut self, bool_literal: &BoolLiteral) -> ir::Value {
        self.builder.push_bool_const_inst(bool_literal.value)
    }

    fn element(&mut self) -> ir::Value {
        self.builder.push_element_inst()
    }

    fn count(&mut self) -> ir::Value {
        self.builder.push_count_inst()
    }

    fn identifier(&mut self, identifier: &Identifier) -> ir::Value {
        self.builder.use_var(identifier.variable.into())
    }

    fn unary_expr(&mut self, expr: &UnaryExpr) -> ir::Value {
        let op = expr.op;

        let expr_value = self.expr(&expr.expr);

        let expr_type = self.builder.get_value_type(expr_value);

        match (op, expr_type.kind) {
            (UnaryOp::Negate, TypeKind::INT) => {
                let zero = self.builder.push_int_const_inst(0);
                self.builder
                    .push_binary_inst(ir::BinaryOp::Sub, zero, expr_value)
            }
            (UnaryOp::Negate, TypeKind::FLOAT) => {
                let zero = self.builder.push_float_const_inst(0.);
                self.builder
                    .push_binary_inst(ir::BinaryOp::Fsub, zero, expr_value)
            }
            (UnaryOp::BitNot, TypeKind::BOOL) | (UnaryOp::LogicalNot, TypeKind::BOOL) => {
                self.builder.push_unary_inst(ir::UnaryOp::Not, expr_value)
            }
            _ => panic!(
                "Invalid unary expression '{}' with operand type '{}'",
                op, expr_type
            ),
        }
    }

    fn binary_expr(&mut self, expr: &BinaryExpr) -> ir::Value {
        let op = expr.op;

        let left_value = self.expr(&expr.left);
        let right_value = self.expr(&expr.right);

        let left_type = self.builder.get_value_type(left_value);
        let right_type = self.builder.get_value_type(right_value);

        if let Some(op) = get_binary_op(op, left_type, right_type) {
            return self.builder.push_binary_inst(op, left_value, right_value);
        }

        if let Some(op) = get_int_comp_op(op, left_type, right_type) {
            return self.builder.push_int_comp_inst(op, left_value, right_value);
        }

        if let Some(op) = get_float_comp_op(op, left_type, right_type) {
            return self
                .builder
                .push_float_comp_inst(op, left_value, right_value);
        }

        panic!(
            "Invalid binary expression '{}' with operand types '{}' and '{}'",
            op, left_type, right_type
        )
    }

    fn index_expr(&mut self, expr: &IndexExpr) -> ir::Value {
        let expr_value = self.expr(&expr.expr);
        let index_value = self.expr(&expr.index);

        self.builder.push_fetch_inst(expr_value, index_value)
    }

    fn set_predicate_and(&mut self, value: ir::Value) -> Option<ir::Value> {
        let predicate = match self.predicate {
            Some(predicate) => self
                .builder
                .push_binary_inst(ir::BinaryOp::And, predicate, value),
            None => value,
        };
        self.set_predicate(predicate)
    }

    fn set_predicate_and_not(&mut self, value: ir::Value) -> Option<ir::Value> {
        let not_value = self.builder.push_unary_inst(ir::UnaryOp::Not, value);

        let predicate = match self.predicate {
            Some(predicate) => {
                self.builder
                    .push_binary_inst(ir::BinaryOp::Add, predicate, not_value)
            }
            None => not_value,
        };
        self.set_predicate(predicate)
    }

    fn set_predicate(&mut self, predicate: ir::Value) -> Option<ir::Value> {
        let prev_predicate = self.predicate;
        self.predicate = Some(predicate);
        prev_predicate
    }

    fn reset_predicate(&mut self, predicate: Option<ir::Value>) {
        self.predicate = match self.not_returned {
            Some(not_returned) => {
                let predicate = match predicate {
                    Some(predicate) => {
                        self.builder
                            .push_binary_inst(ir::BinaryOp::And, predicate, not_returned)
                    }
                    None => not_returned,
                };
                Some(predicate)
            }
            None => predicate,
        };
    }
}

fn get_binary_op(op: BinaryOp, left_type: Type, right_type: Type) -> Option<ir::BinaryOp> {
    match (op, left_type.kind, right_type.kind) {
        (BinaryOp::Mul, TypeKind::FLOAT, TypeKind::FLOAT) => Some(ir::BinaryOp::Fmul),
        (BinaryOp::Add, TypeKind::INT, TypeKind::INT) => Some(ir::BinaryOp::Add),
        (BinaryOp::Add, TypeKind::FLOAT, TypeKind::FLOAT) => Some(ir::BinaryOp::Fadd),
        (BinaryOp::Sub, TypeKind::INT, TypeKind::INT) => Some(ir::BinaryOp::Sub),
        (BinaryOp::Sub, TypeKind::FLOAT, TypeKind::FLOAT) => Some(ir::BinaryOp::Fsub),
        (BinaryOp::Shl, TypeKind::INT, TypeKind::INT) => Some(ir::BinaryOp::Shl),
        (BinaryOp::Shr, TypeKind::INT, TypeKind::INT) => Some(ir::BinaryOp::Asr),
        (BinaryOp::Min, TypeKind::INT, TypeKind::INT) => Some(ir::BinaryOp::Min),
        (BinaryOp::Min, TypeKind::FLOAT, TypeKind::FLOAT) => Some(ir::BinaryOp::Fmin),
        (BinaryOp::Max, TypeKind::INT, TypeKind::INT) => Some(ir::BinaryOp::Max),
        (BinaryOp::Max, TypeKind::FLOAT, TypeKind::FLOAT) => Some(ir::BinaryOp::Fmax),
        (BinaryOp::BitAnd, TypeKind::INT, TypeKind::INT)
        | (BinaryOp::LogicalAnd, TypeKind::BOOL, TypeKind::BOOL) => Some(ir::BinaryOp::And),
        (BinaryOp::BitOr, TypeKind::INT, TypeKind::INT)
        | (BinaryOp::LogicalOr, TypeKind::BOOL, TypeKind::BOOL) => Some(ir::BinaryOp::Or),
        (BinaryOp::BitXor, TypeKind::INT, TypeKind::INT) => Some(ir::BinaryOp::Xor),
        _ => None,
    }
}

fn get_int_comp_op(op: BinaryOp, left_type: Type, right_type: Type) -> Option<ir::CompOp> {
    match (left_type.kind, right_type.kind) {
        (TypeKind::INT, TypeKind::INT) | (TypeKind::BOOL, TypeKind::BOOL) => get_comp_op(op),
        _ => None,
    }
}

fn get_float_comp_op(op: BinaryOp, left_type: Type, right_type: Type) -> Option<ir::CompOp> {
    match (left_type.kind, right_type.kind) {
        (TypeKind::FLOAT, TypeKind::FLOAT) => get_comp_op(op),
        _ => None,
    }
}

fn get_comp_op(op: BinaryOp) -> Option<ir::CompOp> {
    match op {
        BinaryOp::Eq => Some(ir::CompOp::Eq),
        BinaryOp::Ne => Some(ir::CompOp::Ne),
        BinaryOp::Lt => Some(ir::CompOp::Lt),
        BinaryOp::Gt => Some(ir::CompOp::Gt),
        BinaryOp::Le => Some(ir::CompOp::Le),
        BinaryOp::Ge => Some(ir::CompOp::Ge),
        _ => None,
    }
}
