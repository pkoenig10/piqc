use std::collections::HashMap;

use crate::ast::*;
use crate::ir;
use crate::util::Generator;

pub fn generate_ir(func: &Func) -> ir::Func {
    IrGenerator::new().func(func).unwrap()
}

enum Value {
    Var(ir::Variable),
    Val(ir::Value),
    Idx(ir::Value),
    Addr(ir::Value),
}

enum BinaryKind {
    Binary(ir::BinaryOp),
    IntCmp(ir::CmpOp),
    FloatCmp(ir::CmpOp),
}

type ExprResult = Result<(Type, Value), ()>;

type BinaryResult = Result<(Type, BinaryKind), ()>;

type SymbolResult = Result<(Type, ir::Variable), ()>;

#[derive(Debug)]
struct SymbolTable {
    scopes: Vec<HashMap<Symbol, SymbolResult>>,
    generator: Generator<ir::Variable>,
}

impl SymbolTable {
    fn new() -> SymbolTable {
        SymbolTable {
            scopes: Vec::new(),
            generator: Generator::new(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes
            .pop()
            .expect("Failed to pop scope from symbol table");
    }

    fn get(&self, symbol: Symbol) -> Option<&SymbolResult> {
        self.scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.get(&symbol))
            .nth(0)
    }

    fn insert_ok(&mut self, symbol: Symbol, ty: Type) -> ir::Variable {
        let variable = self.generator.next();
        self.insert(symbol, Ok((ty, variable)));
        variable
    }

    fn insert_err(&mut self, symbol: Symbol) {
        self.insert(symbol, Err(()));
    }

    fn insert(&mut self, symbol: Symbol, result: SymbolResult) {
        self.scopes
            .last_mut()
            .expect("Failed to get current scope from symbol")
            .insert(symbol, result);
    }
}

struct IrGenerator {
    builder: ir::FuncBuilder,
    symbols: SymbolTable,
    predicate: Option<ir::Value>,
    not_returned: Option<ir::Value>,
    errors: Vec<String>,
}

impl IrGenerator {
    fn new() -> IrGenerator {
        IrGenerator {
            builder: ir::FuncBuilder::new(),
            symbols: SymbolTable::new(),
            predicate: None,
            not_returned: None,
            errors: Vec::new(),
        }
    }

    fn func(mut self, func: &Func) -> Result<ir::Func, ()> {
        self.symbols.push_scope();

        let entry_ebb = self.builder.create_ebb();
        self.builder.set_position(entry_ebb);

        for param in &func.params {
            if let Ok(variable) = self.param(param) {
                self.builder.push_param(param.ty.into());
                self.builder
                    .push_ebb_param(variable, entry_ebb, param.ty.into());
            }
        }

        self.stmt(&func.stmt);

        if !self.builder.is_filled() {
            self.builder.push_return_inst();
        }

        if !self.errors.is_empty() {
            for error in self.errors {
                eprintln!("{}", error);
            }
            return Err(());
        }

        Ok(self.builder.build())
    }

    fn param(&mut self, param: &Param) -> Result<ir::Variable, ()> {
        match param.ty {
            Type::Prim(Variability::Uniform, _)
            | Type::PrimRef(Variability::Uniform, _)
            | Type::ArrayRef(Variability::Uniform, _) => {
                let variable = self.symbols.insert_ok(param.identifier.symbol, param.ty);
                Ok(variable)
            }
            _ => {
                self.symbols.insert_err(param.identifier.symbol);
                self.errors
                    .push(format!("Invalid parameter with type '{}'", param.ty));
                Err(())
            }
        }
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match stmt.kind {
            StmtKind::Block(ref stmt) => self.block_stmt(stmt),
            StmtKind::Decl(ref stmt) => self.decl_stmt(stmt),
            StmtKind::Assign(ref stmt) => self.assign_stmt(stmt),
            StmtKind::If(ref stmt) => self.if_stmt(stmt),
            StmtKind::While(ref stmt) => self.while_stmt(stmt),
            StmtKind::Return(ref stmt) => self.return_stmt(stmt),
        };
    }

    fn block_stmt(&mut self, stmt: &BlockStmt) {
        self.symbols.push_scope();

        for stmt in &stmt.stmts {
            self.stmt(stmt);
        }

        self.symbols.pop_scope();
    }

    fn decl_stmt(&mut self, stmt: &DeclStmt) {
        let result = fn_block!({
            let expr_result = self.expr_value(&stmt.expr);

            let (expr_type, expr_value) = expr_result?;

            if !is_valid_assign(stmt.ty, expr_type) {
                self.errors.push(format!(
                    "Mismatched types '{}' and '{}'",
                    stmt.ty, expr_type
                ));
                return Err(());
            }

            Ok(expr_value)
        });

        match result {
            Ok(value) => {
                let variable = self.symbols.insert_ok(stmt.identifier.symbol, stmt.ty);
                self.builder.def_var(variable, value);
            }
            Err(()) => {
                self.symbols.insert_err(stmt.identifier.symbol);
            }
        };
    }

    fn assign_stmt(&mut self, stmt: &AssignStmt) {
        let src_result = self.expr(&stmt.src);
        let dest_result = self.expr(&stmt.dest);

        let (src_type, src_value) = unwrap_or_return!(src_result);
        let (dest_type, dest_value) = unwrap_or_return!(dest_result);

        if !is_valid_assign(dest_type, src_type) {
            self.errors.push(format!(
                "Mismatched types '{}' and '{}'",
                dest_type, src_type
            ));
            return;
        }

        match dest_value {
            Value::Var(dest_variable) => {
                let src_value = self.resolve_value(src_type, src_value);

                let src_value = match self.predicate {
                    Some(predicate) => {
                        let prev_value = self.builder.use_var(dest_variable);
                        self.builder
                            .push_select_inst(predicate, src_value, prev_value)
                    }
                    None => src_value,
                };

                self.builder.def_var(dest_variable, src_value);
            }
            Value::Idx(dest_idx) => {
                let src_value = self.resolve_value(src_type, src_value);

                self.builder
                    .push_write_inst(self.predicate, src_value, dest_idx);
            }
            Value::Addr(dest_addr) => {
                let src_idx = match src_value {
                    Value::Idx(src_idx) => src_idx,
                    _ => {
                        let src_value = self.resolve_value(src_type, src_value);
                        let tmp_idx = self.builder.push_alloc_inst(1);
                        self.builder
                            .push_write_inst(self.predicate, src_value, tmp_idx);
                        tmp_idx
                    }
                };

                self.builder.push_store_inst(src_idx, dest_addr)
            }
            _ => {
                self.errors.push(format!("Invalid place expression"));
                return;
            }
        };
    }

    fn if_stmt(&mut self, stmt: &IfStmt) {
        let cond_result = self.expr_value(&stmt.cond);

        let (cond_type, cond_value) = unwrap_or_return!(cond_result);

        match cond_type {
            Type::UNIFORM_BOOL => {
                let else_ebb = self.builder.create_ebb();
                let merge_ebb = match stmt.else_stmt {
                    Some(_) => self.builder.create_ebb(),
                    None => else_ebb,
                };

                self.builder
                    .push_branch_inst(ir::BranchOp::AllFalse, cond_value, else_ebb);

                self.stmt(&stmt.if_stmt);
                self.builder.push_jump_inst(merge_ebb);

                if let Some(ref else_stmt) = stmt.else_stmt {
                    self.builder.set_position(else_ebb);

                    self.stmt(else_stmt);
                    self.builder.push_jump_inst(merge_ebb);
                }

                self.builder.set_position(merge_ebb);
            }
            Type::VARYING_BOOL => {
                let prev_predicate = self.set_predicate_and(cond_value);

                self.stmt(&stmt.if_stmt);
                self.reset_predicate(prev_predicate);

                if let Some(ref else_stmt) = stmt.else_stmt {
                    self.set_predicate_and_not(cond_value);

                    self.stmt(else_stmt);
                    self.reset_predicate(prev_predicate);
                }
            }
            _ => {
                self.errors.push(format!(
                    "Invalid while statement condition with type '{}'",
                    cond_type
                ));
                return;
            }
        };
    }

    fn while_stmt(&mut self, stmt: &WhileStmt) {
        let header_ebb = self.builder.create_ebb();
        let after_ebb = self.builder.create_ebb();

        self.builder.push_jump_inst(header_ebb);
        self.builder.set_position(header_ebb);

        let cond_result = self.expr_value(&stmt.cond);

        let (cond_type, cond_value) = unwrap_or_return!(cond_result);

        match cond_type {
            Type::UNIFORM_BOOL => {
                self.builder
                    .push_branch_inst(ir::BranchOp::AllFalse, cond_value, after_ebb);

                self.stmt(&stmt.stmt);
                self.builder.push_jump_inst(header_ebb);

                self.builder.set_position(after_ebb);
            }
            Type::VARYING_BOOL => {
                let prev_predicate = self.set_predicate_and(cond_value);

                if let Some(predicate) = self.predicate {
                    self.builder
                        .push_branch_inst(ir::BranchOp::AnyFalse, predicate, after_ebb);
                }

                self.stmt(&stmt.stmt);
                self.builder.push_jump_inst(header_ebb);
                self.builder.set_position(after_ebb);

                self.reset_predicate(prev_predicate);
            }
            _ => {
                self.errors.push(format!(
                    "Invalid while statement condition with type '{}'",
                    cond_type
                ));
                return;
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

    fn expr_value(&mut self, expr: &Expr) -> Result<(Type, ir::Value), ()> {
        self.expr(expr).map(|(ty, value)| {
            let value = self.resolve_value(ty, value);
            (ty, value)
        })
    }

    fn resolve_value(&mut self, ty: Type, value: Value) -> ir::Value {
        match value {
            Value::Var(variable) => self.builder.use_var(variable),
            Value::Val(value) => value,
            Value::Idx(value) => self.builder.push_read_inst(value, ty.into()),
            Value::Addr(value) => self.builder.push_fetch_inst(value, ty.into()),
        }
    }

    fn expr(&mut self, expr: &Expr) -> ExprResult {
        match expr.kind {
            ExprKind::Int(ref expr) => self.int_expr(expr),
            ExprKind::Float(ref expr) => self.float_expr(expr),
            ExprKind::Bool(ref expr) => self.bool_expr(expr),
            ExprKind::Element(_) => self.element_expr(),
            ExprKind::Count(_) => self.count_expr(),
            ExprKind::Identifier(ref expr) => self.identifier_expr(expr),
            ExprKind::Unary(ref expr) => self.unary_expr(expr),
            ExprKind::Binary(ref expr) => self.binary_expr(expr),
            ExprKind::Index(ref expr) => self.index_expr(expr),
            ExprKind::Paren(ref expr) => self.paren_expr(expr),
        }
    }

    fn int_expr(&mut self, expr: &IntExpr) -> ExprResult {
        let value = self.builder.push_int_const_inst(expr.value);

        Ok((Type::UNIFORM_INT, Value::Val(value)))
    }

    fn float_expr(&mut self, expr: &FloatExpr) -> ExprResult {
        let value = self.builder.push_float_const_inst(expr.value);

        Ok((Type::UNIFORM_FLOAT, Value::Val(value)))
    }

    fn bool_expr(&mut self, expr: &BoolExpr) -> ExprResult {
        let value = self.builder.push_bool_const_inst(expr.value);

        Ok((Type::UNIFORM_BOOL, Value::Val(value)))
    }

    fn element_expr(&mut self) -> ExprResult {
        let value = self.builder.push_element_inst();

        Ok((Type::VARYING_INT, Value::Val(value)))
    }

    fn count_expr(&mut self) -> ExprResult {
        let value = self.builder.push_count_inst();

        Ok((Type::UNIFORM_INT, Value::Val(value)))
    }

    fn identifier_expr(&mut self, expr: &IdentifierExpr) -> ExprResult {
        match self.symbols.get(expr.identifier.symbol) {
            Some(&Ok((ty, variable))) => Ok((ty, Value::Var(variable))),
            Some(&Err(_)) => Err(()),
            None => {
                self.errors
                    .push(format!("Cannot find variable '{}'", expr.identifier.symbol));
                Err(())
            }
        }
    }

    fn unary_expr(&mut self, expr: &UnaryExpr) -> ExprResult {
        let expr_result = self.expr_value(&expr.expr);

        let (expr_type, expr_value) = expr_result?;

        match (expr.op, expr_type) {
            (UnaryOp::Deref, Type::PrimRef(var, prim)) => {
                let ty = Type::Prim(var, prim);

                Ok((ty, Value::Addr(expr_value)))
            }
            (UnaryOp::Negate, Type::Prim(_, Primitive::Int)) => {
                let zero = self.builder.push_int_const_inst(0);
                let value = self
                    .builder
                    .push_binary_inst(ir::BinaryOp::Sub, zero, expr_value);

                Ok((expr_type, Value::Val(value)))
            }
            (UnaryOp::Negate, Type::Prim(_, Primitive::Float)) => {
                let zero = self.builder.push_float_const_inst(0.);
                let value = self
                    .builder
                    .push_binary_inst(ir::BinaryOp::Fsub, zero, expr_value);

                Ok((expr_type, Value::Val(value)))
            }
            (UnaryOp::Not, Type::Prim(_, Primitive::Int)) => {
                let value = self.builder.push_unary_inst(ir::UnaryOp::Not, expr_value);

                Ok((expr_type, Value::Val(value)))
            }
            (UnaryOp::Not, Type::Prim(_, Primitive::Bool)) => {
                let one = self.builder.push_bool_const_inst(true);
                let value = self
                    .builder
                    .push_binary_inst(ir::BinaryOp::Xor, one, expr_value);

                Ok((expr_type, Value::Val(value)))
            }
            _ => {
                self.errors.push(format!(
                    "Invalid unary expression '{}' with operand type '{}'",
                    expr.op, expr_type
                ));
                Err(())
            }
        }
    }

    fn binary_expr(&mut self, expr: &BinaryExpr) -> ExprResult {
        let left_result = self.expr_value(&expr.left);
        let right_result = self.expr_value(&expr.right);

        let (left_type, left_value) = left_result?;
        let (right_type, right_value) = right_result?;

        match binary_kind(expr.op, left_type, right_type) {
            Ok((ty, BinaryKind::Binary(op))) => {
                let value = self.builder.push_binary_inst(op, left_value, right_value);

                Ok((ty, Value::Val(value)))
            }
            Ok((ty, BinaryKind::IntCmp(op))) => {
                let value = self.builder.push_int_cmp_inst(op, left_value, right_value);

                Ok((ty, Value::Val(value)))
            }
            Ok((ty, BinaryKind::FloatCmp(op))) => {
                let value = self
                    .builder
                    .push_float_cmp_inst(op, left_value, right_value);

                Ok((ty, Value::Val(value)))
            }
            Err(()) => {
                self.errors.push(format!(
                    "Invalid binary expression '{}' with operand types '{}' and '{}'",
                    expr.op, left_type, right_type
                ));
                Err(())
            }
        }
    }

    fn index_expr(&mut self, expr: &IndexExpr) -> ExprResult {
        let expr_result = self.expr_value(&expr.expr);
        let index_result = self.expr_value(&expr.index);

        let (expr_type, expr_value) = expr_result?;
        let (index_type, index_value) = index_result?;

        match (expr_type, index_type) {
            (Type::Array(expr_vari, expr_prim), Type::UNIFORM_INT) => {
                let ty = Type::Prim(expr_vari, expr_prim);

                let value =
                    self.builder
                        .push_binary_inst(ir::BinaryOp::Add, expr_value, index_value);

                Ok((ty, Value::Idx(value)))
            }
            (Type::ArrayRef(expr_vari, expr_prim), Type::Prim(index_vari, Primitive::Int)) => {
                let var = variability(expr_vari, index_vari);
                let ty = Type::Prim(var, expr_prim);

                let two = self.builder.push_int_const_inst(2);
                let offset_value =
                    self.builder
                        .push_binary_inst(ir::BinaryOp::Shl, index_value, two);
                let value =
                    self.builder
                        .push_binary_inst(ir::BinaryOp::Add, expr_value, offset_value);

                Ok((ty, Value::Addr(value)))
            }
            _ => {
                self.errors.push(format!(
                    "Invalid index expression with value type '{}' and index type '{}'",
                    expr_type, index_type
                ));
                Err(())
            }
        }
    }

    fn paren_expr(&mut self, expr: &ParenExpr) -> ExprResult {
        self.expr(&expr.expr)
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

fn is_valid_assign(place_type: Type, value_type: Type) -> bool {
    match (place_type, value_type) {
        (Type::Prim(place_var, place_prim), Type::Prim(value_var, value_prim)) => {
            if place_prim != value_prim {
                return false;
            }

            if (Variability::Uniform, Variability::Varying) == (place_var, value_var) {
                return false;
            }

            true
        }
        _ => false,
    }
}

fn binary_kind(op: BinaryOp, left_type: Type, right_type: Type) -> BinaryResult {
    let (var, prim) = match (left_type, right_type) {
        (Type::Prim(left_var, left_prim), Type::Prim(right_var, right_prim)) => {
            if left_prim != right_prim {
                return Err(());
            }

            let var = variability(left_var, right_var);
            (var, left_prim)
        }
        _ => return Err(()),
    };

    let kind = match (op, prim) {
        (BinaryOp::Mul, Primitive::Float) => BinaryKind::Binary(ir::BinaryOp::Fmul),
        (BinaryOp::Add, Primitive::Int) => BinaryKind::Binary(ir::BinaryOp::Add),
        (BinaryOp::Add, Primitive::Float) => BinaryKind::Binary(ir::BinaryOp::Fadd),
        (BinaryOp::Sub, Primitive::Int) => BinaryKind::Binary(ir::BinaryOp::Sub),
        (BinaryOp::Sub, Primitive::Float) => BinaryKind::Binary(ir::BinaryOp::Fsub),
        (BinaryOp::Shl, Primitive::Int) => BinaryKind::Binary(ir::BinaryOp::Shl),
        (BinaryOp::Shr, Primitive::Int) => BinaryKind::Binary(ir::BinaryOp::Asr),
        (BinaryOp::Min, Primitive::Int) => BinaryKind::Binary(ir::BinaryOp::Min),
        (BinaryOp::Min, Primitive::Float) => BinaryKind::Binary(ir::BinaryOp::Fmin),
        (BinaryOp::Max, Primitive::Int) => BinaryKind::Binary(ir::BinaryOp::Max),
        (BinaryOp::Max, Primitive::Float) => BinaryKind::Binary(ir::BinaryOp::Fmax),
        (BinaryOp::BitAnd, Primitive::Int) => BinaryKind::Binary(ir::BinaryOp::And),
        (BinaryOp::LogicalAnd, Primitive::Bool) => BinaryKind::Binary(ir::BinaryOp::And),
        (BinaryOp::BitOr, Primitive::Int) => BinaryKind::Binary(ir::BinaryOp::Or),
        (BinaryOp::LogicalOr, Primitive::Bool) => BinaryKind::Binary(ir::BinaryOp::Or),
        (BinaryOp::BitXor, Primitive::Int) => BinaryKind::Binary(ir::BinaryOp::Xor),
        (BinaryOp::Eq, Primitive::Int) => BinaryKind::IntCmp(ir::CmpOp::Eq),
        (BinaryOp::Eq, Primitive::Float) => BinaryKind::FloatCmp(ir::CmpOp::Eq),
        (BinaryOp::Ne, Primitive::Int) => BinaryKind::IntCmp(ir::CmpOp::Ne),
        (BinaryOp::Ne, Primitive::Float) => BinaryKind::FloatCmp(ir::CmpOp::Ne),
        (BinaryOp::Lt, Primitive::Int) => BinaryKind::IntCmp(ir::CmpOp::Lt),
        (BinaryOp::Lt, Primitive::Float) => BinaryKind::FloatCmp(ir::CmpOp::Lt),
        (BinaryOp::Gt, Primitive::Int) => BinaryKind::IntCmp(ir::CmpOp::Gt),
        (BinaryOp::Gt, Primitive::Float) => BinaryKind::FloatCmp(ir::CmpOp::Gt),
        (BinaryOp::Le, Primitive::Int) => BinaryKind::IntCmp(ir::CmpOp::Le),
        (BinaryOp::Le, Primitive::Float) => BinaryKind::FloatCmp(ir::CmpOp::Le),
        (BinaryOp::Ge, Primitive::Int) => BinaryKind::IntCmp(ir::CmpOp::Ge),
        (BinaryOp::Ge, Primitive::Float) => BinaryKind::FloatCmp(ir::CmpOp::Ge),
        _ => return Err(()),
    };

    let prim = match kind {
        BinaryKind::Binary(_) => prim,
        BinaryKind::IntCmp(_) => Primitive::Bool,
        BinaryKind::FloatCmp(_) => Primitive::Bool,
    };

    let ty = Type::Prim(var, prim);

    Ok((ty, kind))
}

pub fn variability(variability1: Variability, variability2: Variability) -> Variability {
    match (variability1, variability2) {
        (Variability::Uniform, Variability::Uniform) => Variability::Uniform,
        _ => Variability::Varying,
    }
}
