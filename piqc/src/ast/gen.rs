use std::collections::HashMap;

use crate::ast::*;
use crate::collections::Generator;
use crate::error::{Error, Errors};
use crate::ir;
use crate::Span;

pub fn generate_ir(func: &Func) -> Result<ir::Func, Errors> {
    IrGenerator::new().func(func)
}

enum Value {
    Var(ir::Variable),
    Val(ir::Value),
    Idx(ir::Value),
    Addr(ir::Value),
}

type SymbolResult = Result<(Type, ir::Variable), ()>;
type ExprResult = Result<(Type, Value), Option<String>>;
type StmtResult = Result<(), Option<String>>;

#[derive(Debug)]
struct Symbols {
    scopes: Vec<HashMap<Symbol, SymbolResult>>,
    generator: Generator<ir::Variable>,
}

impl Symbols {
    fn new() -> Symbols {
        Symbols {
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
    symbols: Symbols,
    predicate: Option<ir::Value>,
    not_returned: Option<ir::Value>,
    errors: Errors,
}

impl IrGenerator {
    fn new() -> IrGenerator {
        IrGenerator {
            builder: ir::FuncBuilder::new(),
            symbols: Symbols::new(),
            predicate: None,
            not_returned: None,
            errors: Vec::new(),
        }
    }

    fn func(mut self, func: &Func) -> Result<ir::Func, Errors> {
        self.symbols.push_scope();

        let entry_ebb = self.builder.create_ebb();
        self.builder.switch_to_ebb(entry_ebb);
        self.builder.seal_ebb(entry_ebb);

        for param in &func.params {
            self.param(entry_ebb, param);
        }

        self.stmt(&func.stmt);

        if !self.builder.is_filled() {
            self.builder.ret();
        }

        if self.errors.is_empty() {
            Ok(self.builder.finalize())
        } else {
            Err(self.errors)
        }
    }

    fn param(&mut self, ebb: ir::Ebb, param: &Param) {
        match param.ty {
            Type::Prim(Variability::Uniform, _)
            | Type::PrimRef(Variability::Uniform, _)
            | Type::ArrayRef(Variability::Uniform, _) => {
                let value = self.builder.push_param(ebb, param.ty.into());

                let variable = self.symbols.insert_ok(param.identifier.symbol, param.ty);
                self.builder.declare_var(variable, param.ty.into());
                self.builder.def_var(variable, value);
            }
            _ => {
                self.symbols.insert_err(param.identifier.symbol);
                self.errors.push(Error::new(
                    format!("invalid parameter type '{}'", param.ty),
                    param.span,
                ))
            }
        };
    }

    fn stmt(&mut self, stmt: &Stmt) {
        let result = match stmt.kind {
            StmtKind::Block(ref stmt) => self.block_stmt(stmt),
            StmtKind::Decl(ref stmt) => self.decl_stmt(stmt),
            StmtKind::Assign(ref stmt) => self.assign_stmt(stmt),
            StmtKind::If(ref stmt) => self.if_stmt(stmt),
            StmtKind::While(ref stmt) => self.while_stmt(stmt),
            StmtKind::Return(ref stmt) => self.return_stmt(stmt),
        };

        if let Err(error) = result {
            self.push_error(error, stmt.span);
        }
    }

    fn block_stmt(&mut self, stmt: &BlockStmt) -> StmtResult {
        self.symbols.push_scope();

        for stmt in &stmt.stmts {
            if self.builder.is_filled() {
                break;
            }

            self.stmt(stmt);
        }

        self.symbols.pop_scope();

        Ok(())
    }

    fn decl_stmt(&mut self, stmt: &DeclStmt) -> StmtResult {
        let result = (|| {
            let (expr_type, expr_value) = self.expr_value(&stmt.expr)?;

            if !stmt.ty.is_assignable_from(expr_type) {
                return Err(Some(format!(
                    "mismatched types '{}' and '{}'",
                    stmt.ty, expr_type
                )));
            }

            Ok(expr_value)
        })();

        match result {
            Ok(expr_value) => {
                let variable = self.symbols.insert_ok(stmt.identifier.symbol, stmt.ty);
                self.builder.declare_var(variable, stmt.ty.into());
                self.builder.def_var(variable, expr_value);
                Ok(())
            }
            Err(error) => {
                self.symbols.insert_err(stmt.identifier.symbol);
                Err(error)
            }
        }
    }

    fn assign_stmt(&mut self, stmt: &AssignStmt) -> StmtResult {
        let (src_type, src_value) = self.expr(&stmt.src)?;
        let (dest_type, dest_value) = self.expr(&stmt.dest)?;

        if !dest_type.is_assignable_from(src_type) {
            return Err(Some(format!(
                "mismatched types '{}' and '{}'",
                dest_type, src_type
            )));
        }

        match dest_value {
            Value::Var(dest_variable) => {
                let src_value = self.resolve_value(src_type, src_value);

                let src_value = match self.predicate {
                    Some(predicate) => {
                        let prev_value = self.builder.use_var(dest_variable);
                        self.builder.select(predicate, src_value, prev_value)
                    }
                    None => src_value,
                };

                self.builder.def_var(dest_variable, src_value);
            }
            Value::Idx(dest_idx) => {
                let src_value = self.resolve_value(src_type, src_value);

                self.builder.write(src_value, dest_idx);
            }
            Value::Addr(dest_addr) => {
                let src_idx = match src_value {
                    Value::Idx(src_idx) => src_idx,
                    _ => {
                        let src_value = self.resolve_value(src_type, src_value);
                        let tmp_idx = self.builder.alloc(1);
                        self.builder.write(src_value, tmp_idx);
                        tmp_idx
                    }
                };

                self.builder.store(src_idx, dest_addr)
            }
            _ => {
                return Err(Some(format!("invalid place expression")));
            }
        };

        Ok(())
    }

    fn if_stmt(&mut self, stmt: &IfStmt) -> StmtResult {
        let (cond_type, cond_value) = self.expr_value(&stmt.cond)?;

        match cond_type {
            Type::UNIFORM_BOOL => {
                let else_ebb = self.builder.create_ebb();
                let merge_ebb = match stmt.else_stmt {
                    Some(_) => self.builder.create_ebb(),
                    None => else_ebb,
                };

                self.builder.brallz(cond_value, else_ebb);
                self.builder.seal_ebb(else_ebb);

                self.stmt(&stmt.if_stmt);

                if !self.builder.is_filled() {
                    self.builder.jump(merge_ebb);
                }

                if let Some(ref else_stmt) = stmt.else_stmt {
                    self.builder.switch_to_ebb(else_ebb);

                    self.stmt(else_stmt);

                    if !self.builder.is_filled() {
                        self.builder.jump(merge_ebb);
                    }
                }

                self.builder.seal_ebb(merge_ebb);

                self.builder.switch_to_ebb(merge_ebb);
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
                return Err(Some(format!(
                    "invalid if statement condition with type '{}'",
                    cond_type
                )));
            }
        };

        Ok(())
    }

    fn while_stmt(&mut self, stmt: &WhileStmt) -> StmtResult {
        let header_ebb = self.builder.create_ebb();
        let after_ebb = self.builder.create_ebb();

        self.builder.jump(header_ebb);
        self.builder.switch_to_ebb(header_ebb);

        let (cond_type, cond_value) = self.expr_value(&stmt.cond)?;

        match cond_type {
            Type::UNIFORM_BOOL => {
                self.builder.brallz(cond_value, after_ebb);
                self.builder.seal_ebb(after_ebb);

                self.stmt(&stmt.stmt);

                if !self.builder.is_filled() {
                    self.builder.jump(header_ebb);
                }
                self.builder.seal_ebb(header_ebb);

                self.builder.switch_to_ebb(after_ebb);
            }
            Type::VARYING_BOOL => {
                let prev_predicate = self.set_predicate_and(cond_value);

                let predicate = self
                    .predicate
                    .expect("Predicate is none after while condition");
                self.builder.brallz(predicate, after_ebb);
                self.builder.seal_ebb(after_ebb);

                self.stmt(&stmt.stmt);

                if !self.builder.is_filled() {
                    self.builder.jump(header_ebb);
                }
                self.builder.seal_ebb(header_ebb);

                self.builder.switch_to_ebb(after_ebb);

                self.reset_predicate(prev_predicate);
            }
            _ => {
                return Err(Some(format!(
                    "invalid while statement condition with type '{}'",
                    cond_type
                )));
            }
        };

        Ok(())
    }

    fn return_stmt(&mut self, _stmt: &ReturnStmt) -> StmtResult {
        match self.predicate {
            Some(predicate) => {
                let not_predicate = self.builder.not(predicate);
                let not_returned = match self.not_returned {
                    Some(not_returned) => self.builder.add(not_returned, not_predicate),
                    None => not_predicate,
                };
                self.not_returned = Some(not_returned);
            }
            None => {
                self.builder.ret();
            }
        };

        Ok(())
    }

    fn expr_value(&mut self, expr: &Expr) -> Result<(Type, ir::Value), Option<String>> {
        self.expr(expr).map(|(ty, value)| {
            let value = self.resolve_value(ty, value);
            (ty, value)
        })
    }

    fn resolve_value(&mut self, ty: Type, value: Value) -> ir::Value {
        match value {
            Value::Var(variable) => self.builder.use_var(variable),
            Value::Val(value) => value,
            Value::Idx(value) => self.builder.read(ty.into(), value),
            Value::Addr(value) => self.builder.fetch(ty.into(), value),
        }
    }

    fn expr(&mut self, expr: &Expr) -> ExprResult {
        let result = match expr.kind {
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
        };

        result.map_err(|error| self.push_error(error, expr.span))
    }

    fn int_expr(&mut self, expr: &IntExpr) -> ExprResult {
        let value = self.builder.iconst(expr.value);

        Ok((Type::UNIFORM_INT, Value::Val(value)))
    }

    fn float_expr(&mut self, expr: &FloatExpr) -> ExprResult {
        let value = self.builder.fconst(expr.value);

        Ok((Type::UNIFORM_FLOAT, Value::Val(value)))
    }

    fn bool_expr(&mut self, expr: &BoolExpr) -> ExprResult {
        let value = self.builder.bconst(expr.value);

        Ok((Type::UNIFORM_BOOL, Value::Val(value)))
    }

    fn element_expr(&mut self) -> ExprResult {
        let value = self.builder.element();

        Ok((Type::VARYING_INT, Value::Val(value)))
    }

    fn count_expr(&mut self) -> ExprResult {
        let value = self.builder.count();

        Ok((Type::UNIFORM_INT, Value::Val(value)))
    }

    fn identifier_expr(&mut self, expr: &IdentifierExpr) -> ExprResult {
        match self.symbols.get(expr.identifier.symbol) {
            Some(&Ok((ty, variable))) => Ok((ty, Value::Var(variable))),
            Some(&Err(_)) => Err(None),
            None => Err(Some(format!(
                "cannot find variable '{}'",
                expr.identifier.symbol
            ))),
        }
    }

    fn unary_expr(&mut self, expr: &UnaryExpr) -> ExprResult {
        let expr_result = self.expr_value(&expr.expr);

        let (expr_type, expr_value) = expr_result?;

        let (ty, value) = match (expr.op, expr_type) {
            (UnaryOp::Deref, Type::PrimRef(var, prim)) => {
                let ty = Type::Prim(var, prim);

                (ty, Value::Addr(expr_value))
            }
            (UnaryOp::Negate, Type::Prim(_, Primitive::Int)) => {
                let zero = self.builder.iconst(0);
                let value = self.builder.sub(zero, expr_value);

                (expr_type, Value::Val(value))
            }
            (UnaryOp::Negate, Type::Prim(_, Primitive::Float)) => {
                let zero = self.builder.fconst(0.);
                let value = self.builder.fsub(zero, expr_value);

                (expr_type, Value::Val(value))
            }
            (UnaryOp::Not, Type::Prim(_, Primitive::Int)) => {
                let value = self.builder.not(expr_value);

                (expr_type, Value::Val(value))
            }
            (UnaryOp::Not, Type::Prim(_, Primitive::Bool)) => {
                let one = self.builder.bconst(true);
                let value = self.builder.xor(one, expr_value);

                (expr_type, Value::Val(value))
            }
            _ => {
                return Err(Some(format!(
                    "invalid unary expression '{}' with operand type '{}'",
                    expr.op, expr_type
                )));
            }
        };

        Ok((ty, value))
    }

    fn binary_expr(&mut self, expr: &BinaryExpr) -> ExprResult {
        let (left_type, left_value) = self.expr_value(&expr.left)?;
        let (right_type, right_value) = self.expr_value(&expr.right)?;

        let (var, prim) = match (left_type, right_type) {
            (Type::Prim(left_vari, left_prim), Type::Prim(right_vari, right_prim))
                if left_prim == right_prim =>
            {
                (left_vari | right_vari, left_prim)
            }
            _ => {
                return Err(Some(format!(
                    "invalid binary expression '{}' with operand types '{}' and '{}'",
                    expr.op, left_type, right_type
                )));
            }
        };

        let (ty, value) = match (expr.op, prim) {
            (BinaryOp::Mul, Primitive::Float) => {
                let value = self.builder.fmul(left_value, right_value);
                (Type::Prim(var, prim), value)
            }
            (BinaryOp::Add, Primitive::Int) => {
                let value = self.builder.add(left_value, right_value);
                (Type::Prim(var, prim), value)
            }
            (BinaryOp::Add, Primitive::Float) => {
                let value = self.builder.fadd(left_value, right_value);
                (Type::Prim(var, prim), value)
            }
            (BinaryOp::Sub, Primitive::Int) => {
                let value = self.builder.sub(left_value, right_value);
                (Type::Prim(var, prim), value)
            }
            (BinaryOp::Sub, Primitive::Float) => {
                let value = self.builder.fsub(left_value, right_value);
                (Type::Prim(var, prim), value)
            }
            (BinaryOp::Shl, Primitive::Int) => {
                let value = self.builder.shl(left_value, right_value);
                (Type::Prim(var, prim), value)
            }
            (BinaryOp::Shr, Primitive::Int) => {
                let value = self.builder.shr(left_value, right_value);
                (Type::Prim(var, prim), value)
            }
            (BinaryOp::Min, Primitive::Int) => {
                let value = self.builder.min(left_value, right_value);
                (Type::Prim(var, prim), value)
            }
            (BinaryOp::Min, Primitive::Float) => {
                let value = self.builder.fmin(left_value, right_value);
                (Type::Prim(var, prim), value)
            }
            (BinaryOp::Max, Primitive::Int) => {
                let value = self.builder.max(left_value, right_value);
                (Type::Prim(var, prim), value)
            }
            (BinaryOp::Max, Primitive::Float) => {
                let value = self.builder.fmax(left_value, right_value);
                (Type::Prim(var, prim), value)
            }
            (BinaryOp::BitAnd, Primitive::Int) => {
                let value = self.builder.and(left_value, right_value);
                (Type::Prim(var, prim), value)
            }
            (BinaryOp::LogicalAnd, Primitive::Bool) => {
                let value = self.builder.and(left_value, right_value);
                (Type::Prim(var, prim), value)
            }
            (BinaryOp::BitOr, Primitive::Int) => {
                let value = self.builder.or(left_value, right_value);
                (Type::Prim(var, prim), value)
            }
            (BinaryOp::LogicalOr, Primitive::Bool) => {
                let value = self.builder.or(left_value, right_value);
                (Type::Prim(var, prim), value)
            }
            (BinaryOp::BitXor, Primitive::Int) => {
                let value = self.builder.xor(left_value, right_value);
                (Type::Prim(var, prim), value)
            }
            (BinaryOp::Eq, Primitive::Int) => {
                let value = self.builder.icmp(ir::Cond::Eq, left_value, right_value);
                (Type::Prim(var, Primitive::Bool), value)
            }
            (BinaryOp::Eq, Primitive::Float) => {
                let value = self.builder.fcmp(ir::Cond::Eq, left_value, right_value);
                (Type::Prim(var, Primitive::Bool), value)
            }
            (BinaryOp::Ne, Primitive::Int) => {
                let value = self.builder.icmp(ir::Cond::Ne, left_value, right_value);
                (Type::Prim(var, Primitive::Bool), value)
            }
            (BinaryOp::Ne, Primitive::Float) => {
                let value = self.builder.fcmp(ir::Cond::Ne, left_value, right_value);
                (Type::Prim(var, Primitive::Bool), value)
            }
            (BinaryOp::Lt, Primitive::Int) => {
                let value = self.builder.icmp(ir::Cond::Lt, left_value, right_value);
                (Type::Prim(var, Primitive::Bool), value)
            }
            (BinaryOp::Lt, Primitive::Float) => {
                let value = self.builder.fcmp(ir::Cond::Lt, left_value, right_value);
                (Type::Prim(var, Primitive::Bool), value)
            }
            (BinaryOp::Gt, Primitive::Int) => {
                let value = self.builder.icmp(ir::Cond::Gt, left_value, right_value);
                (Type::Prim(var, Primitive::Bool), value)
            }
            (BinaryOp::Gt, Primitive::Float) => {
                let value = self.builder.fcmp(ir::Cond::Gt, left_value, right_value);
                (Type::Prim(var, Primitive::Bool), value)
            }
            (BinaryOp::Le, Primitive::Int) => {
                let value = self.builder.icmp(ir::Cond::Le, left_value, right_value);
                (Type::Prim(var, Primitive::Bool), value)
            }
            (BinaryOp::Le, Primitive::Float) => {
                let value = self.builder.fcmp(ir::Cond::Le, left_value, right_value);
                (Type::Prim(var, Primitive::Bool), value)
            }
            (BinaryOp::Ge, Primitive::Int) => {
                let value = self.builder.icmp(ir::Cond::Ge, left_value, right_value);
                (Type::Prim(var, Primitive::Bool), value)
            }
            (BinaryOp::Ge, Primitive::Float) => {
                let value = self.builder.fcmp(ir::Cond::Ge, left_value, right_value);
                (Type::Prim(var, Primitive::Bool), value)
            }
            _ => {
                return Err(Some(format!(
                    "invalid binary expression '{}' with operand types '{}' and '{}'",
                    expr.op, left_type, right_type
                )));
            }
        };

        Ok((ty, Value::Val(value)))
    }

    fn index_expr(&mut self, expr: &IndexExpr) -> ExprResult {
        let (expr_type, expr_value) = self.expr_value(&expr.expr)?;
        let (index_type, index_value) = self.expr_value(&expr.index)?;

        let (ty, value) = match (expr_type, index_type) {
            (Type::Array(expr_vari, expr_prim), Type::UNIFORM_INT) => {
                let ty = Type::Prim(expr_vari, expr_prim);

                let value = self.builder.add(expr_value, index_value);

                (ty, Value::Idx(value))
            }
            (Type::ArrayRef(expr_vari, expr_prim), Type::Prim(index_vari, Primitive::Int)) => {
                let ty = Type::Prim(expr_vari | index_vari, expr_prim);

                let two = self.builder.iconst(2);
                let offset_value = self.builder.shl(index_value, two);
                let value = self.builder.add(expr_value, offset_value);

                (ty, Value::Addr(value))
            }
            _ => {
                return Err(Some(format!(
                    "invalid index expression with value type '{}' and index type '{}'",
                    expr_type, index_type
                )));
            }
        };

        Ok((ty, value))
    }

    fn paren_expr(&mut self, expr: &ParenExpr) -> ExprResult {
        Ok(self.expr(&expr.expr)?)
    }

    fn set_predicate_and(&mut self, value: ir::Value) -> Option<ir::Value> {
        let predicate = match self.predicate {
            Some(predicate) => self.builder.and(predicate, value),
            None => value,
        };
        self.set_predicate(predicate)
    }

    fn set_predicate_and_not(&mut self, value: ir::Value) -> Option<ir::Value> {
        let not_value = self.builder.not(value);

        let predicate = match self.predicate {
            Some(predicate) => self.builder.add(predicate, not_value),
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
                    Some(predicate) => self.builder.add(predicate, not_returned),
                    None => not_returned,
                };
                Some(predicate)
            }
            None => predicate,
        };
    }

    fn push_error(&mut self, error: Option<String>, span: Span) -> Option<String> {
        if let Some(error) = error {
            self.errors.push(Error::new(error, span));
        }
        None
    }
}
