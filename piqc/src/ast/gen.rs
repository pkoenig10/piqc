use std::collections::HashMap;

use crate::ast::*;
use crate::ir;
use crate::util::Generator;

pub fn generate_ir(func: &Func) -> ir::Func {
    IrGenerator::new().func(func).unwrap()
}

enum Place {
    Variable(ir::Variable),
    Addr(ir::Value),
}

type SymbolData = Result<(Type, ir::Variable), ()>;

#[derive(Debug)]
struct SymbolTable {
    scopes: Vec<HashMap<Symbol, SymbolData>>,
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

    fn get(&self, symbol: Symbol) -> Option<&SymbolData> {
        self.scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.get(&symbol))
            .nth(0)
    }

    fn insert_ok(&mut self, symbol: Symbol, type_: Type) -> ir::Variable {
        let variable = self.generator.next();
        self.insert(symbol, Ok((type_, variable)));
        variable
    }

    fn insert_err(&mut self, symbol: Symbol) {
        self.insert(symbol, Err(()));
    }

    fn insert(&mut self, symbol: Symbol, data: SymbolData) {
        self.scopes
            .last_mut()
            .expect("Failed to get current scope from symbol")
            .insert(symbol, data);
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
            if let Ok((type_, variable)) = self.param(param) {
                self.builder.push_param(type_);
                self.builder.push_ebb_param(variable, entry_ebb, type_);
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

    fn param(&mut self, param: &Param) -> Result<(Type, ir::Variable), ()> {
        match param.type_.qualifier {
            TypeQualifier::Uniform => {
                let variable = self.symbols.insert_ok(param.identifier.symbol, param.type_);
                Ok((param.type_, variable))
            }
            _ => {
                self.symbols.insert_err(param.identifier.symbol);
                self.errors
                    .push(format!("Invalid parameter with type '{}'", param.type_));
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
        }
    }

    fn block_stmt(&mut self, stmt: &BlockStmt) {
        self.symbols.push_scope();

        for stmt in &stmt.stmts {
            self.stmt(stmt);
        }

        self.symbols.pop_scope();
    }

    fn decl_stmt(&mut self, stmt: &DeclStmt) {
        let expr_result = self.expr(&stmt.expr);

        let result = fn_block!({
            let (expr_type, expr_value) = expr_result?;

            if !is_valid_assign(stmt.type_, expr_type) {
                self.errors.push(format!(
                    "Mismatched types '{}' and '{}'",
                    stmt.type_, expr_type
                ));
                return Err(());
            }

            Ok((stmt.type_, expr_value))
        });

        match result {
            Ok((type_, value)) => {
                let variable = self.symbols.insert_ok(stmt.identifier.symbol, type_);
                self.builder.def_var(variable, value);
            }
            Err(_) => {
                self.symbols.insert_err(stmt.identifier.symbol);
            }
        };
    }

    fn assign_stmt(&mut self, stmt: &AssignStmt) {
        let src_result = self.expr(&stmt.src);
        let dest_result = self.expr_place(&stmt.dest);

        let result = fn_block!({
            let (src_type, src_value) = src_result?;
            let (dest_type, dest_place) = dest_result?;

            if !is_valid_assign(dest_type, src_type) {
                self.errors.push(format!(
                    "Mismatched types '{}' and '{}'",
                    dest_type, src_type
                ));
                return Err(());
            }

            Ok((dest_place, src_value))
        });

        match result {
            Ok((Place::Variable(variable), src_value)) => {
                let src_value = match self.predicate {
                    Some(predicate) => {
                        let prev_value = self.builder.use_var(variable);
                        self.builder
                            .push_select_inst(predicate, src_value, prev_value)
                    }
                    None => src_value,
                };

                self.builder.def_var(variable, src_value);
            }
            Ok((Place::Addr(addr_value), src_value)) => {
                self.builder.push_store_inst(src_value, addr_value);
            }
            Err(_) => {}
        };
    }

    fn if_stmt(&mut self, stmt: &IfStmt) {
        let cond_result = self.expr(&stmt.cond);

        let result = fn_block!({
            let (cond_type, cond_value) = cond_result?;

            if cond_type.kind != TypeKind::BOOL {
                self.errors.push(format!(
                    "Invalid if statement condition with type '{}'",
                    cond_type
                ));
                return Err(());
            }

            Ok((cond_type, cond_value))
        });

        match result {
            Ok((cond_type, cond_value)) => match cond_type.qualifier {
                TypeQualifier::Uniform => {
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
                TypeQualifier::Varying => {
                    let prev_predicate = self.set_predicate_and(cond_value);

                    self.stmt(&stmt.if_stmt);
                    self.reset_predicate(prev_predicate);

                    if let Some(ref else_stmt) = stmt.else_stmt {
                        self.set_predicate_and_not(cond_value);

                        self.stmt(else_stmt);
                        self.reset_predicate(prev_predicate);
                    }
                }
            },
            Err(_) => {}
        };
    }

    fn while_stmt(&mut self, stmt: &WhileStmt) {
        let header_ebb = self.builder.create_ebb();
        let after_ebb = self.builder.create_ebb();

        self.builder.push_jump_inst(header_ebb);
        self.builder.set_position(header_ebb);

        let cond_result = self.expr(&stmt.cond);

        let result = fn_block!({
            let (cond_type, cond_value) = cond_result?;

            if cond_type.kind != TypeKind::BOOL {
                self.errors.push(format!(
                    "Invalid while statement condition with type '{}'",
                    cond_type
                ));
                return Err(());
            }

            Ok((cond_type, cond_value))
        });

        match result {
            Ok((cond_type, cond_value)) => match cond_type.qualifier {
                TypeQualifier::Uniform => {
                    self.builder
                        .push_branch_inst(ir::BranchOp::AllFalse, cond_value, after_ebb);

                    self.stmt(&stmt.stmt);
                    self.builder.push_jump_inst(header_ebb);

                    self.builder.set_position(after_ebb);
                }
                TypeQualifier::Varying => {
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
            },
            Err(_) => {}
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

    fn expr(&mut self, expr: &Expr) -> Result<(Type, ir::Value), ()> {
        match expr.kind {
            ExprKind::Int(ref expr) => self.int_expr(expr),
            ExprKind::Float(ref expr) => self.float_expr(expr),
            ExprKind::Bool(ref expr) => self.bool_expr(expr),
            ExprKind::Element(_) => self.element_expr(),
            ExprKind::Count(_) => self.count_expr(),
            ExprKind::Identifier(ref expr) => self.identifier_expr_value(expr),
            ExprKind::Unary(ref expr) => self.unary_expr(expr),
            ExprKind::Binary(ref expr) => self.binary_expr(expr),
            ExprKind::Index(ref expr) => self.index_expr_value(expr),
            ExprKind::Paren(ref expr) => self.paren_expr(expr),
        }
    }

    fn expr_place(&mut self, expr: &Expr) -> Result<(Type, Place), ()> {
        match expr.kind {
            ExprKind::Identifier(ref expr) => self.identifier_expr_place(expr),
            ExprKind::Index(ref expr) => self.index_expr_place(expr),
            _ => {
                self.errors.push(format!("Invalid place expression"));
                Err(())
            }
        }
    }

    fn int_expr(&mut self, expr: &IntExpr) -> Result<(Type, ir::Value), ()> {
        let value = self.builder.push_int_const_inst(expr.value);
        Ok((Type::UNIFORM_INT, value))
    }

    fn float_expr(&mut self, expr: &FloatExpr) -> Result<(Type, ir::Value), ()> {
        let value = self.builder.push_float_const_inst(expr.value);
        Ok((Type::UNIFORM_FLOAT, value))
    }

    fn bool_expr(&mut self, expr: &BoolExpr) -> Result<(Type, ir::Value), ()> {
        let value = self.builder.push_bool_const_inst(expr.value);
        Ok((Type::UNIFORM_BOOL, value))
    }

    fn element_expr(&mut self) -> Result<(Type, ir::Value), ()> {
        let value = self.builder.push_element_inst();
        Ok((Type::VARYING_INT, value))
    }

    fn count_expr(&mut self) -> Result<(Type, ir::Value), ()> {
        let value = self.builder.push_count_inst();
        Ok((Type::UNIFORM_INT, value))
    }

    fn identifier_expr_value(&mut self, expr: &IdentifierExpr) -> Result<(Type, ir::Value), ()> {
        self.identifier_expr(expr).map(|(type_, variable)| {
            let value = self.builder.use_var(variable);
            (type_, value)
        })
    }

    fn identifier_expr_place(&mut self, expr: &IdentifierExpr) -> Result<(Type, Place), ()> {
        self.identifier_expr(expr)
            .map(|(type_, variable)| (type_, Place::Variable(variable)))
    }

    fn identifier_expr(&mut self, expr: &IdentifierExpr) -> Result<(Type, ir::Variable), ()> {
        match self.symbols.get(expr.identifier.symbol) {
            Some(&result) => result,
            None => {
                self.errors
                    .push(format!("Cannot find variable '{}'", expr.identifier.symbol));
                Err(())
            }
        }
    }

    fn unary_expr(&mut self, expr: &UnaryExpr) -> Result<(Type, ir::Value), ()> {
        let expr_result = self.expr(&expr.expr);

        let (expr_type, expr_value) = expr_result?;

        let result = fn_block!({
            let (type_, value) = match (expr.op, expr_type.kind) {
                (UnaryOp::Negate, TypeKind::INT) => {
                    let zero = self.builder.push_int_const_inst(0);
                    let value = self
                        .builder
                        .push_binary_inst(ir::BinaryOp::Sub, zero, expr_value);
                    (expr_type, value)
                }
                (UnaryOp::Negate, TypeKind::FLOAT) => {
                    let zero = self.builder.push_float_const_inst(0.);
                    let value = self
                        .builder
                        .push_binary_inst(ir::BinaryOp::Fsub, zero, expr_value);
                    (expr_type, value)
                }
                (UnaryOp::Not, TypeKind::INT) => {
                    let value = self.builder.push_unary_inst(ir::UnaryOp::Not, expr_value);
                    (expr_type, value)
                }
                (UnaryOp::Not, TypeKind::BOOL) => {
                    let one = self.builder.push_bool_const_inst(true);
                    let value = self
                        .builder
                        .push_binary_inst(ir::BinaryOp::Xor, one, expr_value);
                    (expr_type, value)
                }
                _ => return Err(()),
            };

            Ok((type_, value))
        });

        if let Err(_) = result {
            self.errors.push(format!(
                "Invalid unary expression '{}' with operand type '{}'",
                expr.op, expr_type
            ));
        }

        result
    }

    fn binary_expr(&mut self, expr: &BinaryExpr) -> Result<(Type, ir::Value), ()> {
        let left_result = self.expr(&expr.left);
        let right_result = self.expr(&expr.right);

        let (left_type, left_value) = left_result?;
        let (right_type, right_value) = right_result?;

        let result = fn_block!({
            if left_type.kind != right_type.kind {
                return Err(());
            }

            let (kind, value) = match binary_kind(expr.op, left_type.kind) {
                BinaryKind::Binary(op) => {
                    let value = self.builder.push_binary_inst(op, left_value, right_value);
                    (left_type.kind, value)
                }
                BinaryKind::IntComp(op) => {
                    let value = self.builder.push_int_comp_inst(op, left_value, right_value);
                    (TypeKind::BOOL, value)
                }
                BinaryKind::FloatComp(op) => {
                    let value = self
                        .builder
                        .push_float_comp_inst(op, left_value, right_value);
                    (TypeKind::BOOL, value)
                }
                BinaryKind::None => {
                    return Err(());
                }
            };

            let qualifier = TypeQualifier::get(left_type.qualifier, right_type.qualifier);
            let type_ = Type::new(qualifier, kind);

            Ok((type_, value))
        });

        if let Err(_) = result {
            self.errors.push(format!(
                "Invalid binary expression '{}' with operand types '{}' and '{}'",
                expr.op, left_type, right_type
            ));
        }

        result
    }

    fn index_expr_value(&mut self, expr: &IndexExpr) -> Result<(Type, ir::Value), ()> {
        self.index_expr(expr).map(|(type_, value)| {
            let value = self.builder.push_fetch_inst(value);
            (type_, value)
        })
    }

    fn index_expr_place(&mut self, expr: &IndexExpr) -> Result<(Type, Place), ()> {
        self.index_expr(expr)
            .map(|(type_, value)| (type_, Place::Addr(value)))
    }

    fn index_expr(&mut self, expr: &IndexExpr) -> Result<(Type, ir::Value), ()> {
        let expr_result = self.expr(&expr.expr);
        let index_result = self.expr(&expr.index);

        let (expr_type, expr_value) = expr_result?;
        let (index_type, index_value) = index_result?;

        let result = fn_block!({
            let (kind, value) = match (expr_type.kind, index_type.kind) {
                (TypeKind::Ptr(type_), TypeKind::INT) => {
                    let two = self.builder.push_int_const_inst(2);
                    let offset_value =
                        self.builder
                            .push_binary_inst(ir::BinaryOp::Shl, index_value, two);
                    let value =
                        self.builder
                            .push_binary_inst(ir::BinaryOp::Add, expr_value, offset_value);

                    (type_.deref(), value)
                }
                _ => {
                    return Err(());
                }
            };

            let qualifier = TypeQualifier::get(expr_type.qualifier, index_type.qualifier);
            let type_ = Type::new(qualifier, kind);

            Ok((type_, value))
        });

        if let Err(_) = result {
            self.errors.push(format!(
                "Invalid index expression with value type '{}' and index type '{}'",
                expr_type, index_type
            ));
        }

        result
    }

    fn paren_expr(&mut self, expr: &ParenExpr) -> Result<(Type, ir::Value), ()> {
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

fn is_valid_assign(variable_type: Type, expr_type: Type) -> bool {
    if let (TypeQualifier::Uniform, TypeQualifier::Varying) =
        (variable_type.qualifier, expr_type.qualifier)
    {
        return false;
    }

    if variable_type.kind != expr_type.kind {
        return false;
    }

    true
}

enum BinaryKind {
    Binary(ir::BinaryOp),
    IntComp(ir::CompOp),
    FloatComp(ir::CompOp),
    None,
}

fn binary_kind(op: BinaryOp, kind: TypeKind) -> BinaryKind {
    match (op, kind) {
        (BinaryOp::Mul, TypeKind::FLOAT) => BinaryKind::Binary(ir::BinaryOp::Fmul),
        (BinaryOp::Add, TypeKind::INT) => BinaryKind::Binary(ir::BinaryOp::Add),
        (BinaryOp::Add, TypeKind::FLOAT) => BinaryKind::Binary(ir::BinaryOp::Fadd),
        (BinaryOp::Sub, TypeKind::INT) => BinaryKind::Binary(ir::BinaryOp::Sub),
        (BinaryOp::Sub, TypeKind::FLOAT) => BinaryKind::Binary(ir::BinaryOp::Fsub),
        (BinaryOp::Shl, TypeKind::INT) => BinaryKind::Binary(ir::BinaryOp::Shl),
        (BinaryOp::Shr, TypeKind::INT) => BinaryKind::Binary(ir::BinaryOp::Asr),
        (BinaryOp::Min, TypeKind::INT) => BinaryKind::Binary(ir::BinaryOp::Min),
        (BinaryOp::Min, TypeKind::FLOAT) => BinaryKind::Binary(ir::BinaryOp::Fmin),
        (BinaryOp::Max, TypeKind::INT) => BinaryKind::Binary(ir::BinaryOp::Max),
        (BinaryOp::Max, TypeKind::FLOAT) => BinaryKind::Binary(ir::BinaryOp::Fmax),
        (BinaryOp::BitAnd, TypeKind::INT) => BinaryKind::Binary(ir::BinaryOp::And),
        (BinaryOp::LogicalAnd, TypeKind::BOOL) => BinaryKind::Binary(ir::BinaryOp::And),
        (BinaryOp::BitOr, TypeKind::INT) => BinaryKind::Binary(ir::BinaryOp::Or),
        (BinaryOp::LogicalOr, TypeKind::BOOL) => BinaryKind::Binary(ir::BinaryOp::Or),
        (BinaryOp::BitXor, TypeKind::INT) => BinaryKind::Binary(ir::BinaryOp::Xor),
        (BinaryOp::Eq, TypeKind::INT) => BinaryKind::IntComp(ir::CompOp::Eq),
        (BinaryOp::Eq, TypeKind::FLOAT) => BinaryKind::FloatComp(ir::CompOp::Eq),
        (BinaryOp::Ne, TypeKind::INT) => BinaryKind::IntComp(ir::CompOp::Ne),
        (BinaryOp::Ne, TypeKind::FLOAT) => BinaryKind::FloatComp(ir::CompOp::Ne),
        (BinaryOp::Lt, TypeKind::INT) => BinaryKind::IntComp(ir::CompOp::Lt),
        (BinaryOp::Lt, TypeKind::FLOAT) => BinaryKind::FloatComp(ir::CompOp::Lt),
        (BinaryOp::Gt, TypeKind::INT) => BinaryKind::IntComp(ir::CompOp::Gt),
        (BinaryOp::Gt, TypeKind::FLOAT) => BinaryKind::FloatComp(ir::CompOp::Gt),
        (BinaryOp::Le, TypeKind::INT) => BinaryKind::IntComp(ir::CompOp::Le),
        (BinaryOp::Le, TypeKind::FLOAT) => BinaryKind::FloatComp(ir::CompOp::Le),
        (BinaryOp::Ge, TypeKind::INT) => BinaryKind::IntComp(ir::CompOp::Ge),
        (BinaryOp::Ge, TypeKind::FLOAT) => BinaryKind::FloatComp(ir::CompOp::Ge),
        _ => BinaryKind::None,
    }
}
