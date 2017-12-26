use std::collections::HashMap;

use ast::*;
use ast::Type::*;
use ast::UnaryOp::*;
use ast::BinaryOp::*;

pub fn type_check(prog: &Prog) {
    let mut type_checker = TypeChecker::new();
    type_checker.check_func(prog.func());
}

#[derive(Debug)]
struct Symbol<'input> {
    identifier: Identifier<'input>,
    type_: Type,
}

impl<'input> Symbol<'input> {
    pub fn new(identifier: Identifier<'input>, type_: Type) -> Symbol<'input> {
        Symbol { identifier, type_ }
    }

    pub fn type_(&self) -> Type {
        self.type_
    }
}

#[derive(Debug)]
struct SymbolTable<'a> {
    maps: Vec<HashMap<&'a str, Symbol<'a>>>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> SymbolTable<'a> {
        SymbolTable { maps: Vec::new() }
    }

    pub fn push_scope(&mut self) {
        self.maps.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.maps.pop().unwrap();
    }

    pub fn get(&self, name: &str) -> Option<&Symbol<'a>> {
        for scope in self.maps.iter().rev() {
            let symbol = scope.get(name);
            if symbol.is_some() {
                return symbol;
            }
        }
        None
    }

    pub fn insert(&mut self, name: &'a str, symbol: Symbol<'a>) {
        self.maps.last_mut().unwrap().insert(name, symbol);
    }
}

struct TypeChecker<'input> {
    symbol_table: SymbolTable<'input>,
}

impl<'input> TypeChecker<'input> {
    pub fn new() -> TypeChecker<'input> {
        TypeChecker { symbol_table: SymbolTable::new() }
    }

    fn check_func(&mut self, func: &Func<'input>) {
        self.symbol_table.push_scope();

        for param in func.params() {
            self.insert_symbol(param.identifier(), param.type_());
        }

        self.check_block_stmt(func.stmt());

        self.symbol_table.pop_scope();
    }

    fn check_stmt(&mut self, stmt: &Stmt<'input>) {
        match *stmt {
            Stmt::BlockStmt(ref stmt) => self.check_block_stmt(stmt),
            Stmt::DeclStmt(ref stmt) => self.check_decl_stmt(stmt),
            Stmt::AssignStmt(ref stmt) => self.check_assign_stmt(stmt),
            Stmt::IfStmt(ref stmt) => self.check_if_stmt(stmt),
            Stmt::WhileStmt(ref stmt) => self.check_while_stmt(stmt),
            Stmt::ReturnStmt(_) => {}
        }
    }

    fn check_block_stmt(&mut self, stmt: &BlockStmt<'input>) {
        self.symbol_table.push_scope();

        for stmt in stmt.stmts() {
            self.check_stmt(stmt);
        }

        self.symbol_table.pop_scope();
    }

    fn check_decl_stmt(&mut self, stmt: &DeclStmt<'input>) {
        let expr_type = self.check_expr(stmt.expr());

        let type_ = stmt.type_();

        if type_ != expr_type {
            panic!("Mismatched types '{}' and '{}'", type_, expr_type);
        }

        self.insert_symbol(stmt.identifier(), type_);
    }

    fn check_assign_stmt(&mut self, stmt: &AssignStmt<'input>) {
        let expr_type = self.check_expr(stmt.expr());

        let type_ = self.check_identifier(stmt.identifier());

        if type_ != expr_type {
            panic!("Mismatched types '{}' and '{}'", type_, expr_type);
        }
    }

    fn check_if_stmt(&mut self, stmt: &IfStmt<'input>) {
        let expr_type = self.check_expr(stmt.expr());

        if expr_type != Bool {
            panic!("If statement expression with type '{}'", expr_type);
        }

        self.check_block_stmt(stmt.stmt());
    }

    fn check_while_stmt(&mut self, stmt: &WhileStmt<'input>) {
        let expr_type = self.check_expr(stmt.expr());

        if expr_type != Bool {
            panic!("While statement expression with type '{}'", expr_type);
        }

        self.check_block_stmt(stmt.stmt());
    }

    fn check_expr(&mut self, expr: &Expr) -> Type {
        match *expr {
            Expr::IntLiteral(_) => Int,
            Expr::FloatLiteral(_) => Float,
            Expr::BoolLiteral(_) => Bool,
            Expr::Identifier(ref identifier) => self.check_identifier(identifier),
            Expr::UnaryExpr(ref expr) => self.check_unary_expr(expr),
            Expr::BinaryExpr(ref expr) => self.check_binary_expr(expr),
        }
    }

    fn check_identifier(&mut self, identifier: &Identifier) -> Type {
        self.get_symbol(identifier).type_()
    }

    fn check_unary_expr(&mut self, expr: &UnaryExpr) -> Type {
        let expr_type = self.check_expr(expr.expr());

        let op = expr.op();

        match (op, expr_type) {
            (Negate, Int) | (BitNot, Int) => Int,
            (Negate, Float) => Float,
            (LogicalNot, Bool) => Bool,
            _ => {
                panic!(
                    "Cannot apply unary operator '{}' to type '{}'",
                    op,
                    expr_type
                )
            }
        }
    }

    fn check_binary_expr(&mut self, expr: &BinaryExpr) -> Type {
        let left_type = self.check_expr(expr.left());
        let right_type = self.check_expr(expr.right());

        let op = expr.op();

        match (op, left_type, right_type) {
            (Add, Int, Int) |
            (Sub, Int, Int) |
            (Shl, Int, Int) |
            (Shr, Int, Int) |
            (BitAnd, Int, Int) |
            (BitXor, Int, Int) |
            (BitOr, Int, Int) => Int,
            (Mul, Float, Float) |
            (Add, Float, Float) |
            (Sub, Float, Float) => Float,
            (Eq, Int, Int) |
            (Eq, Float, Float) |
            (Eq, Bool, Bool) |
            (Ne, Int, Int) |
            (Ne, Float, Float) |
            (Ne, Bool, Bool) |
            (Lt, Int, Int) |
            (Lt, Float, Float) |
            (Gt, Int, Int) |
            (Gt, Float, Float) |
            (Le, Int, Int) |
            (Le, Float, Float) |
            (Ge, Int, Int) |
            (Ge, Float, Float) |
            (LogicalAnd, Bool, Bool) |
            (LogicalOr, Bool, Bool) => Bool,
            _ => {
                panic!(
                    "Cannot apply binary operator '{}' to types '{}' and '{}'",
                    op,
                    left_type,
                    right_type
                )
            }
        }
    }

    fn insert_symbol(&mut self, identifier: &Identifier<'input>, type_: Type) {
        let symbol = Symbol::new(*identifier, type_);
        self.symbol_table.insert(identifier.name(), symbol);
    }

    fn get_symbol(&self, identifier: &Identifier) -> &Symbol<'input> {
        self.symbol_table.get(identifier.name()).unwrap()
    }
}
