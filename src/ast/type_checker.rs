use std::collections::HashMap;

use ast::*;

pub fn type_check(prog: &Prog) {
    let mut type_checker = TypeChecker::new();
    type_checker.check_func(&prog.func);
}

#[derive(Debug)]
struct Symbol<'a> {
    identifier: Identifier<'a>,
    type_: Type,
}

impl<'a> Symbol<'a> {
    fn new(identifier: Identifier<'a>, type_: Type) -> Symbol<'a> {
        Symbol { identifier, type_ }
    }

    fn type_(&self) -> Type {
        self.type_
    }
}

#[derive(Debug)]
struct SymbolTable<'a> {
    scopes: Vec<HashMap<&'a str, Symbol<'a>>>,
}

impl<'a> SymbolTable<'a> {
    fn new() -> SymbolTable<'a> {
        SymbolTable { scopes: Vec::new() }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    fn get(&self, name: &str) -> Option<&Symbol<'a>> {
        for scope in self.scopes.iter().rev() {
            let symbol = scope.get(name);
            if symbol.is_some() {
                return symbol;
            }
        }
        None
    }

    fn insert(&mut self, name: &'a str, symbol: Symbol<'a>) {
        self.scopes.last_mut().unwrap().insert(name, symbol);
    }
}

struct TypeChecker<'input> {
    symbols: SymbolTable<'input>,
}

impl<'input> TypeChecker<'input> {
    fn new() -> TypeChecker<'input> {
        TypeChecker {
            symbols: SymbolTable::new(),
        }
    }

    fn check_func(&mut self, func: &Func<'input>) {
        self.symbols.push_scope();

        for param in &func.params {
            self.check_param(param);
        }

        self.check_stmt(&func.stmt);

        self.symbols.pop_scope();
    }

    fn check_param(&mut self, param: &Param<'input>) {
        let type_ = param.type_;

        assert_eq!(
            type_.qualifier,
            TypeQualifier::Uniform,
            "Parameter with type '{}'",
            type_
        );

        self.insert_symbol(&param.identifier, param.type_);
    }

    fn check_stmt(&mut self, stmt: &Stmt<'input>) {
        match *stmt {
            Stmt::Block(ref stmt) => self.check_block_stmt(stmt),
            Stmt::Decl(ref stmt) => self.check_decl_stmt(stmt),
            Stmt::Assign(ref stmt) => self.check_assign_stmt(stmt),
            Stmt::If(ref stmt) => self.check_if_stmt(stmt),
            Stmt::While(ref stmt) => self.check_while_stmt(stmt),
            Stmt::Return(_) => {}
        }
    }

    fn check_block_stmt(&mut self, stmt: &BlockStmt<'input>) {
        self.symbols.push_scope();

        for stmt in &stmt.stmts {
            self.check_stmt(stmt);
        }

        self.symbols.pop_scope();
    }

    fn check_decl_stmt(&mut self, stmt: &DeclStmt<'input>) {
        let expr_type = self.check_expr(&stmt.expr);
        let type_ = stmt.type_;

        let valid = match (type_.qualifier, expr_type.qualifier) {
            (TypeQualifier::Uniform, TypeQualifier::Varying) => false,
            _ => type_.kind == expr_type.kind,
        };
        assert!(valid, "Mismatched types '{}' and '{}'", type_, expr_type);

        self.insert_symbol(&stmt.identifier, type_);
    }

    fn check_assign_stmt(&mut self, stmt: &AssignStmt<'input>) {
        let expr_type = self.check_expr(&stmt.expr);
        let type_ = self.check_identifier(&stmt.identifier);

        let valid = match (type_.qualifier, expr_type.qualifier) {
            (TypeQualifier::Uniform, TypeQualifier::Varying) => false,
            _ => type_.kind == expr_type.kind,
        };
        assert!(valid, "Mismatched types '{}' and '{}'", type_, expr_type);
    }

    fn check_if_stmt(&mut self, stmt: &IfStmt<'input>) {
        let expr_type = self.check_expr(&stmt.expr);

        assert_eq!(
            expr_type.kind,
            TypeKind::BOOL,
            "If statement expression with type '{}'",
            expr_type
        );

        self.check_stmt(&stmt.if_stmt);

        if let Some(else_stmt) = &stmt.else_stmt {
            self.check_stmt(else_stmt);
        }
    }

    fn check_while_stmt(&mut self, stmt: &WhileStmt<'input>) {
        let expr_type = self.check_expr(&stmt.expr);
        assert_eq!(
            expr_type.kind,
            TypeKind::BOOL,
            "While statement expression with type '{}'",
            expr_type
        );

        self.check_stmt(&stmt.stmt);
    }

    fn check_expr(&mut self, expr: &Expr) -> Type {
        match *expr {
            Expr::IntLiteral(_) | Expr::Count(_) => Type::UNIFORM_INT,
            Expr::Index(_) => Type::VARYING_INT,
            Expr::FloatLiteral(_) => Type::UNIFORM_FLOAT,
            Expr::BoolLiteral(_) => Type::UNIFORM_BOOL,
            Expr::Identifier(ref identifier) => self.check_identifier(identifier),
            Expr::Unary(ref expr) => self.check_unary_expr(expr),
            Expr::Binary(ref expr) => self.check_binary_expr(expr),
        }
    }

    fn check_identifier(&mut self, identifier: &Identifier) -> Type {
        self.get_symbol(identifier).type_()
    }

    fn check_unary_expr(&mut self, expr: &UnaryExpr) -> Type {
        let expr_type = self.check_expr(&expr.expr);

        let kind = match (expr.op, expr_type.kind) {
            (UnaryOp::Negate, TypeKind::INT) | (UnaryOp::BitNot, TypeKind::INT) => TypeKind::INT,
            (UnaryOp::Negate, TypeKind::FLOAT) => TypeKind::FLOAT,
            (UnaryOp::LogicalNot, TypeKind::BOOL) => TypeKind::BOOL,
            _ => panic!(
                "Cannot apply unary operator '{}' to type '{}'",
                expr.op, expr_type
            ),
        };

        Type::new(expr_type.qualifier, kind)
    }

    fn check_binary_expr(&mut self, expr: &BinaryExpr) -> Type {
        let left_type = self.check_expr(&expr.left);
        let right_type = self.check_expr(&expr.right);

        let qualifier = get_type_qualifier(left_type.qualifier, right_type.qualifier);

        let kind = match (expr.op, left_type.kind, right_type.kind) {
            (BinaryOp::Add, TypeKind::INT, TypeKind::INT)
            | (BinaryOp::Sub, TypeKind::INT, TypeKind::INT)
            | (BinaryOp::Shl, TypeKind::INT, TypeKind::INT)
            | (BinaryOp::Shr, TypeKind::INT, TypeKind::INT)
            | (BinaryOp::BitAnd, TypeKind::INT, TypeKind::INT)
            | (BinaryOp::BitXor, TypeKind::INT, TypeKind::INT)
            | (BinaryOp::BitOr, TypeKind::INT, TypeKind::INT)
            | (BinaryOp::Min, TypeKind::INT, TypeKind::INT)
            | (BinaryOp::Max, TypeKind::INT, TypeKind::INT) => TypeKind::INT,
            (BinaryOp::Mul, TypeKind::FLOAT, TypeKind::FLOAT)
            | (BinaryOp::Add, TypeKind::FLOAT, TypeKind::FLOAT)
            | (BinaryOp::Sub, TypeKind::FLOAT, TypeKind::FLOAT)
            | (BinaryOp::Min, TypeKind::FLOAT, TypeKind::FLOAT)
            | (BinaryOp::Max, TypeKind::FLOAT, TypeKind::FLOAT) => TypeKind::FLOAT,
            (BinaryOp::Eq, TypeKind::INT, TypeKind::INT)
            | (BinaryOp::Eq, TypeKind::FLOAT, TypeKind::FLOAT)
            | (BinaryOp::Eq, TypeKind::BOOL, TypeKind::BOOL)
            | (BinaryOp::Ne, TypeKind::INT, TypeKind::INT)
            | (BinaryOp::Ne, TypeKind::FLOAT, TypeKind::FLOAT)
            | (BinaryOp::Ne, TypeKind::BOOL, TypeKind::BOOL)
            | (BinaryOp::Lt, TypeKind::INT, TypeKind::INT)
            | (BinaryOp::Lt, TypeKind::FLOAT, TypeKind::FLOAT)
            | (BinaryOp::Gt, TypeKind::INT, TypeKind::INT)
            | (BinaryOp::Gt, TypeKind::FLOAT, TypeKind::FLOAT)
            | (BinaryOp::Le, TypeKind::INT, TypeKind::INT)
            | (BinaryOp::Le, TypeKind::FLOAT, TypeKind::FLOAT)
            | (BinaryOp::Ge, TypeKind::INT, TypeKind::INT)
            | (BinaryOp::Ge, TypeKind::FLOAT, TypeKind::FLOAT)
            | (BinaryOp::LogicalAnd, TypeKind::BOOL, TypeKind::BOOL)
            | (BinaryOp::LogicalOr, TypeKind::BOOL, TypeKind::BOOL) => TypeKind::BOOL,
            _ => panic!(
                "Cannot apply binary operator '{}' to types '{}' and '{}'",
                expr.op, left_type, right_type
            ),
        };

        Type::new(qualifier, kind)
    }

    fn insert_symbol(&mut self, identifier: &Identifier<'input>, type_: Type) {
        let symbol = Symbol::new(*identifier, type_);
        self.symbols.insert(identifier.name, symbol);
    }

    fn get_symbol(&self, identifier: &Identifier) -> &Symbol<'input> {
        self.symbols.get(identifier.name).unwrap()
    }
}
