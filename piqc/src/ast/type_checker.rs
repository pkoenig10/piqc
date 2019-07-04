use std::collections::HashMap;

use crate::ast::*;

pub fn type_check(func: &Func) {
    TypeChecker::new().check_func(func);
}

#[derive(Debug)]
struct Symbol {
    identifier: Identifier,
    type_: Type,
}

impl Symbol {
    fn new(identifier: Identifier, type_: Type) -> Symbol {
        Symbol { identifier, type_ }
    }

    fn type_(&self) -> Type {
        self.type_
    }
}

#[derive(Debug)]
struct SymbolTable {
    scopes: Vec<HashMap<Variable, Symbol>>,
}

impl SymbolTable {
    fn new() -> SymbolTable {
        SymbolTable { scopes: Vec::new() }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    fn get(&self, variable: Variable) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            let symbol = scope.get(&variable);
            if symbol.is_some() {
                return symbol;
            }
        }
        None
    }

    fn insert(&mut self, variable: Variable, symbol: Symbol) {
        self.scopes.last_mut().unwrap().insert(variable, symbol);
    }
}

struct TypeChecker {
    symbols: SymbolTable,
}

impl TypeChecker {
    fn new() -> TypeChecker {
        TypeChecker {
            symbols: SymbolTable::new(),
        }
    }

    fn check_func(&mut self, func: &Func) {
        self.symbols.push_scope();

        for param in &func.params {
            self.check_param(param);
        }

        self.check_stmt(&func.stmt);

        self.symbols.pop_scope();
    }

    fn check_param(&mut self, param: &Param) {
        let type_ = param.type_;

        assert_eq!(
            type_.qualifier,
            TypeQualifier::Uniform,
            "Parameter with type '{}'",
            type_
        );

        self.insert_symbol(&param.identifier, param.type_);
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt.kind {
            StmtKind::Block(ref stmt) => self.check_block_stmt(stmt),
            StmtKind::Decl(ref stmt) => self.check_decl_stmt(stmt),
            StmtKind::Assign(ref stmt) => self.check_assign_stmt(stmt),
            StmtKind::If(ref stmt) => self.check_if_stmt(stmt),
            StmtKind::While(ref stmt) => self.check_while_stmt(stmt),
            StmtKind::Return(_) => {}
        }
    }

    fn check_block_stmt(&mut self, stmt: &BlockStmt) {
        self.symbols.push_scope();

        for stmt in &stmt.stmts {
            self.check_stmt(stmt);
        }

        self.symbols.pop_scope();
    }

    fn check_decl_stmt(&mut self, stmt: &DeclStmt) {
        let expr_type = self.check_expr(&stmt.expr);
        let type_ = stmt.type_;

        let valid = match (type_.qualifier, expr_type.qualifier) {
            (TypeQualifier::Uniform, TypeQualifier::Varying) => false,
            _ => type_.kind == expr_type.kind,
        };
        assert!(valid, "Mismatched types '{}' and '{}'", type_, expr_type);

        self.insert_symbol(&stmt.identifier, type_);
    }

    fn check_assign_stmt(&mut self, stmt: &AssignStmt) {
        let src_type = self.check_expr(&stmt.src);
        let dest_type = self.check_expr(&stmt.dest);

        let valid = match (dest_type.qualifier, src_type.qualifier) {
            (TypeQualifier::Uniform, TypeQualifier::Varying) => false,
            _ => dest_type.kind == src_type.kind,
        };
        assert!(valid, "Mismatched types '{}' and '{}'", dest_type, src_type);
    }

    fn check_if_stmt(&mut self, stmt: &IfStmt) {
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

    fn check_while_stmt(&mut self, stmt: &WhileStmt) {
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
        match expr.kind {
            ExprKind::Int(_) | ExprKind::Count(_) => Type::UNIFORM_INT,
            ExprKind::Element(_) => Type::VARYING_INT,
            ExprKind::Float(_) => Type::UNIFORM_FLOAT,
            ExprKind::Bool(_) => Type::UNIFORM_BOOL,
            ExprKind::Identifier(ref expr) => self.check_identifier_expr(expr),
            ExprKind::Unary(ref expr) => self.check_unary_expr(expr),
            ExprKind::Binary(ref expr) => self.check_binary_expr(expr),
            ExprKind::Index(ref expr) => self.check_index_expr(expr),
            ExprKind::Paren(ref expr) => self.check_paren_expr(expr),
        }
    }

    fn check_identifier_expr(&mut self, expr: &IdentifierExpr) -> Type {
        self.get_symbol(&expr.identifier).type_()
    }

    fn check_unary_expr(&mut self, expr: &UnaryExpr) -> Type {
        let expr_type = self.check_expr(&expr.expr);

        let kind = match (expr.op, expr_type.kind) {
            (UnaryOp::Negate, TypeKind::INT) | (UnaryOp::BitNot, TypeKind::INT) => TypeKind::INT,
            (UnaryOp::Negate, TypeKind::FLOAT) => TypeKind::FLOAT,
            (UnaryOp::LogicalNot, TypeKind::BOOL) => TypeKind::BOOL,
            _ => panic!(
                "Can't apply unary operator '{}' to type '{}'",
                expr.op, expr_type
            ),
        };

        Type::new(expr_type.qualifier, kind)
    }

    fn check_binary_expr(&mut self, expr: &BinaryExpr) -> Type {
        let left_type = self.check_expr(&expr.left);
        let right_type = self.check_expr(&expr.right);

        let qualifier = TypeQualifier::get(left_type.qualifier, right_type.qualifier);

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
                "Can't apply binary operator '{}' to types '{}' and '{}'",
                expr.op, left_type, right_type
            ),
        };

        Type::new(qualifier, kind)
    }

    fn check_index_expr(&mut self, expr: &IndexExpr) -> Type {
        let expr_type = self.check_expr(&expr.expr);
        let index_type = self.check_expr(&expr.index);

        let qualifier = TypeQualifier::get(expr_type.qualifier, index_type.qualifier);

        let expr_type = match expr_type.kind {
            TypeKind::Ptr(ty) => ty,
            _ => panic!("Can't index type '{}'", expr_type),
        };

        assert_eq!(
            index_type.kind,
            TypeKind::INT,
            "Can't index using type '{}'",
            index_type
        );

        Type::new(qualifier, expr_type.deref())
    }

    fn check_paren_expr(&mut self, expr: &ParenExpr) -> Type {
        self.check_expr(&expr.expr)
    }

    fn insert_symbol(&mut self, identifier: &Identifier, type_: Type) {
        let symbol = Symbol::new(*identifier, type_);
        self.symbols.insert(identifier.variable, symbol);
    }

    fn get_symbol(&self, identifier: &Identifier) -> &Symbol {
        self.symbols.get(identifier.variable).unwrap()
    }
}
