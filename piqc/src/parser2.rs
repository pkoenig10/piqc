use crate::ast;
use crate::collections::InternMap;
use crate::error::Error;
use crate::Span;
use logos::Logos;
use std::fmt;

impl From<logos::Span> for Span {
    fn from(span: logos::Span) -> Span {
        Span::new(span.start, span.end)
    }
}

enum Associativity {
    Left,
    Right,
}

impl ast::BinaryOp {
    fn precedence(&self) -> u8 {
        match self {
            ast::BinaryOp::Mul => 10,
            ast::BinaryOp::Add | ast::BinaryOp::Sub => 9,
            ast::BinaryOp::Shl | ast::BinaryOp::Shr => 8,
            ast::BinaryOp::BitAnd => 7,
            ast::BinaryOp::BitXor => 6,
            ast::BinaryOp::BitOr => 5,
            ast::BinaryOp::Min | ast::BinaryOp::Max => 4,
            ast::BinaryOp::Eq
            | ast::BinaryOp::Ne
            | ast::BinaryOp::Lt
            | ast::BinaryOp::Gt
            | ast::BinaryOp::Le
            | ast::BinaryOp::Ge => 3,
            ast::BinaryOp::LogicalAnd => 2,
            ast::BinaryOp::LogicalOr => 1,
        }
    }

    fn associativity(&self) -> Associativity {
        match self {
            ast::BinaryOp::Mul
            | ast::BinaryOp::Add
            | ast::BinaryOp::Sub
            | ast::BinaryOp::Shl
            | ast::BinaryOp::Shr
            | ast::BinaryOp::BitAnd
            | ast::BinaryOp::BitXor
            | ast::BinaryOp::BitOr
            | ast::BinaryOp::Min
            | ast::BinaryOp::Max
            | ast::BinaryOp::Eq
            | ast::BinaryOp::Ne
            | ast::BinaryOp::Lt
            | ast::BinaryOp::Gt
            | ast::BinaryOp::Le
            | ast::BinaryOp::Ge
            | ast::BinaryOp::LogicalAnd
            | ast::BinaryOp::LogicalOr => Associativity::Left,
        }
    }
}

#[derive(Logos, Clone, Copy, Debug, PartialEq)]
enum Token {
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("return")]
    Return,
    #[token("uniform")]
    Uniform,
    #[token("varying")]
    Varying,

    #[regex(r"\d+")]
    Int,
    #[regex(r"\d+\.\d*")]
    Float,
    #[regex("true|false")]
    Bool,
    #[token("@count")]
    Count,
    #[token("@element")]
    Element,
    #[regex("[A-Za-z_][0-9A-Za-z_]*")]
    Identifier,

    #[token(";")]
    Semi,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,

    #[token("=")]
    Assign,
    #[token("!")]
    Not,
    #[token("*")]
    Mul,
    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("<<")]
    Shl,
    #[token(">>")]
    Shr,
    #[token("&")]
    BitAnd,
    #[token("|")]
    BitOr,
    #[token("^")]
    BitXor,
    #[token("<?")]
    Min,
    #[token(">?")]
    Max,
    #[token("==")]
    Eq,
    #[token("!=")]
    Ne,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,
    #[token("&&")]
    LogicalAnd,
    #[token("||")]
    LogicalOr,

    #[error]
    #[regex(r"[ \t\n\r\f\v]+", logos::skip)]
    Error,

    Start,
    End,
}

type ParseResult<T> = Result<T, ()>;

pub struct Parser<'source> {
    lex: logos::Lexer<'source, Token>,
    symbols: InternMap<ast::Symbol, &'source str>,
    errors: Vec<Error>,
    token: Token,
}

impl<'source> Parser<'source> {
    pub fn new(source: &'source str) -> Parser<'source> {
        let mut parser = Parser {
            lex: Token::lexer(source),
            symbols: InternMap::new(),
            errors: Vec::new(),
            token: Token::Start,
        };

        parser.next();

        return parser;
    }

    pub fn parse_func(&mut self) -> Result<ast::Func, ()> {
        let start = self.span();

        self.parse_token(Token::Fn)?;
        let identifier = self.parse_identifier()?;
        let params = self.parse_params()?;
        let stmt = self.parse_block()?;

        let end = self.span();
        let span = start.to(end);

        Ok(ast::Func::new(span, identifier, params, stmt))
    }

    fn parse_params(&mut self) -> ParseResult<Vec<ast::Param>> {
        self.parse_seq(
            Token::OpenParen,
            Token::CloseParen,
            Some(Token::Comma),
            Parser::parse_param,
        )
    }

    fn parse_param(&mut self) -> ParseResult<ast::Param> {
        let start = self.span();

        let identifier = self.parse_identifier()?;
        self.parse_token(Token::Colon)?;
        let ty = self.parse_type()?;

        let end = self.span();
        let span = start.to(end);

        Ok(ast::Param::new(span, ty, identifier))
    }

    fn parse_block(&mut self) -> ParseResult<ast::Stmt> {
        let start = self.span();

        let block_stmt = self.parse_block_stmt()?;

        let end = self.span();
        let span = start.to(end);

        Ok(ast::Stmt::new(span, block_stmt.into()))
    }

    fn parse_stmt(&mut self) -> ParseResult<ast::Stmt> {
        let start = self.span();

        let kind = match self.token {
            Token::OpenBrace => self.parse_block_stmt()?.into(),
            Token::Let => self.parse_decl_stmt()?.into(),
            Token::If => self.parse_if_stmt()?.into(),
            Token::While => self.parse_while_stmt()?.into(),
            Token::Return => self.parse_return_stmt()?.into(),
            _ => self.parse_assign_stmt()?.into(),
        };

        let end = self.span();
        let span = start.to(end);

        Ok(ast::Stmt::new(span, kind))
    }

    fn parse_block_stmt(&mut self) -> ParseResult<ast::BlockStmt> {
        let stmts = self.parse_seq(
            Token::OpenBrace,
            Token::CloseBrace,
            None,
            Parser::parse_stmt,
        )?;

        Ok(ast::BlockStmt::new(stmts))
    }

    fn parse_decl_stmt(&mut self) -> ParseResult<ast::DeclStmt> {
        self.parse_token(Token::Let)?;
        let identifier = self.parse_identifier()?;
        self.parse_token(Token::Colon)?;
        let ty = self.parse_type()?;
        self.parse_token(Token::Assign)?;
        let expr = self.parse_expr()?;
        self.parse_token(Token::Semi)?;

        Ok(ast::DeclStmt::new(ty, identifier, expr))
    }

    fn parse_assign_stmt(&mut self) -> ParseResult<ast::AssignStmt> {
        let dest = self.parse_expr()?;
        self.parse_token(Token::Assign)?;
        let src = self.parse_expr()?;
        self.parse_token(Token::Semi)?;

        Ok(ast::AssignStmt::new(dest, src))
    }

    fn parse_if_stmt(&mut self) -> ParseResult<ast::IfStmt> {
        self.parse_token(Token::If)?;
        self.parse_token(Token::OpenParen)?;
        let cond = self.parse_expr()?;
        self.parse_token(Token::CloseParen)?;
        let if_stmt = self.parse_block()?;
        let else_stmt = self.parse_else_stmt()?;

        Ok(ast::IfStmt::new(cond, if_stmt, else_stmt))
    }

    fn parse_else_stmt(&mut self) -> ParseResult<Option<ast::Stmt>> {
        if !self.try_parse_token(Token::Else) {
            return Ok(None);
        }

        let start = self.span();

        let kind = match self.token {
            Token::OpenBrace => self.parse_block_stmt()?.into(),
            Token::If => self.parse_if_stmt()?.into(),
            _ => {
                self.unexpected_token("else block");
                return Err(());
            }
        };

        let end = self.span();
        let span = start.to(end);

        Ok(Some(ast::Stmt::new(span, kind)))
    }

    fn parse_while_stmt(&mut self) -> ParseResult<ast::WhileStmt> {
        self.parse_token(Token::While)?;
        self.parse_token(Token::OpenParen)?;
        let cond = self.parse_expr()?;
        self.parse_token(Token::CloseParen)?;
        let stmt = self.parse_block()?;

        Ok(ast::WhileStmt::new(cond, stmt))
    }

    fn parse_return_stmt(&mut self) -> ParseResult<ast::ReturnStmt> {
        self.parse_token(Token::Return)?;
        self.parse_token(Token::Semi)?;

        Ok(ast::ReturnStmt::new())
    }

    pub fn parse_expr(&mut self) -> ParseResult<ast::Expr> {
        self.parse_expr_with_precedence(0)
    }

    fn parse_expr_with_precedence(&mut self, min_prec: u8) -> ParseResult<ast::Expr> {
        let mut lhs = self.parse_prefix_expr()?;

        while let Some(op) = self.check_binary_op() {
            let prec = op.precedence();
            if prec < min_prec {
                break;
            }

            let next_min_prec = match op.associativity() {
                Associativity::Left => prec + 1,
                Associativity::Right => prec,
            };

            self.next();

            let rhs = self.parse_expr_with_precedence(next_min_prec)?;

            let span = lhs.span().to(rhs.span());

            lhs = ast::Expr::new(
                span,
                ast::ExprKind::Binary(ast::BinaryExpr::new(lhs, op, rhs)),
            );
        }

        Ok(lhs)
    }

    fn parse_prefix_expr(&mut self) -> ParseResult<ast::Expr> {
        let start = self.span();

        let op = match self.check_unary_op() {
            Some(op) => op,
            None => return self.parse_postfix_expr(),
        };

        self.next();

        let expr = self.parse_prefix_expr()?;

        let end = self.span();
        let span = start.to(end);

        Ok(ast::Expr::new(
            span,
            ast::ExprKind::Unary(ast::UnaryExpr::new(op, expr)),
        ))
    }

    fn parse_postfix_expr(&mut self) -> ParseResult<ast::Expr> {
        let start = self.span();

        let expr = self.parse_primary_expr()?;

        let kind = match self.token {
            Token::OpenBracket => self.parse_index_expr(expr)?.into(),
            _ => return Ok(expr),
        };

        let end = self.span();
        let span = start.to(end);

        Ok(ast::Expr::new(span, kind))
    }

    fn parse_primary_expr(&mut self) -> ParseResult<ast::Expr> {
        let start = self.span();

        let lhs_kind = match self.token {
            Token::Int => self.parse_int_expr()?.into(),
            Token::Float => self.parse_float_expr()?.into(),
            Token::Bool => self.parse_bool_expr()?.into(),
            Token::Count => self.parse_count_expr()?.into(),
            Token::Element => self.parse_element_expr()?.into(),
            Token::Identifier => self.parse_identifier_expr()?.into(),
            Token::OpenParen => self.parse_paren_expr()?.into(),
            _ => return Err(()),
        };

        let end = self.span();
        let span = start.to(end);

        Ok(ast::Expr::new(span, lhs_kind))
    }

    fn parse_int_expr(&mut self) -> ParseResult<ast::IntExpr> {
        self.expect(Token::Int)?;

        let value = self.slice().parse().expect("Failed to parse int");

        self.next();

        Ok(ast::IntExpr::new(value))
    }

    fn parse_float_expr(&mut self) -> ParseResult<ast::FloatExpr> {
        self.expect(Token::Float)?;

        let value = self.slice().parse().expect("Failed to parse float");

        self.next();

        Ok(ast::FloatExpr::new(value))
    }

    fn parse_bool_expr(&mut self) -> ParseResult<ast::BoolExpr> {
        self.expect(Token::Bool)?;

        let value = self.slice().parse().expect("Failed to parse bool");

        self.next();

        Ok(ast::BoolExpr::new(value))
    }

    fn parse_count_expr(&mut self) -> ParseResult<ast::CountExpr> {
        self.parse_token(Token::Count)?;

        Ok(ast::CountExpr::new())
    }

    fn parse_element_expr(&mut self) -> ParseResult<ast::ElementExpr> {
        self.parse_token(Token::Element)?;

        Ok(ast::ElementExpr::new())
    }

    fn parse_identifier_expr(&mut self) -> ParseResult<ast::IdentifierExpr> {
        let identifier = self.parse_identifier()?;

        Ok(ast::IdentifierExpr::new(identifier))
    }

    fn parse_index_expr(&mut self, expr: ast::Expr) -> ParseResult<ast::IndexExpr> {
        self.parse_token(Token::OpenParen)?;
        let index = self.parse_expr()?;
        self.parse_token(Token::CloseParen)?;

        Ok(ast::IndexExpr::new(expr, index))
    }

    fn parse_paren_expr(&mut self) -> ParseResult<ast::ParenExpr> {
        self.parse_token(Token::OpenParen)?;
        let expr = self.parse_expr()?;
        self.parse_token(Token::CloseParen)?;

        Ok(ast::ParenExpr::new(expr))
    }

    fn parse_type(&mut self) -> ParseResult<ast::Type> {
        let variability = self.parse_variability()?;

        let is_ref = self.try_parse_token(Token::BitAnd);

        let primitive = self.parse_primitive()?;

        let ty = if is_ref {
            ast::Type::PrimRef(variability, primitive)
        } else {
            ast::Type::Prim(variability, primitive)
        };

        Ok(ty)
    }

    fn parse_variability(&mut self) -> ParseResult<ast::Variability> {
        match self.token {
            Token::Uniform => {
                self.next();
                Ok(ast::Variability::Uniform)
            }
            Token::Varying => {
                self.next();
                Ok(ast::Variability::Varying)
            }
            _ => Ok(ast::Variability::Varying),
        }
    }

    fn parse_primitive(&mut self) -> ParseResult<ast::Primitive> {
        self.expect(Token::Identifier)?;

        let primitive = match self.slice() {
            "int" => ast::Primitive::Int,
            "float" => ast::Primitive::Float,
            "bool" => ast::Primitive::Bool,
            _ => {
                self.unexpected_token("type");
                return Err(());
            }
        };

        self.next();

        Ok(primitive)
    }

    fn parse_identifier(&mut self) -> ParseResult<ast::Identifier> {
        self.expect(Token::Identifier)?;

        let span = self.span().into();
        let symbol = self.intern_symbol(self.slice());

        self.next();

        Ok(ast::Identifier::new(span, symbol))
    }

    fn parse_seq<T, F>(
        &mut self,
        start: Token,
        end: Token,
        delim: Option<Token>,
        mut parse: F,
    ) -> ParseResult<Vec<T>>
    where
        F: FnMut(&mut Parser<'source>) -> ParseResult<T>,
    {
        self.parse_token(start)?;

        let mut values = Vec::new();

        let mut first = true;
        loop {
            if self.try_parse_token(end) {
                break;
            }

            if first {
                first = false;
            } else if let Some(delim) = delim {
                self.parse_token(delim)?;
            }

            if self.try_parse_token(end) {
                break;
            }

            let value = parse(self)?;
            values.push(value);
        }

        Ok(values)
    }

    fn check_unary_op(&self) -> Option<ast::UnaryOp> {
        let op = match self.token {
            Token::Mul => ast::UnaryOp::Deref,
            Token::Sub => ast::UnaryOp::Negate,
            Token::Not => ast::UnaryOp::Not,
            _ => return None,
        };

        Some(op)
    }

    fn check_binary_op(&self) -> Option<ast::BinaryOp> {
        let op = match self.token {
            Token::Mul => ast::BinaryOp::Mul,
            Token::Add => ast::BinaryOp::Add,
            Token::Sub => ast::BinaryOp::Sub,
            Token::Shl => ast::BinaryOp::Shl,
            Token::Shr => ast::BinaryOp::Shr,
            Token::BitAnd => ast::BinaryOp::BitAnd,
            Token::BitXor => ast::BinaryOp::BitXor,
            Token::BitOr => ast::BinaryOp::BitOr,
            Token::Min => ast::BinaryOp::Min,
            Token::Max => ast::BinaryOp::Max,
            Token::Eq => ast::BinaryOp::Eq,
            Token::Ne => ast::BinaryOp::Ne,
            Token::Lt => ast::BinaryOp::Lt,
            Token::Gt => ast::BinaryOp::Gt,
            Token::Le => ast::BinaryOp::Le,
            Token::Ge => ast::BinaryOp::Ge,
            Token::LogicalAnd => ast::BinaryOp::LogicalAnd,
            Token::LogicalOr => ast::BinaryOp::LogicalOr,
            _ => return None,
        };

        Some(op)
    }

    fn next(&mut self) {
        self.token = self.lex.next().unwrap_or(Token::End);
    }

    fn expect(&mut self, expected: Token) -> ParseResult<()> {
        if self.token == expected {
            Ok(())
        } else {
            self.unexpected_token(expected);
            Err(())
        }
    }

    fn parse_token(&mut self, token: Token) -> ParseResult<()> {
        let result = self.expect(token);
        if result.is_ok() {
            self.next();
        }

        result
    }

    fn try_parse_token(&mut self, expected: Token) -> bool {
        if self.token == expected {
            self.next();
            true
        } else {
            false
        }
    }

    fn unexpected_token<T>(&mut self, expected: T)
    where
        T: fmt::Debug,
    {
        self.errors.push(Error::new(
            format!(
                "Unexpected token: Expected {:#?}, found {:#?}",
                expected, self.token
            ),
            self.span(),
        ));
    }

    fn intern_symbol(&mut self, name: &'source str) -> ast::Symbol {
        self.symbols.intern(name)
    }

    fn span(&self) -> Span {
        self.lex.span().into()
    }

    fn slice(&self) -> &'source str {
        self.lex.slice()
    }
}

mod test {
    use super::*;
    use crate::SourceMap;

    #[test]
    fn parser() {
        let source = r"fn hello(value: int) {
    let a: int = 3 + 4 * 6;
}";

        let mut lex = Token::lexer(source);
        let mut parser = Parser::new(source);
        let source_map = SourceMap::new("source", source);

        while let Some(token) = lex.next() {
            println!("{:#?}: |{}|", token, lex.slice());
        }

        match parser.parse_func() {
            Ok(f) => println!("jjj{:#?}", f),
            Err(s) => {
                assert!(!parser.errors.is_empty());
                for error in parser.errors {
                    println!("{}", error.display(&source_map))
                }
            }
        }
    }

    #[test]
    fn expr() {
        let source = r"3 + *4 * 7";

        let mut lex = Token::lexer(source);
        let mut parser = Parser::new(source);
        let source_map = SourceMap::new("source", source);

        while let Some(token) = lex.next() {
            println!("{:#?}: |{}|", token, lex.slice());
        }

        match parser.parse_expr() {
            Ok(f) => println!("jjj{:#?}", f),
            Err(s) => {
                assert!(!parser.errors.is_empty());
                for error in parser.errors {
                    println!("{}", error.display(&source_map))
                }
            }
        }
    }
}
