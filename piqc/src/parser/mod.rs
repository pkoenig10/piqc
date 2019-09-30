use lalrpop_util::lalrpop_mod;

use std::fmt;

use crate::ast;
use crate::collections::InternMap;

lalrpop_mod!(
    #[allow(warnings)]
    parser
);

use parser::Token;

pub fn parse(input: &str) -> Result<ast::Func, Vec<Error>> {
    let mut context = Context::new();
    match parser::FuncParser::new().parse(&mut context, input) {
        Ok(func) => {
            if context.errors.is_empty() {
                return Ok(func);
            }
        },
        Err(error) => {
            context.push_error(Expected::Func, error);
        }
    }

    Err(context.errors)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: u32,
    end: u32,
}

impl Span {
    pub fn from_location(location: usize) -> Span {
        Span {
            start: location as u32,
            end: location as u32,
        }
    }

    pub fn from_token<T>((start, _, end): (usize, T, usize)) -> Span {
        Span {
            start: start as u32,
            end: end as u32,
        }
    }
}

#[derive(Debug)]
pub enum Expected {
    Expr,
    Stmt,
    Func,
}

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let expected = match self {
            Expr => "expression",
            Stmt => "statement",
            Func => "function",
        }
    }
}

#[derive(Debug)]
pub enum Error {
    UnexpectedToken(Expected, Span),
    Custom(&'static str),
}

pub struct Context<'input> {
    symbols: InternMap<ast::Symbol, &'input str>,
    errors: Vec<Error>,
}

impl<'input> Context<'input> {
    pub fn new() -> Context<'input> {
        Context {
            symbols: InternMap::new(),
            errors: Vec::new(),
        }
    }

    pub fn intern_symbol(&mut self, name: &'input str) -> ast::Symbol {
        self.symbols.intern(name)
    }

    pub fn push_error_recovery(
        &mut self,
        expected: Expected,
        error_recovery: lalrpop_util::ErrorRecovery<usize, Token<'input>, &'static str>,
    ) {
        self.push_error(expected, error_recovery.error);
    }

    fn push_error(
        &mut self,
        expected: Expected,
        error: lalrpop_util::ParseError<usize, Token<'input>, &'static str>,
    ) {
        let error = match error_recovery.error {
            lalrpop_util::ParseError::InvalidToken {
                location,
                ..
            }
            | lalrpop_util::ParseError::UnrecognizedEOF {
                location,
                ..
            } => {
                Error::UnexpectedToken(expected, Span::from_location(location))
            },
            lalrpop_util::ParseError::UnrecognizedToken {
                token,
                ..
            }
            | lalrpop_util::ParseError::ExtraToken {
                token,
                ..
            } => {
                Error::UnexpectedToken(expected, Span::from_token(token))
            }
            lalrpop_util::ParseError::Custom {
                error,
                ..
            } => {
                Error::Custom(error)
            }
        }
        self.errors.push(error);
    }
}
