use lalrpop_util::lalrpop_mod;
use lalrpop_util::ErrorRecovery;

use std::fmt::Debug;

use crate::ast;
use crate::collections::InternMap;

lalrpop_mod!(
    #[allow(warnings)]
    parser
);

pub fn parse(input: &str) -> ast::Func {
    let mut context = &mut Context::new();
    let result = parser::FuncParser::new().parse(&mut context, input);
    result.unwrap().unwrap()
}

pub enum Error {
    Error,
}

pub struct Context<'input> {
    symbols: InternMap<ast::Symbol, &'input str>,
    errors: Vec<(usize, &'static str)>,
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

    pub fn push_parse_error<T: Debug>(&mut self, error: ErrorRecovery<usize, T, &'static str>) {
        println!("ERROR: {:#?}", error);
    }
}
