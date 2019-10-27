use crate::ast;
use crate::collections::InternMap;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(warnings)]
    parser
);

pub fn parse(input: &str) -> ast::Func {
    parser::FuncParser::new()
        .parse(&mut Context::new(), input)
        .unwrap()
}

pub struct Context<'input> {
    symbols: InternMap<ast::Symbol, &'input str>,
}

impl<'input> Context<'input> {
    pub fn new() -> Context<'input> {
        Context {
            symbols: InternMap::new(),
        }
    }

    pub fn intern_symbol(&mut self, name: &'input str) -> ast::Symbol {
        self.symbols.intern(name)
    }
}
