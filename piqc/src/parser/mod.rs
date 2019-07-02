use lalrpop_util::lalrpop_mod;

use crate::ast;
use crate::util::InternMap;

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
    variables: InternMap<ast::Variable, &'input str>,
}

impl<'input> Context<'input> {
    pub fn new() -> Context<'input> {
        Context {
            variables: InternMap::new(),
        }
    }

    pub fn intern_variable(&mut self, name: &'input str) -> ast::Variable {
        self.variables.intern(name)
    }
}
