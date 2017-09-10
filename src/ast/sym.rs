use std::collections::HashMap;

use ast::expr::*;
use ast::type_::*;

#[derive(Debug)]
pub struct Symbol<'input: 'ast, 'ast> {
    identifier: &'ast Identifier<'input>,
    type_: Type,
}

impl<'input, 'ast> Symbol<'input, 'ast> {
    pub fn new(identifier: &'ast Identifier<'input>, type_: Type) -> Symbol<'input, 'ast> {
        Symbol { identifier, type_ }
    }

    pub fn identifier(&self) -> &'ast Identifier<'input> {
        self.identifier
    }

    pub fn type_(&self) -> Type {
        self.type_
    }
}

#[derive(Debug)]
struct ScopeSymbolTable<'input: 'ast, 'ast> {
    symbols: HashMap<&'input str, Symbol<'input, 'ast>>,
}

impl<'input, 'ast> ScopeSymbolTable<'input, 'ast> {
    pub fn new() -> ScopeSymbolTable<'input, 'ast> {
        ScopeSymbolTable { symbols: HashMap::new() }
    }

    fn get_symbol(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    fn insert_symbol(&mut self, symbol: Symbol<'input, 'ast>) {
        self.symbols.insert(symbol.identifier().name(), symbol);
    }
}

#[derive(Debug)]
pub struct SymbolTable<'input: 'ast, 'ast> {
    symbol_tables: Vec<ScopeSymbolTable<'input, 'ast>>,
}

impl<'input, 'ast> SymbolTable<'input, 'ast> {
    pub fn new() -> SymbolTable<'input, 'ast> {
        SymbolTable { symbol_tables: Vec::new() }
    }

    pub fn push_scope(&mut self) {
        self.symbol_tables.push(ScopeSymbolTable::new());
    }

    pub fn pop_scope(&mut self) {
        self.symbol_tables.pop().unwrap();
    }

    pub fn get_symbol(&self, name: &str) -> Option<&Symbol> {
        for scope in self.symbol_tables.iter().rev() {
            let symbol = scope.get_symbol(name);
            if symbol.is_some() {
                return symbol;
            }
        }
        return None;
    }

    pub fn insert_symbol(&mut self, symbol: Symbol<'input, 'ast>) {
        self.symbol_tables.last_mut().unwrap().insert_symbol(symbol);
    }
}
