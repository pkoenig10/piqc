use std::collections::HashMap;

use ast::*;

#[derive(Debug)]
pub struct Symbol<'input> {
    identifier: Identifier<'input>,
    type_: Type,
}

impl<'input> Symbol<'input> {
    pub fn new(identifier: Identifier<'input>, type_: Type) -> Symbol<'input> {
        Symbol { identifier, type_ }
    }

    pub fn identifier(&self) -> &Identifier<'input> {
        &self.identifier
    }

    pub fn type_(&self) -> Type {
        self.type_
    }
}

#[derive(Debug)]
struct ScopeSymbolTable<'input> {
    symbols: HashMap<&'input str, Symbol<'input>>,
}

impl<'input> ScopeSymbolTable<'input> {
    pub fn new() -> ScopeSymbolTable<'input> {
        ScopeSymbolTable { symbols: HashMap::new() }
    }

    fn get_symbol(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    fn insert_symbol(&mut self, symbol: Symbol<'input>) {
        self.symbols.insert(symbol.identifier().name(), symbol);
    }
}

#[derive(Debug)]
pub struct SymbolTable<'input> {
    symbol_tables: Vec<ScopeSymbolTable<'input>>,
}

impl<'input> SymbolTable<'input> {
    pub fn new() -> SymbolTable<'input> {
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

    pub fn insert_symbol(&mut self, symbol: Symbol<'input>) {
        self.symbol_tables.last_mut().unwrap().insert_symbol(symbol);
    }
}
