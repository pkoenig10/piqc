use std::collections::HashMap;

#[derive(Debug)]
struct ScopeSymbolTable<'input, T> {
    symbols: HashMap<&'input str, T>,
}

impl<'input, T> ScopeSymbolTable<'input, T> {
    pub fn new() -> ScopeSymbolTable<'input, T> {
        ScopeSymbolTable { symbols: HashMap::new() }
    }

    fn get(&self, name: &str) -> Option<&T> {
        self.symbols.get(name)
    }

    fn insert(&mut self, name: &'input str, symbol: T) {
        self.symbols.insert(name, symbol);
    }
}

#[derive(Debug)]
pub struct SymbolTable<'input, T> {
    symbol_tables: Vec<ScopeSymbolTable<'input, T>>,
}

impl<'input, T> SymbolTable<'input, T> {
    pub fn new() -> SymbolTable<'input, T> {
        SymbolTable { symbol_tables: Vec::new() }
    }

    pub fn push_scope(&mut self) {
        self.symbol_tables.push(ScopeSymbolTable::new());
    }

    pub fn pop_scope(&mut self) {
        self.symbol_tables.pop().unwrap();
    }

    pub fn get(&self, name: &str) -> Option<&T> {
        for scope in self.symbol_tables.iter().rev() {
            let symbol = scope.get(name);
            if symbol.is_some() {
                return symbol;
            }
        }
        return None;
    }

    pub fn insert(&mut self, name: &'input str, symbol: T) {
        self.symbol_tables.last_mut().unwrap().insert(name, symbol);
    }
}
