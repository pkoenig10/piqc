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
