#[macro_use]
mod collections;

pub mod ast;
pub mod error;
pub mod ir;
pub mod parser;

use crate::error::Errors;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }
}

#[derive(Debug)]
pub struct SourceMap<'a> {
    name: String,
    input: &'a str,
    lines: Vec<usize>,
}

impl<'a> SourceMap<'a> {
    pub fn new<T>(name: T, input: &'a str) -> SourceMap<'a>
    where
        T: Into<String>,
    {
        SourceMap {
            name: name.into(),
            input,
            lines: lines(input),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn location(&self, offset: usize) -> (usize, usize) {
        let line = match self.lines.binary_search(&offset) {
            Ok(line) => line,
            Err(line) => line - 1,
        };
        let col = offset - self.lines[line];
        (line, col)
    }

    pub fn line(&self, line: usize) -> &'a str {
        let start = self.line_start(line);
        let end = self.line_end(line);
        &self.input[start..end]
    }

    fn line_start(&self, line: usize) -> usize {
        self.lines[line]
    }

    fn line_end(&self, line: usize) -> usize {
        match self.lines.get(line + 1) {
            Some(offset) => offset - 1,
            None => self.input.len(),
        }
    }
}

fn lines(input: &str) -> Vec<usize> {
    let mut lines = vec![0];
    for (i, c) in input.char_indices() {
        if c == '\n' {
            lines.push(i + 1)
        }
    }
    lines
}

pub fn compile(input: &str) -> Result<ir::Func, Errors> {
    let func = parser::parse(&input);

    let mut func = ast::generate_ir(&func)?;

    ir::verify_ir(&func);

    func.resolve_aliases();

    Ok(func)
}
