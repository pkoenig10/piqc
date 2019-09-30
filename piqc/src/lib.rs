#[macro_use]
mod collections;

pub mod ast;
pub mod ir;

mod parser;

pub fn compile(s: &str) -> Result<String, ()> {
    let parse_result = parser::parse(s);

    let func = match parse_result {
        Ok(func) => func,
        Err(errors) => {
            for error in errors {

            }
        }
    };

    println!("{:#?}", func);

    let mut func = ast::generate_ir(&func);

    ir::verify_ir(&func);

    func.resolve_aliases();

    Ok(format!("{}", func))
}

fn format_error<'a>(input: &Input<'a>, error: parser::Error) -> String {
    match error {
        parser::Error::UnexpectedToken(expected, span) => {
            (line, col) = input.get_location(span.start)
            format!(
                "{s}--> file.piq:{l}:{c}\n\
                 {s} |\n\
                 {j} | {line}\n\
                 {s} | {underline}\n\
                 {s} |\n\
                 {s} = {message}",
                s = "    ",
                j = format!("   {}", line),
                l = line,
                c = col,
                line = self.line,
                underline = self.underline(),
                message = format!("Expected {}", expected)
            )
        }
        parser::Error::Custom(error) => error.to_owned(),
    }
}

struct Input<'a> {
    input: &'a str,
    lines: Vec<usize>
}

impl<'a> Input<'a> {
    pub fn new(input: &'a str) -> Input<'a> {
        Input {
            input,
            lines: compute_lines(input),
        }
    }

    pub fn get_location(&self, offset: usize) -> (usize, usize) {
        let index = match v.binary_search(&offset) {
            Ok(index) => index,
            Err(index) => index,
        };
        let line_offset = match index {
            0 => 0,
            index => lines[index - 1]
        };
        let line = index + 1;
        let col = offset - line_offset;
        (line, col)
    }
}

fn compute_lines(input: &str) -> Vec<usize> {
    input.char_indices()
            .filter(|&(_, c)| c == '\n')
            .map(|(i, _)| i)
            .collect()
}
