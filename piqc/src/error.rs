use crate::{SourceMap, Span};
use std::cmp;
use std::error;
use std::fmt;

#[derive(Debug)]
pub struct Error {
    message: String,
    span: Span,
}

impl Error {
    pub fn new<T>(message: T, span: Span) -> Error
    where
        T: Into<String>,
    {
        Error {
            message: message.into(),
            span,
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn display<'a>(self, source_map: &'a SourceMap) -> Display<'a> {
        Display::new(self, source_map)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl error::Error for Error {}

pub type Errors = Vec<Error>;

#[derive(Debug)]
pub struct Display<'a> {
    error: Error,
    source_map: &'a SourceMap<'a>,
}

impl<'a> Display<'a> {
    fn new(error: Error, source_map: &'a SourceMap) -> Display<'a> {
        Display { error, source_map }
    }
}

impl fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (start_line, start_col) = self.source_map.location(self.error.span().start());
        let (end_line, end_col) = self.source_map.location(self.error.span().end());
        let num_lines = end_line - start_line + 1;

        let start_input = self.source_map.line(start_line);

        let gutter_width_max = if num_lines <= 2 { 0 } else { 2 };
        let gutter_width = cmp::max(format!("{}", end_line).len(), gutter_width_max);

        let underline_end = if num_lines <= 1 {
            end_col
        } else {
            start_input.len()
        };
        let underline_width = underline_end - start_col;

        write!(
            f,
            "{name}:{line}:{col}: {message}\n\
             {s:>w$} |\n\
             {l:>w$} | {start_input}\n\
             {s:>w$} | {s:padding_width$}{s:~<underline_width$}\n",
            name = self.source_map.name(),
            line = start_line + 1,
            col = start_col + 1,
            message = self.error.message(),
            s = "",
            l = start_line + 1,
            w = gutter_width,
            start_input = start_input,
            padding_width = start_col,
            underline_width = underline_width
        )?;

        if num_lines > 1 {
            if num_lines > 2 {
                write!(f, "...\n")?;
            }

            let end_input = self.source_map.line(end_line);

            write!(
                f,
                "{l:>w$} | {end_input}\n\
                 {s:>w$} | {s:~<underline_width$}\n",
                s = "",
                l = end_line + 1,
                w = gutter_width,
                end_input = end_input,
                underline_width = end_col
            )?;
        }

        write!(f, "{s:>w$} |\n", s = "", w = gutter_width)
    }
}

impl error::Error for Display<'_> {}
