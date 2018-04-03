mod piqc;

use ast;

use self::piqc::ProgParser;

pub fn parse<'input>(input: &'input str) -> ast::Prog<'input> {
    ProgParser::new().parse(input).unwrap()
}
