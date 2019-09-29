use pest::error::Error;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parser2/grammar.pest"]
struct MyParser;

pub fn parse(input: &str) {
    match MyParser::parse(Rule::int, input) {
        Ok(pairs) => {
            for pair in pairs {
                println!("{:#?}", pair);
            }
        }
        Err(error) => {
            println!("{}", error);
        }
    };
}
