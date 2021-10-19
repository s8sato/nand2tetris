extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "jack.pest"]
pub struct JackParser;

use std::fs;
use std::io::{BufWriter, Write};

fn main() {
    let unparsed_file = fs::read_to_string("jack.jack").expect("cannot read file");

    let file = JackParser::parse(Rule::file, &unparsed_file)
        .expect("unsuccessful parse") // unwrap the parse result
        .next()
        .unwrap(); // get and unwrap the `file` rule; never fails

    let writer = BufWriter::new(fs::File::create("jackT.xml").unwrap());

    for token in file.into_inner() {
        println!("{:?}", token)
        // match token.as_rule() {
        //     Rule::token => {
        //         let mut mark_up = |s: &str| writeln!(writer, "<{0}>{1}</{0}>", s, token.as_str()).unwrap();
        //         match token.into_inner().next().unwrap().as_rule() {
        //             Rule::keyword => mark_up("keyword"),
        //             Rule::symbol => mark_up("symbol"),
        //             Rule::integer_constant => mark_up("integerConstant"),
        //             Rule::string_constant => mark_up("stringConstant"),
        //             Rule::identifier => mark_up("identifier"),
        //             _ => unreachable!(),
        //         }
        //     }
        //     Rule::EOI => (),
        //     _ => unreachable!(),
        // }
    }
}
