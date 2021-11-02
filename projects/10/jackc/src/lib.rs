extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod parser;
pub mod tokenizer;

use convert_case::{Case, Casing};
use prelude::*;
use std::collections::HashMap;

pub type Writer = BufWriter<fs::File>;

pub struct IO {
    pub input: String,
    pub writer: Writer,
}

pub struct Stack(Vec<Element>);

pub struct Element {
    tag: String,
    body: Option<String>,
}

impl<'a> Element {
    fn new<R: RuleType>(pair: &Pair<'a, R>, f: impl Fn(&'a str) -> &'a str) -> Self {
        Self::with_body(pair, Some(f(pair.as_str()).to_owned()))
    }
    fn tag<R: RuleType>(pair: &Pair<R>) -> Self {
        Self::with_body(pair, None)
    }
    fn with_body<R: RuleType>(pair: &Pair<R>, body: Option<String>) -> Self {
        Self {
            tag: format!("{:?}", pair.as_rule()).to_case(Case::Camel),
            body,
        }
    }
}

impl Stack {
    pub fn new() -> Self {
        Self(Vec::new())
    }
    fn push(&mut self, element: Element, writer: &mut Writer) {
        self.indent(writer);
        write!(writer, "<{}>", element.tag).unwrap();
        self.delimit(element.body.is_some(), writer);
        self.0.push(element)
    }
    fn pop(&mut self, writer: &mut Writer) -> Option<Element> {
        let element = self.0.pop()?;
        if let Some(body) = &element.body {
            write!(writer, "{}", body).unwrap();
            self.delimit(true, writer);
        } else {
            self.indent(writer);
        }
        writeln!(writer, "</{}>", element.tag).unwrap();
        Some(element)
    }
    fn indent(&mut self, writer: &mut Writer) {
        (0..self.0.len()).for_each(|_| write!(writer, "  ").unwrap())
    }
    fn delimit(&mut self, inline: bool, writer: &mut Writer) {
        let delimiter = if inline { " " } else { "\n" };
        write!(writer, "{}", delimiter).unwrap();
    }
}

pub fn symbol_map() -> HashMap<&'static str, &'static str> {
    [("&", "&amp;"), (">", "&gt;"), ("<", "&lt;")].into()
}

pub mod prelude {
    pub use super::{symbol_map, Element, Stack, Writer, IO};
    pub use pest::iterators::Pair;
    pub use pest::{Parser, RuleType};
    pub use std::convert::identity;
    pub use std::fs;
    pub use std::io::{BufWriter, Write};
    pub use std::path::PathBuf;
}
