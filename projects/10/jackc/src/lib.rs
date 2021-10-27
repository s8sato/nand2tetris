extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod parser;
pub mod tokenizer;

use prelude::*;

pub struct IO {
    pub input: String,
    pub writer: BufWriter<File>,
}

pub struct Stack(Vec<Element>);

pub struct Element {
    tag: &'static str,
    body: Option<String>,
}

impl Element {
    fn new(tag: &'static str, body: impl ToString) -> Self {
        Self {
            tag,
            body: Some(body.to_string()),
        }
    }
    fn tag(tag: &'static str) -> Self {
        Self { tag, body: None }
    }
}

impl Stack {
    pub fn new() -> Self {
        Self(Vec::new())
    }
    fn push(&mut self, element: Element, writer: &mut BufWriter<File>) {
        self.indent(writer);
        write!(writer, "<{}>", element.tag).unwrap();
        self.delimit(element.body.is_some(), writer);
        self.0.push(element)
    }
    fn pop(&mut self, writer: &mut BufWriter<File>) -> Option<Element> {
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
    fn indent(&mut self, writer: &mut BufWriter<File>) {
        (0..self.0.len()).for_each(|_| write!(writer, "  ").unwrap())
    }
    fn delimit(&mut self, inline: bool, writer: &mut BufWriter<File>) {
        let delimiter = if inline { " " } else { "\n" };
        write!(writer, "{}", delimiter).unwrap();
    }
    fn push_t(&mut self, element: Element, writer: &mut BufWriter<File>) {
        // self.indent(writer);
        write!(writer, "<{}>", element.tag).unwrap();
        self.delimit(element.body.is_some(), writer);
        self.0.push(element)
    }
}

pub mod prelude {
    pub use super::{Element, Stack, IO};
    pub use pest::iterators::Pair;
    pub use pest::Parser;
    pub use std::collections::HashMap;
    pub use std::fmt::Display;
    pub use std::fs::{self, File};
    pub use std::io::{BufWriter, Write};
    pub use std::path::PathBuf;
}
