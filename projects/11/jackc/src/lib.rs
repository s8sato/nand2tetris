extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod parser;
pub mod symbol_table;
pub mod tokenizer;

use convert_case::{Case, Casing};
use prelude::*;
use std::collections::HashMap;
use symbol_table::{Class, Subroutine, SymbolTable, Type};
pub type Writer = BufWriter<fs::File>;

pub struct IO {
    pub input: String,
    pub writer: Writer,
}

pub struct Stack {
    elements: Vec<Element>,
    context: Context,
    class_table: SymbolTable<Class>,
    subroutine_table: SymbolTable<Subroutine>,
}

#[derive(Default)]
pub struct Element {
    tag: String,
    body: Option<String>,
    metadata: Option<MetaData>,
}

#[derive(Debug)]
pub struct MetaData {
    pub du: DU,
    pub category: Category,
}

#[derive(Debug)]
pub enum Category {
    Var(Index),
    Argument(Index),
    Static(Index),
    Field(Index),
    Class,
    Subroutine,
}

type Index = u32;

struct Context {
    scope: Scope,
    type_: Type,
    du: DU,
}

enum Scope {
    Class,
    Subroutine,
}

#[derive(Debug)]
enum DU {
    Defined,
    Used,
}

impl Context {
    fn new() -> Self {
        Self {
            scope: Scope::Class,
            type_: Type::Int,
            du: DU::Defined,
        }
    }
}

impl<'a> Element {
    fn new<R: RuleType>(pair: &Pair<'a, R>, f: impl Fn(&'a str) -> &'a str) -> Self {
        Self::construct(pair, true, f)
    }
    fn tag<R: RuleType>(pair: &Pair<R>) -> Self {
        Self::construct(pair, false, identity)
    }
    fn construct<R: RuleType>(
        pair: &Pair<'a, R>,
        has_body: bool,
        f: impl Fn(&'a str) -> &'a str,
    ) -> Self {
        let body = has_body.then(|| f(pair.as_str()).to_owned());
        Self {
            tag: format!("{:?}", pair.as_rule()).to_case(Case::Camel),
            body,
            metadata: None,
        }
    }
    fn metadata(mut self, metadata: MetaData) -> Self {
        self.metadata = Some(metadata);
        self
    }
}

impl Stack {
    pub fn new() -> Self {
        Self {
            elements: Vec::new(),
            context: Context::new(),
            class_table: SymbolTable::<Class>::new(),
            subroutine_table: SymbolTable::<Subroutine>::new(),
        }
    }
    fn push(&mut self, element: Element, writer: &mut Writer) {
        self.indent(writer);
        write!(writer, "<{} {:?}>", element.tag, element.metadata).unwrap();
        self.delimit(element.body.is_some(), writer);
        self.elements.push(element)
    }
    fn pop(&mut self, writer: &mut Writer) -> Option<Element> {
        let element = self.elements.pop()?;
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
        (0..self.elements.len()).for_each(|_| write!(writer, "  ").unwrap())
    }
    fn delimit(&mut self, inline: bool, writer: &mut Writer) {
        let delimiter = if inline { " " } else { "\n" };
        write!(writer, "{}", delimiter).unwrap();
    }
}

pub fn symbol_map() -> HashMap<&'static str, &'static str> {
    [("&", "&amp;"), (">", "&gt;"), ("<", "&lt;")].into()
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use std::cmp::PartialEq;
    use std::fmt::Debug;

    const DIRS: [&str; 3] = ["../ArrayTest", "../ExpressionLessSquare", "../Square"];

    pub trait Convert<T> {
        fn convert(self) -> T;
    }

    pub fn test<O, S, T>(run: fn(IO), file_stem_suffix: &str)
    where
        O: From<String> + Convert<T>,
        S: From<String> + Convert<T>,
        T: PartialEq + Debug,
    {
        for dir in DIRS.into_iter().map(PathBuf::from) {
            test_for_each_directory::<O, S, T>(run, file_stem_suffix, dir)
        }
    }

    fn test_for_each_directory<O, S, T>(run: fn(IO), file_stem_suffix: &str, dir: PathBuf)
    where
        O: From<String> + Convert<T>,
        S: From<String> + Convert<T>,
        T: PartialEq + Debug,
    {
        let in_files = dir
            .read_dir()
            .expect("Failed to read directory")
            .map(|entry| entry.unwrap().path())
            .filter(|path| path.extension().map(|ext| ext.to_str()) == Some(Some("jack")))
            .collect::<Vec<PathBuf>>();

        for in_file in in_files {
            test_for_each_file::<O, S, T>(run, file_stem_suffix, in_file)
        }
    }

    fn test_for_each_file<O, S, T>(run: fn(IO), file_stem_suffix: &str, in_file: PathBuf)
    where
        O: From<String> + Convert<T>,
        S: From<String> + Convert<T>,
        T: PartialEq + Debug,
    {
        let mut file_stem = in_file.file_stem().unwrap_or_default().to_owned();
        file_stem.push(file_stem_suffix);

        let out_file = in_file
            .with_file_name(&file_stem)
            .with_extension("test.xml");
        let writer = BufWriter::new(fs::File::create(&out_file).unwrap());
        let input = fs::read_to_string(&in_file).unwrap();
        run(IO { input, writer });
        let output = O::from(fs::read_to_string(&out_file).unwrap());

        let solution_file = in_file.with_file_name(&file_stem).with_extension("xml");
        let solution = S::from(fs::read_to_string(&solution_file).unwrap());

        assert_eq!(output.convert(), solution.convert());
        fs::remove_file(out_file).unwrap();
    }
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
