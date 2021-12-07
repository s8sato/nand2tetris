extern crate pest;
#[macro_use]
extern crate pest_derive;

use convert_case::{Case, Casing};
use pest::{iterators::Pair, Parser, RuleType};
use std::collections::HashMap;
use std::convert::{self, Infallible};
use std::fmt;
use std::fs;
use std::io::{BufWriter, Write};
use std::str::FromStr;
use symbol_table::{Class, Subroutine, SymbolTable};

mod symbol_table;

pub struct IO {
    pub input: String,
    pub writer: Writer,
}

type Writer = BufWriter<fs::File>;

impl IO {
    pub fn run(self) {
        let file = Jack::parse(Rule::file, &self.input)
            .expect("Failed to parse input")
            .next()
            .unwrap();
        let mut writer = self.writer;
        let mut stack = Stack::new();

        stack.scan(file, &mut writer);
        writer.flush().unwrap()
    }
}

#[derive(Parser)]
#[grammar = "jack.pest"]
struct Jack;

struct Stack {
    elements: Vec<Element>,
    context: Context,
    class_table: SymbolTable<Class>,
    subroutine_table: SymbolTable<Subroutine>,
}

#[derive(Default)]
struct Element {
    tag: String,
    body: Option<String>,
    metadata: Option<MetaData>,
}

struct Context {
    du: DU,
    type_: Type,
    var_kind: VarKind,
}

#[derive(Debug, Clone, Copy)]
enum DU {
    Defined,
    Used,
}

#[derive(Clone)]
pub enum Type {
    Void,
    Int,
    Char,
    Boolean,
    Class(Name),
}

type Name = String;

enum VarKind {
    Class(ClassVarKind),
    Subroutine(SubroutineVarKind),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClassVarKind {
    Static,
    Field,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum SubroutineVarKind {
    Argument,
    Var,
}

#[derive(Debug)]
struct MetaData {
    du: DU,
    category: Category,
}

#[derive(Debug)]
enum Category {
    Var(Index),
    Argument(Index),
    Static(Index),
    Field(Index),
    Class,
    Subroutine,
}

type Index = u32;

impl Stack {
    fn new() -> Self {
        Self {
            elements: Vec::new(),
            context: Context::new(),
            class_table: SymbolTable::<Class>::new(),
            subroutine_table: SymbolTable::<Subroutine>::new(),
        }
    }

    fn scan(&mut self, pair: Pair<Rule>, writer: &mut Writer) {
        for p in pair.into_inner() {
            self.context.switch(&p);
            let rule = p.as_rule();
            if matches!(rule, Rule::subroutine_dec) {
                self.subroutine_table = SymbolTable::<Subroutine>::new();
            }
            if !is_ignored(&rule) {
                let mut element = Element::from(&p);
                self.handle_identifier(&mut element, &p);
                self.push(element, writer)
            }
            if !is_terminal(&rule) {
                self.scan(p, writer)
            }
            if !is_ignored(&rule) {
                self.pop(writer);
            }
        }
    }

    fn handle_identifier(&mut self, element: &mut Element, pair: &Pair<Rule>) {
        if !is_identifier(&pair.as_rule()) {
            return;
        }
        if let DU::Defined = self.context.du {
            let name = pair.as_str().to_string();
            match self.context.var_kind {
                VarKind::Class(kind) => {
                    self.class_table
                        .insert(name, self.context.type_.clone(), kind);
                }
                VarKind::Subroutine(kind) => {
                    self.subroutine_table
                        .insert(name, self.context.type_.clone(), kind);
                }
            }
        }
        element.metadata(MetaData {
            du: self.context.du,
            category: self.category(pair),
        });
    }

    fn category(&self, pair: &Pair<Rule>) -> Category {
        let rule = pair.as_rule();
        debug_assert!(is_identifier(&rule));
        match rule {
            Rule::class_name => Category::Class,
            Rule::subroutine_name => Category::Subroutine,
            Rule::var_name => {
                let name = pair.as_str();
                // Allow shadowing
                self.subroutine_table
                    .get(&name)
                    .map(Into::into)
                    .or_else(|| self.class_table.get(&name).map(Into::into))
                    .expect("Undeclared variable")
            }
            _ => unreachable!(),
        }
    }

    fn push(&mut self, element: Element, writer: &mut Writer) {
        self.indent(writer);
        if let Some(metadata) = &element.metadata {
            write!(writer, "<{} {}>", element.tag, metadata).unwrap();
        } else {
            write!(writer, "<{}>", element.tag).unwrap();
        }
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

impl fmt::Display for MetaData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "# {:?}, {:?}", self.du, self.category,)
    }
}

impl<'a> Element {
    fn new<R: RuleType>(pair: &Pair<'a, R>, f: impl Fn(&'a str) -> &'a str) -> Self {
        Self::construct(pair, true, f)
    }

    fn tag<R: RuleType>(pair: &Pair<R>) -> Self {
        Self::construct(pair, false, convert::identity)
    }

    fn construct<R: RuleType>(
        pair: &Pair<'a, R>,
        has_body: bool,
        f: impl Fn(&'a str) -> &'a str,
    ) -> Self {
        Self {
            tag: format!("{:?}", pair.as_rule()).to_case(Case::Camel),
            body: has_body.then(|| f(pair.as_str()).to_string()),
            metadata: None,
        }
    }

    fn metadata(&mut self, metadata: MetaData) {
        self.metadata = Some(metadata);
    }
}

impl From<&Pair<'_, Rule>> for Element {
    fn from(pair: &Pair<Rule>) -> Self {
        let rule = pair.as_rule();
        match rule {
            Rule::string_constant => Element::new(pair, |s| s.trim_matches('"')),
            Rule::op => Element::new(pair, |s| symbol_map().get(s).unwrap_or(&s)),
            _ if is_terminal(&rule) => Element::new(pair, convert::identity),
            _ => Element::tag(pair),
        }
    }
}

fn symbol_map() -> HashMap<&'static str, &'static str> {
    [("&", "&amp;"), (">", "&gt;"), ("<", "&lt;")].into()
}

impl Context {
    fn new() -> Self {
        Self {
            du: DU::Defined,
            type_: Type::Void,
            var_kind: VarKind::Class(ClassVarKind::Static),
        }
    }

    fn switch(&mut self, pair: &Pair<Rule>) {
        self.du.switch(pair);
        self.type_.switch(pair);
        self.var_kind.switch(pair);
    }
}

impl DU {
    fn switch(&mut self, pair: &Pair<Rule>) {
        use Rule::*;
        match pair.as_rule() {
            class_var_dec | subroutine_dec | var_dec => *self = DU::Defined,
            let_statement | expression | index_expression | subroutine_call => *self = DU::Used,
            _ => (),
        }
    }
}

impl Type {
    fn switch(&mut self, pair: &Pair<Rule>) {
        use Rule::*;
        match pair.as_rule() {
            type_ | subroutine_type => *self = pair.as_str().parse().expect("Infallible"),
            _ => (),
        }
    }
}

impl FromStr for Type {
    type Err = Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let res = match s {
            "void" => Self::Void,
            "int" => Self::Int,
            "char" => Self::Char,
            "boolean" => Self::Boolean,
            s => Self::Class(s.into()),
        };
        Ok(res)
    }
}

impl VarKind {
    fn switch(&mut self, pair: &Pair<Rule>) {
        use Rule::*;
        match pair.as_rule() {
            class_var_kind => {
                *self = pair
                    .as_str()
                    .parse::<ClassVarKind>()
                    .map(Self::Class)
                    .expect("Infallible")
            }
            parameter_list => *self = Self::Subroutine(SubroutineVarKind::Argument),
            var_dec => *self = Self::Subroutine(SubroutineVarKind::Var),
            _ => (),
        }
    }
}

impl FromStr for ClassVarKind {
    type Err = Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let res = match s {
            "static" => Self::Static,
            "field" => Self::Field,
            s => panic!("Unexpected ClassVarKind: {}", s),
        };
        Ok(res)
    }
}

fn is_terminal(rule: &Rule) -> bool {
    use Rule::*;
    matches!(
        rule,
        integer_constant
            | string_constant
            | class_var_kind
            | type_
            | subroutine_kind
            | subroutine_type
            | class_name
            | subroutine_name
            | var_name
            | op
            | unary_op
            | keyword_constant
    )
}

fn is_ignored(rule: &Rule) -> bool {
    use Rule::*;
    matches!(rule, EOI)
}

fn is_identifier(rule: &Rule) -> bool {
    use Rule::*;
    matches!(rule, class_name | subroutine_name | var_name)
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use std::io::Read;

    #[test]
    fn test() -> std::io::Result<()> {
        const SOURCE_PATH: &str = "../Square/Square.jack";
        const EXPECT_PATH: &str = "Square.stage1.expect.xml";
        const LINES: usize = 338;

        let input = fs::read_to_string(SOURCE_PATH)?;
        let mut file = tempfile::NamedTempFile::new()?;
        let writer = BufWriter::new(file.reopen()?);

        IO { input, writer }.run();

        let mut output = String::new();
        file.read_to_string(&mut output)?;

        let expect = fs::read_to_string(EXPECT_PATH)?;
        let mut count = 0;
        for (output, expect) in output.lines().take(LINES).zip(expect.lines()) {
            assert_eq!(output, expect);
            count += 1;
        }
        assert_eq!(count, LINES);

        Ok(())
    }
}
