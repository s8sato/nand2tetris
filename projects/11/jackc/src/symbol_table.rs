use crate::Category;
use enum_iterator::IntoEnumIterator;
use std::collections::HashMap;
use std::convert::Infallible;
use std::hash::Hash;

pub struct SymbolTable<S: Scope> {
    body: HashMap<Name, Record<S>>,
    counter: HashMap<S::VarKind, Index>,
}

pub trait Scope {
    type VarKind: Copy + Eq + Hash + IntoEnumIterator;
}

pub struct Class;
pub struct Subroutine;

impl Scope for Class {
    type VarKind = ClassVarKind;
}
impl Scope for Subroutine {
    type VarKind = SubroutineVarKind;
}

pub struct Record<S: Scope> {
    type_: Type,
    kind: S::VarKind,
    index: Index,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, IntoEnumIterator)]
pub enum ClassVarKind {
    Static,
    Field,
}
#[derive(Clone, Copy, PartialEq, Eq, Hash, IntoEnumIterator)]
pub enum SubroutineVarKind {
    Argument,
    Var,
}

type Name = String;
type Index = u32;

#[derive(Clone)]
pub enum Type {
    Void,
    Int,
    Char,
    Boolean,
    Class(Name),
}

impl std::str::FromStr for ClassVarKind {
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

impl std::str::FromStr for Type {
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

impl<S: Scope> SymbolTable<S> {
    pub fn new() -> Self {
        Self {
            body: HashMap::new(),
            counter: S::VarKind::into_enum_iter().map(|k| (k, 0)).collect(),
        }
    }

    pub fn get(&self, name: &Name) -> Option<&Record<S>> {
        self.body.get(name)
    }

    pub fn insert(&mut self, name: Name, type_: Type, kind: S::VarKind) -> Option<Record<S>> {
        let index = self.counter.entry(kind).or_default();
        let record = Record::<S>::new(type_, kind, *index);
        let res = self.body.insert(name, record);
        *index += 1;
        res
    }
}

impl<S: Scope> Record<S> {
    fn new(type_: Type, kind: S::VarKind, index: Index) -> Self {
        Self { type_, kind, index }
    }
}

impl From<&Record<Class>> for Category {
    fn from(record: &Record<Class>) -> Self {
        match record.kind {
            ClassVarKind::Static => Category::Static(record.index),
            ClassVarKind::Field => Category::Field(record.index),
        }
    }
}

impl From<&Record<Subroutine>> for Category {
    fn from(record: &Record<Subroutine>) -> Self {
        match record.kind {
            SubroutineVarKind::Argument => Category::Argument(record.index),
            SubroutineVarKind::Var => Category::Var(record.index),
        }
    }
}
