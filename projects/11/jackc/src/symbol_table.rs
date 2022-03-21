use crate::Category;
use enum_iterator::IntoEnumIterator;
use std::collections::HashMap;
use std::hash::Hash;

pub struct SymbolTable<S: Scope> {
    body: HashMap<Name, Record<S>>,
    counter: HashMap<S::Kind, Index>,
}

pub trait Scope {
    type Kind: Eq + Hash + IntoEnumIterator;
}

pub struct Class;
pub struct Subroutine;

impl Scope for Class {
    type Kind = ClassKind;
}
impl Scope for Subroutine {
    type Kind = SubroutineKind;
}

struct Record<S: Scope> {
    type_: Type,
    kind: S::Kind,
    index: Index,
}

#[derive(PartialEq, Eq, Hash, IntoEnumIterator)]
enum ClassKind {
    Static,
    Field,
}
#[derive(PartialEq, Eq, Hash, IntoEnumIterator)]
enum SubroutineKind {
    Argument,
    Var,
}

type Name = String;
type Index = u32;

pub enum Type {
    Int,
    Char,
    Boolean,
    Class(Name),
}

impl<S: Scope> SymbolTable<S> {
    pub fn new() -> Self {
        Self {
            body: HashMap::new(),
            counter: S::Kind::into_enum_iter().map(|k| (k, 0)).collect(),
        }
    }

    pub fn get(&self, name: &Name) -> Option<&Record<S>> {
        self.body.get(name)
    }

    pub fn insert(&mut self, name: Name, kind: S::Kind, type_: Type) -> Option<Record<S>> {
        let mut index = self.counter.entry(kind).or_default();
        let record = Record::<S>::new(type_, kind, *index);
        let res = self.body.insert(name, record);
        *index += 1;
        res
    }
}

impl<S: Scope> Record<S> {
    fn new(type_: Type, kind: S::Kind, index: Index) -> Self {
        Self { type_, kind, index }
    }
}

impl From<&Record<Class>> for Category {
    fn from(record: &Record<Class>) -> Self {
        match record.kind {
            ClassKind::Static => Category::Static(record.index),
            ClassKind::Field => Category::Field(record.index),
        }
    }
}

impl From<&Record<Subroutine>> for Category {
    fn from(record: &Record<Subroutine>) -> Self {
        match record.kind {
            SubroutineKind::Argument => Category::Argument(record.index),
            SubroutineKind::Var => Category::Var(record.index),
        }
    }
}
