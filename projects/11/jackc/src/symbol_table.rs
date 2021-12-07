use crate::{Category, ClassVarKind, Index, Name, SubroutineVarKind, Type};
use std::collections::HashMap;
use std::hash::Hash;

pub struct SymbolTable<S: Scope> {
    body: HashMap<Name, Record<S>>,
    counter: HashMap<S::VarKind, Index>,
}

pub trait Scope {
    type VarKind: Copy + Eq + Hash;
}

pub struct Record<S: Scope> {
    #[allow(dead_code)]
    type_: Type,
    kind: S::VarKind,
    index: Index,
}

pub struct Class;

pub struct Subroutine;

impl Scope for Class {
    type VarKind = ClassVarKind;
}

impl Scope for Subroutine {
    type VarKind = SubroutineVarKind;
}

impl SymbolTable<Class> {
    pub fn new() -> Self {
        Self {
            body: HashMap::new(),
            counter: HashMap::new(),
        }
    }
}

impl SymbolTable<Subroutine> {
    pub fn new() -> Self {
        Self {
            body: HashMap::new(),
            // Leave "argument 0" free for the "this" keyword
            counter: [(SubroutineVarKind::Argument, 1)].into(),
        }
    }
}

impl<S: Scope> SymbolTable<S> {
    pub fn get(&self, name: &str) -> Option<&Record<S>> {
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
