use std::collections::HashMap;

pub struct SymbolTable<S: Scope> {
    body: HashMap<Name, S::Record>,
    counter: HashMap<S::Kind, Index>,
}

pub trait Scope {
    type Record;
    type Kind;
}

struct Class;
struct Subroutine;

impl Scope for Class {
    type Record = Record<Self>;
    type Kind = ClassKind;
}
impl Scope for Subroutine {
    type Record = Record<Self>;
    type Kind = SubroutineKind;
}

struct Record<S: Scope> {
    type_: Type,
    kind: S::Kind,
    index: Index,
}

enum ClassKind {
    Static,
    Field,
}
enum SubroutineKind {
    Argument,
    Var,
}

type Name = String;
type Index = u32;

enum Type {
    Hoge,
    Fuga,
}
