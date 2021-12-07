use std::collections::HashMap;

trait Scope {}

struct Class;
struct Subroutine;

impl Scope for Class {}
impl Scope for Subroutine {}

trait SymbolTable<S: Scope> {}

pub struct ClassTable {
    body: HashMap<Name, ClassSuite>,
    counter: HashMap<ClassKind, Index>,
}

pub struct SubroutineTable {
    body: HashMap<Name, SubroutineSuite>,
    counter: HashMap<SubroutineKind, Index>,
}

impl SymbolTable<Class> for ClassTable {}
impl SymbolTable<Subroutine> for SubroutineTable {}

trait SymbolSuite<S: Scope> {}

struct ClassSuite {
    type_: Type,
    kind: ClassKind,
    index: Index,
}
struct SubroutineSuite {
    type_: Type,
    kind: SubroutineKind,
    index: Index,
}

impl SymbolSuite<Class> for ClassSuite {}
impl SymbolSuite<Subroutine> for SubroutineSuite {}

trait Kind<S: Scope> {}

enum ClassKind {
    Static,
    Field,
}
enum SubroutineKind {
    Argument,
    Var,
}

impl Kind<Class> for ClassKind {}
impl Kind<Subroutine> for SubroutineKind {}

type Name = String;
type Index = u32;

enum Type {
    Hoge,
    Fuga,
}
