pub enum Command {
    Arithmetic(Arithmetic),
    Push { segment: Segment, idx: u32 },
    Pop { segment: Segment, idx: u32 },
    Label(Symbol),
    Goto(Symbol),
    IfGoto(Symbol),
    Function { f: Symbol, n_locals: u32 },
    Call { f: Symbol, n_args: u32 },
    Return,
}

pub enum Arithmetic {
    Add,
    Sub,
    Neg,
    Eq,
    Gt,
    Lt,
    And,
    Or,
    Not,
}

pub enum Segment {
    Constant,
    Local,
    Argument,
    This,
    That,
    Pointer,
    Temp,
    Static,
}

pub type Symbol = String;
