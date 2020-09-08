use std::collections::HashMap;

use crate::Symbol;
use crate::Addr;

pub type SymbolTable = HashMap<Symbol, Addr>;

pub fn new() -> SymbolTable {
    [ (String::from("SP"    ), 0     )
    , (String::from("LCL"   ), 1     )
    , (String::from("ARG"   ), 2     )
    , (String::from("THIS"  ), 3     )
    , (String::from("THAT"  ), 4     )
    , (String::from("SCREEN"), 0x4000)
    , (String::from("KBD"   ), 0x6000)
    ].iter().cloned().chain((0..=15).map(|a| (format!("R{}", a), a)))
    .map(|(s, a)| (Symbol(s), Addr(a))).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initial_table() {
        let table = new();
        assert_eq!(table[&Symbol(String::from("SP"    ))], Addr(0     ));
        assert_eq!(table[&Symbol(String::from("LCL"   ))], Addr(1     ));
        assert_eq!(table[&Symbol(String::from("ARG"   ))], Addr(2     ));
        assert_eq!(table[&Symbol(String::from("THIS"  ))], Addr(3     ));
        assert_eq!(table[&Symbol(String::from("THAT"  ))], Addr(4     ));
        assert_eq!(table[&Symbol(String::from("SCREEN"))], Addr(0x4000));
        assert_eq!(table[&Symbol(String::from("KBD"   ))], Addr(0x6000));
        assert_eq!(table[&Symbol(String::from("R0"    ))], Addr(0     ));
        assert_eq!(table[&Symbol(String::from("R15"   ))], Addr(15    ));
    }
}
