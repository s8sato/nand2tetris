use crate::Command;
use crate::Addr;
use crate::Dest;
use crate::{Comp, IR, R, AM};
use crate::Jump;

impl Command {
    pub fn encode(&self) -> u16 {
        match self {
            Command::A(Addr(v)) => *v,
            Command::C { dest, comp, jump } => {
                let c = match *comp {
                    Comp::O                         => 0b0101010, // "0"  
                    Comp::Id(IR::I)                 => 0b0111111, // "1"  
                    Comp::Neg(IR::I)                => 0b0111010, // "-1" 
                    Comp::Id(IR::R(R::D))           => 0b0001100, // "D"  
                    Comp::Id(IR::R(R::AM(AM::A)))   => 0b0110000, // "A"  
                    Comp::Not(R::D)                 => 0b0001101, // "!D" 
                    Comp::Not(R::AM(AM::A))         => 0b0110001, // "!A" 
                    Comp::Neg(IR::R(R::D))          => 0b0001111, // "-D" 
                    Comp::Neg(IR::R(R::AM(AM::A)))  => 0b0110011, // "-A" 
                    Comp::Inc(R::D)                 => 0b0011111, // "D+1"
                    Comp::Inc(R::AM(AM::A))         => 0b0110111, // "A+1"
                    Comp::Dec(R::D)                 => 0b0001110, // "D-1"
                    Comp::Dec(R::AM(AM::A))         => 0b0110010, // "A-1"
                    Comp::AddD(AM::A)               => 0b0000010, // "D+A"
                    Comp::SubD(AM::A)               => 0b0010011, // "D-A"
                    Comp::SubXD(AM::A)              => 0b0000111, // "A-D"
                    Comp::AndD(AM::A)               => 0b0000000, // "D&A"
                    Comp::OrD(AM::A)                => 0b0010101, // "D|A"
                    Comp::Id(IR::R(R::AM(AM::M)))   => 0b1110000, // "M"  
                    Comp::Not(R::AM(AM::M))         => 0b1110001, // "!M" 
                    Comp::Neg(IR::R(R::AM(AM::M)))  => 0b1110011, // "-M" 
                    Comp::Inc(R::AM(AM::M))         => 0b1110111, // "M+1"
                    Comp::Dec(R::AM(AM::M))         => 0b1110010, // "M-1"
                    Comp::AddD(AM::M)               => 0b1000010, // "D+M"
                    Comp::SubD(AM::M)               => 0b1010011, // "D-M"
                    Comp::SubXD(AM::M)              => 0b1000111, // "M-D"
                    Comp::AndD(AM::M)               => 0b1000000, // "D&M"
                    Comp::OrD(AM::M)                => 0b1010101, // "D|M"
                };
                let d = match *dest {
                    None => 0b000,
                    Some(Dest { a, d, m }) => {
                        [a, d, m].iter().fold(0b0, |acc, x| (acc << 1) + *x as u16)
                    }
                };
                let j = match *jump {
                    None => 0b000,
                    Some(Jump { lt, eq, gt }) => {
                        [lt, eq, gt].iter().fold(0b0, |acc, x| (acc << 1) + *x as u16)
                    }
                };
                (0b111 << 13) + [c, d, j].iter().fold(0b0, |acc, x| (acc << 3) + x)
            },
            _ => unreachable!()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bit_shift() {
        let c = 0b1010101;
        let d = 0b010;
        let j = 0b101;
        let trial = [c, d, j].iter().fold(0b0, |acc, x| (acc << 3) + x);
        let expect = 0b1010101_010_101;
        assert_eq!(trial, expect);
    }
    #[test]
    fn cmd_a() {
        let trial = Command::A(Addr(32767)).encode();
        let expect = 0b0111111111111111;
        assert_eq!(trial, expect);
    }
    #[test]
    fn cmd_c() {
        let c = Comp::OrD(AM::M);
        let d = Dest { a:false, d:true, m:false };
        let j = Jump { lt:true, eq:false, gt:true };
        let trial = Command::C { dest: Some(d), comp: c, jump: Some(j) }.encode();
        let expect = 0b111_1010101_010_101;
        assert_eq!(trial, expect);
    }
    #[test]
    fn bool_bit() {
        let trial = true as u16 ;
        let expect = 1;
        assert_eq!(trial, expect);
    }
}
