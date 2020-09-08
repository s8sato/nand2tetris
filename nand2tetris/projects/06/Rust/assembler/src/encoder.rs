use crate::Command;
use crate::Addr;
use crate::Dest;
use crate::Comp;
use crate::Jump;

impl Command {
    pub fn encode(&self) -> u16 {
        match self {
            Command::A(Addr(v)) => *v,
            Command::C { dest, comp: Comp(s), jump } => {
                let c = match &**s {
                    "0"   => 0b0101010,
                    "1"   => 0b0111111,
                    "-1"  => 0b0111010,
                    "D"   => 0b0001100,
                    "A"   => 0b0110000,
                    "!D"  => 0b0001101,
                    "!A"  => 0b0110001,
                    "-D"  => 0b0001111,
                    "-A"  => 0b0110011,
                    "D+1" => 0b0011111,
                    "A+1" => 0b0110111,
                    "D-1" => 0b0001110,
                    "A-1" => 0b0110010,
                    "D+A" => 0b0000010,
                    "D-A" => 0b0010011,
                    "A-D" => 0b0000111,
                    "D&A" => 0b0000000,
                    "D|A" => 0b0010101,
                    "M"   => 0b1110000,
                    "!M"  => 0b1110001,
                    "-M"  => 0b1110011,
                    "M+1" => 0b1110111,
                    "M-1" => 0b1110010,
                    "D+M" => 0b1000010,
                    "D-M" => 0b1010011,
                    "M-D" => 0b1000111,
                    "D&M" => 0b1000000,
                    "D|M" => 0b1010101,
                    _     => unreachable!()
                };
                let d = match dest {
                    None      => 0b000,
                    Some(Dest(s)) => match &**s {
                        "M"   => 0b001,
                        "D"   => 0b010,
                        "MD"  => 0b011,
                        "A"   => 0b100,
                        "AM"  => 0b101,
                        "AD"  => 0b110,
                        "AMD" => 0b111,
                        _     => unreachable!()
                    }
                };
                let j = match jump {
                    None      => 0b000,
                    Some(Jump(s)) => match &**s {
                        "JGT" => 0b001,
                        "JEQ" => 0b010,
                        "JGE" => 0b011,
                        "JLT" => 0b100,
                        "JNE" => 0b101,
                        "JLE" => 0b110,
                        "JMP" => 0b111,
                        _     => unreachable!()
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
        let c = Comp("D|M".to_string());
        let d = Dest("D".to_string());
        let j = Jump("JNE".to_string());
        let trial = Command::C { dest: Some(d), comp: c, jump: Some(j) }.encode();
        let expect = 0b111_1010101_010_101;
        assert_eq!(trial, expect);
    }
}
