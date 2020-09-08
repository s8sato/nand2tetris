use combine::{ParseError, Parser, Stream, attempt, between, choice, eof, from_str, look_ahead, many, many1, parser, satisfy, token};
use combine::parser::char::{digit, string};
use combine::error::StreamError;
use std::str::FromStr;
use std::error::Error;

use crate::Command;
use crate::Symbol;
use crate::Addr;
use crate::Dest;
use crate::Comp;
use crate::Jump;

impl FromStr for Command {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(a_command().parse(s)?.0)
    }
}

parser! {
    fn a_command[Input]()(Input) -> Command
    where [
        Input: Stream<Token = char>,
    ]
    {
        choice((
            attempt(between(token('('), token(')'), a_symbol()).skip(eof())
                .map(|x| Command::L(x))),
            attempt(token('@').with(a_symbol()).skip(eof())
                .map(|x| Command::V(x))),
            attempt(token('@').with(a_addr()).skip(eof())
                .map(|x| Command::A(x))),
            attempt(a_dest().skip(token('=')).and(a_comp()).skip(token(';')).and(a_jump()).skip(eof())
                // TODO flatten tuple
                .map(|((d, c), j)| Command::C { dest: Some(d), comp: c, jump: Some(j) })),
            attempt(a_dest().skip(token('=')).and(a_comp()).skip(eof())
                .map(|(d, c)| Command::C { dest: Some(d), comp: c, jump: None })),
            attempt(a_comp().skip(token(';')).and(a_jump()).skip(eof())
                .map(|(c, j)| Command::C { dest: None, comp: c, jump: Some(j) })),
        ))
    }
}

fn is_sign(c: char) -> bool {
    "$:._".chars().any(|s| c == s)
}
fn is_head(c: char) -> bool {
    is_sign(c) || c.is_ascii_alphabetic()
}
fn is_tail(c: char) -> bool {
    is_head(c) || c.is_ascii_digit()
}

parser! {
    fn a_symbol[Input]()(Input) -> Symbol
    where [
        Input: Stream<Token = char>,
    ]
    {
        satisfy(is_head).and(many(satisfy(is_tail))).map(|(h, t): (_, String)| {
            Symbol(format!("{}{}", h, t))
        })
    }
}

parser! {
    fn a_addr[Input]()(Input) -> Addr 
    where [
        Input: Stream<Token = char>,
    ]
    {
        decimal().and_then(|x| {
            Addr::new(x).map_err(|_| {
                <Input::Error as ParseError<Input::Token, Input::Range, Input::Position>>::StreamError
                ::message_static_message("") // TODO find better way
            })
        })
    }
}

parser! {
    fn decimal[Input]()(Input) -> u16
    where [
        Input: Stream<Token = char>,
    ]
    {
        from_str(many1::<String, _, _>(digit()))
    }
}

parser! {
    fn a_dest[Input]()(Input) -> Dest
    where [
        Input: Stream<Token = char>,
    ]
    {
        // choice([
        //     "M"  ,
        //     "D"  ,
        //     "MD" ,
        //     "A"  ,
        //     "AM" ,
        //     "AD" ,
        //     "AMD",
        //     ].iter().map(|x| attempt(string(x).skip(token('=')))).collect()
        // ).map(|s| Dest(s.to_string()))

        // TODO reduce repetition

        choice([
            attempt(string("M"  ).skip(look_ahead(token('=')))),
            attempt(string("D"  ).skip(look_ahead(token('=')))),
            attempt(string("MD" ).skip(look_ahead(token('=')))),
            attempt(string("A"  ).skip(look_ahead(token('=')))),
            attempt(string("AM" ).skip(look_ahead(token('=')))),
            attempt(string("AD" ).skip(look_ahead(token('=')))),
            attempt(string("AMD").skip(look_ahead(token('=')))),
        ]).map(|s| Dest(s.to_string()))
    }
}

parser! {
    fn a_comp[Input]()(Input) -> Comp
    where [
        Input: Stream<Token = char>,
    ]
    {
        choice([
            attempt(string("0"  ).skip(look_ahead(token(';')))),
            attempt(string("1"  ).skip(look_ahead(token(';')))),
            attempt(string("-1" ).skip(look_ahead(token(';')))),
            attempt(string("D"  ).skip(look_ahead(token(';')))),
            attempt(string("A"  ).skip(look_ahead(token(';')))),
            attempt(string("!D" ).skip(look_ahead(token(';')))),
            attempt(string("!A" ).skip(look_ahead(token(';')))),
            attempt(string("-D" ).skip(look_ahead(token(';')))),
            attempt(string("-A" ).skip(look_ahead(token(';')))),
            attempt(string("D+1").skip(look_ahead(token(';')))),
            attempt(string("A+1").skip(look_ahead(token(';')))),
            attempt(string("D-1").skip(look_ahead(token(';')))),
            attempt(string("A-1").skip(look_ahead(token(';')))),
            attempt(string("D+A").skip(look_ahead(token(';')))),
            attempt(string("D-A").skip(look_ahead(token(';')))),
            attempt(string("A-D").skip(look_ahead(token(';')))),
            attempt(string("D&A").skip(look_ahead(token(';')))),
            attempt(string("D|A").skip(look_ahead(token(';')))),
            attempt(string("M"  ).skip(look_ahead(token(';')))),
            attempt(string("!M" ).skip(look_ahead(token(';')))),
            attempt(string("-M" ).skip(look_ahead(token(';')))),
            attempt(string("M+1").skip(look_ahead(token(';')))),
            attempt(string("M-1").skip(look_ahead(token(';')))),
            attempt(string("D+M").skip(look_ahead(token(';')))),
            attempt(string("D-M").skip(look_ahead(token(';')))),
            attempt(string("M-D").skip(look_ahead(token(';')))),
            attempt(string("D&M").skip(look_ahead(token(';')))),
            attempt(string("D|M").skip(look_ahead(token(';')))),
        ]).or(choice([
            attempt(string("0"  ).skip(look_ahead(eof()))),
            attempt(string("1"  ).skip(look_ahead(eof()))),
            attempt(string("-1" ).skip(look_ahead(eof()))),
            attempt(string("D"  ).skip(look_ahead(eof()))),
            attempt(string("A"  ).skip(look_ahead(eof()))),
            attempt(string("!D" ).skip(look_ahead(eof()))),
            attempt(string("!A" ).skip(look_ahead(eof()))),
            attempt(string("-D" ).skip(look_ahead(eof()))),
            attempt(string("-A" ).skip(look_ahead(eof()))),
            attempt(string("D+1").skip(look_ahead(eof()))),
            attempt(string("A+1").skip(look_ahead(eof()))),
            attempt(string("D-1").skip(look_ahead(eof()))),
            attempt(string("A-1").skip(look_ahead(eof()))),
            attempt(string("D+A").skip(look_ahead(eof()))),
            attempt(string("D-A").skip(look_ahead(eof()))),
            attempt(string("A-D").skip(look_ahead(eof()))),
            attempt(string("D&A").skip(look_ahead(eof()))),
            attempt(string("D|A").skip(look_ahead(eof()))),
            attempt(string("M"  ).skip(look_ahead(eof()))),
            attempt(string("!M" ).skip(look_ahead(eof()))),
            attempt(string("-M" ).skip(look_ahead(eof()))),
            attempt(string("M+1").skip(look_ahead(eof()))),
            attempt(string("M-1").skip(look_ahead(eof()))),
            attempt(string("D+M").skip(look_ahead(eof()))),
            attempt(string("D-M").skip(look_ahead(eof()))),
            attempt(string("M-D").skip(look_ahead(eof()))),
            attempt(string("D&M").skip(look_ahead(eof()))),
            attempt(string("D|M").skip(look_ahead(eof()))),
        ])).map(|s| Comp(s.to_string()))
    }
}

parser! {
    fn a_jump[Input]()(Input) -> Jump
    where [
        Input: Stream<Token = char>,
    ]
    {
        choice([
            attempt(string("JGT")),
            attempt(string("JEQ")),
            attempt(string("JGE")),
            attempt(string("JLT")),
            attempt(string("JNE")),
            attempt(string("JLE")),
            attempt(string("JMP")),
        ]).map(|s| Jump(s.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use combine::EasyParser;

    #[test]
    fn i32_u16() {
        let i = 2i32.pow(15)-1 ;
        assert_eq!(i as u16, 0x7FFFu16);
    }
    #[test]
    fn addr_under() {
        let trial = a_addr().easy_parse("-1");
        assert!(trial.is_err());
    }
    #[test]
    fn addr_min() {
        let trial = a_addr().easy_parse("0");
        let expect = Ok((Addr(0), ""));
        assert_eq!(trial, expect);
    }
    #[test]
    fn addr_max() {
        let trial = a_addr().easy_parse("32767");
        let expect = Ok((Addr(0x7FFF), ""));
        assert_eq!(trial, expect);
    }
    #[test]
    fn addr_over() {
        let trial = a_addr().easy_parse("32768");
        eprintln!("{:?}", trial);
        assert!(trial.is_err());
    }
    #[test]
    fn cmd_a() {
        let trial = a_command().easy_parse("@32767");
        let expect = Ok((Command::A(Addr(0x7FFF)), ""));
        assert_eq!(trial, expect);
    }
    #[test]
    fn cmd_c_dcj() {
        let trial = a_command().easy_parse("AMD=D&M;JMP");
        let expect = Ok((Command::C {
            dest: Some(Dest("AMD".to_string())),
            comp: Comp("D&M".to_string()),
            jump: Some(Jump("JMP".to_string())),
        }, ""));
        assert_eq!(trial, expect);
    }
    #[test]
    fn cmd_c_dc() {
        let trial = a_command().easy_parse("D=0");
        let expect = Ok((Command::C {
            dest: Some(Dest("D".to_string())),
            comp: Comp("0".to_string()),
            jump: None,
        }, ""));
        assert_eq!(trial, expect);
    }
    #[test]
    fn cmd_c_cj() {
        let trial = a_command().easy_parse("0;JMP");
        let expect = Ok((Command::C {
            dest: None,
            comp: Comp("0".to_string()),
            jump: Some(Jump("JMP".to_string())),
        }, ""));
        assert_eq!(trial, expect);
    }
    #[test]
    fn cmd_err_1() {
        let trial = a_command().easy_parse("D=0;");
        assert!(trial.is_err());
    }
    #[test]
    fn cmd_err_2() {
        let trial = a_command().easy_parse("=0;JMP");
        assert!(trial.is_err());
    }
    #[test]
    fn cmd_err_3() {
        let trial = a_command().easy_parse("DAM=M|D");
        assert!(trial.is_err());
    }
    #[test]
    fn cmd_err_4() {
        let trial = a_command().easy_parse("0");
        assert!(trial.is_err());
    }
    #[test]
    fn cmd_err_5() {
        let trial = a_command().easy_parse("0;JMPoo");
        assert!(trial.is_err());
    }
    #[test]
    fn symbol_err_1() {
        let trial = a_symbol().easy_parse("1symbol");
        assert!(trial.is_err());
    }
    #[test]
    fn symbol_err_2() {
        let trial = a_symbol().easy_parse(r"!#%&'()-^@[;],/\=~|`{+*}>?");
        assert!(trial.is_err());
    }
    #[test]
    fn symbol_ok_1() {
        let trial = a_symbol().easy_parse("s1ymbol");
        assert!(trial.is_ok());
    }
    #[test]
    fn symbol_ok_2() {
        let trial = a_symbol().easy_parse("$:._");
        assert!(trial.is_ok());
    }
    #[test]
    fn cmd_l() {
        let trial = a_command().easy_parse("(LABEL)");
        let expect = Ok((Command::L(Symbol("LABEL".to_string())), ""));
        assert_eq!(trial, expect);
    }
    #[test]
    fn cmd_v() {
        let trial = a_command().easy_parse("@variable");
        let expect = Ok((Command::V(Symbol("variable".to_string())), ""));
        assert_eq!(trial, expect);
    }
}
