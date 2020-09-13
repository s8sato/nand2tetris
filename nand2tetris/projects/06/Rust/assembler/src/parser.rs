use combine::{ParseError, Parser, Stream, attempt, between, choice, eof, from_str, look_ahead, many, many1, optional, parser, satisfy, token, value};
use combine::parser::char::{digit, string};
use combine::error::StreamError;
use std::str::FromStr;
use std::error::Error;

use crate::Label;
use crate::Command;
use crate::Symbol;
use crate::Addr;
use crate::Dest;
use crate::{Comp, IR, R, AM};
use crate::Jump;

impl FromStr for Label {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(a_label().parse(s)?.0)
    }
}

impl FromStr for Command {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(a_command().parse(s)?.0)
    }
}

parser! {
    fn a_label[Input]()(Input) -> Label
    where [
        Input: Stream<Token = char>,
    ]
    {
        between(token('('), token(')'), a_symbol()).skip(eof())
            .map(|x| Label(x))
    }
}

parser! {
    fn a_command[Input]()(Input) -> Command
    where [
        Input: Stream<Token = char>,
    ]
    {
        choice((
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
        satisfy(is_head).and(many(satisfy(is_tail)))
            .map(|(h, t): (_, String)| Symbol(format!("{}{}", h, t)))
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
        optional(token('A')).and(optional(token('M'))).and(optional(token('D')))
            .and_then(|((a, m), d)| { // TODO flatten tuple
                let a = a.is_some();
                let d = d.is_some();
                let m = m.is_some();
                if [a, d, m].iter().all(|x| !x) {
                    return Err(
                        <Input::Error as ParseError<Input::Token, Input::Range, Input::Position>>::StreamError
                        ::message_static_message("") // TODO find better way
                    )
                }
                Ok(Dest { a:a, d:d, m:m })
            })
    }
}

parser! {
    fn a_comp[Input]()(Input) -> Comp
    where [
        Input: Stream<Token = char>,
    ]
    {
        choice((
            attempt(token('0').skip(look_ahead(token(';')))
                .map(|_| Comp::O)),
            attempt(a_ir().skip(look_ahead(token(';')))
                .map(|x| Comp::Id(x))),
            attempt(token('-').with(a_ir()).skip(look_ahead(token(';')))
                .map(|x| Comp::Neg(x))),
            attempt(token('!').with(a_r()).skip(look_ahead(token(';')))
                .map(|x| Comp::Not(x))),
            attempt(a_r().skip(string("+1")).skip(look_ahead(token(';')))
                .map(|x| Comp::Inc(x))),
            attempt(a_r().skip(string("-1")).skip(look_ahead(token(';')))
                .map(|x| Comp::Dec(x))),
            attempt(string("D+").with(a_am()).skip(look_ahead(token(';')))
                .map(|x| Comp::AddD(x))),
            attempt(string("D-").with(a_am()).skip(look_ahead(token(';')))
                .map(|x| Comp::SubD(x))),
            attempt(a_am().skip(string("-D")).skip(look_ahead(token(';')))
                .map(|x| Comp::SubXD(x))),
            attempt(string("D&").with(a_am()).skip(look_ahead(token(';')))
                .map(|x| Comp::AndD(x))),
            attempt(string("D|").with(a_am()).skip(look_ahead(token(';')))
                .map(|x| Comp::OrD(x))),
        )).or(choice((
            attempt(token('0').skip(look_ahead(eof()))
                .map(|_| Comp::O)),
            attempt(a_ir().skip(look_ahead(eof()))
                .map(|x| Comp::Id(x))),
            attempt(token('-').with(a_ir()).skip(look_ahead(eof()))
                .map(|x| Comp::Neg(x))),
            attempt(token('!').with(a_r()).skip(look_ahead(eof()))
                .map(|x| Comp::Not(x))),
            attempt(a_r().skip(string("+1")).skip(look_ahead(eof()))
                .map(|x| Comp::Inc(x))),
            attempt(a_r().skip(string("-1")).skip(look_ahead(eof()))
                .map(|x| Comp::Dec(x))),
            attempt(string("D+").with(a_am()).skip(look_ahead(eof()))
                .map(|x| Comp::AddD(x))),
            attempt(string("D-").with(a_am()).skip(look_ahead(eof()))
                .map(|x| Comp::SubD(x))),
            attempt(a_am().skip(string("-D")).skip(look_ahead(eof()))
                .map(|x| Comp::SubXD(x))),
            attempt(string("D&").with(a_am()).skip(look_ahead(eof()))
                .map(|x| Comp::AndD(x))),
            attempt(string("D|").with(a_am()).skip(look_ahead(eof()))
                .map(|x| Comp::OrD(x))),
        )))
    }
}

parser! {
    fn a_ir[Input]()(Input) -> IR
    where [
        Input: Stream<Token = char>,
    ]
    {
        choice((
            token('1').map(|_| IR::I),
            a_r().map(|x| IR::R(x)),
        ))
    }
}

parser! {
    fn a_r[Input]()(Input) -> R
    where [
        Input: Stream<Token = char>,
    ]
    {
        choice((
            token('D').map(|_| R::D),
            a_am().map(|x| R::AM(x)),
        ))
    }
}

parser! {
    fn a_am[Input]()(Input) -> AM
    where [
        Input: Stream<Token = char>,
    ]
    {
        choice((
            token('A').map(|_| AM::A),
            token('M').map(|_| AM::M),
        ))
    }
}

parser! {
    fn a_jump[Input]()(Input) -> Jump
    where [
        Input: Stream<Token = char>,
    ]
    {
        token('J').with(choice((
            attempt(choice([
                token('L'),
                token('G'),
            ]).and(choice([
                token('E'),
                token('T'),
            ])).then(|(lg, et)| {
                let l = lg == 'L';
                let e = et == 'E';
                let g = lg == 'G';
                value((l, e, g))
            })),
            attempt(string("EQ").with(value((false, true , false)))),
            attempt(string("NE").with(value((true , false, true )))),
            attempt(string("MP").with(value((true , true , true )))),
        ))).map(|(l, e, g)| Jump { lt:l, eq:e, gt:g })
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
        assert!(trial.is_err());
        eprintln!("{:?}", trial);
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
            dest: Some(Dest { a:true, d:true, m:true }),
            comp: Comp::AndD(AM::M),
            jump: Some(Jump { lt:true, eq:true, gt:true }),
        }, ""));
        assert_eq!(trial, expect);
    }
    #[test]
    fn cmd_c_dc() {
        let trial = a_command().easy_parse("D=0");
        let expect = Ok((Command::C {
            dest: Some(Dest { a:false, d:true, m:false }),
            comp: Comp::O,
            jump: None,
        }, ""));
        assert_eq!(trial, expect);
    }
    #[test]
    fn cmd_c_cj() {
        let trial = a_command().easy_parse("0;JMP");
        let expect = Ok((Command::C {
            dest: None,
            comp: Comp::O,
            jump: Some(Jump { lt:true, eq:true, gt:true }),
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
    fn label() {
        let trial = a_label().easy_parse("(LABEL)");
        let expect = Ok((Label(Symbol("LABEL".to_string())), ""));
        assert_eq!(trial, expect);
    }
    #[test]
    fn cmd_v() {
        let trial = a_command().easy_parse("@variable");
        let expect = Ok((Command::V(Symbol("variable".to_string())), ""));
        assert_eq!(trial, expect);
    }
}
