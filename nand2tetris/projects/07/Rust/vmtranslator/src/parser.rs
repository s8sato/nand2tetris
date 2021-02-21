use combine::{
    Parser, Stream, attempt, choice, eof, from_str, many, many1,
    parser, satisfy, skip_many1
};
use combine::parser::{
    char::{digit, space, string},
    combinator::recognize,
};
use std::error::Error;
use std::str::FromStr;

use crate::*;

impl FromStr for Command {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(command_().parse(s)?.0)
    }
}

parser! {
    fn command_[Input]()(Input) -> Command
    where [ Input: Stream<Token = char> ]
    {
        choice((
            arithmetic_().skip(eof()).map(|x| Command::Arithmetic(x)),
            attempt(string("push")).with(spaces1_().with(segment_())).and(spaces1_().with(non_nega_i_())).skip(eof())
                .map(|(segment, i)| Command::Push { segment: segment, idx: i }),
            attempt(string("pop")).with(spaces1_().with(segment_())).and(spaces1_().with(non_nega_i_())).skip(eof())
                .map(|(segment, i)| Command::Pop { segment: segment, idx: i }),
            attempt(string("label")).with(spaces1_().with(symbol_())).skip(eof()).map(|x| Command::Label(x)),
            attempt(string("goto")).with(spaces1_().with(symbol_())).skip(eof()).map(|x| Command::Goto(x)),
            attempt(string("if-goto")).with(spaces1_().with(symbol_())).skip(eof()).map(|x| Command::If(x)),
            attempt(string("function")).with(spaces1_().with(symbol_())).and(spaces1_().with(non_nega_i_())).skip(eof())
                .map(|(symbol, i)| Command::Function { symbol: symbol, num: i }),
            attempt(string("call")).with(spaces1_().with(symbol_())).and(spaces1_().with(non_nega_i_())).skip(eof())
                .map(|(symbol, i)| Command::Call { symbol: symbol, num: i }),
            attempt(string("return")).skip(eof()).map(|_| Command::Return),
        ))
    }
}
parser! {
    fn arithmetic_[Input]()(Input) -> Arithmetic
    where [ Input: Stream<Token = char> ]
    {
        choice((
            attempt(string("add")).map(|_| Arithmetic::Add),
            attempt(string("sub")).map(|_| Arithmetic::Sub),
            attempt(string("neg")).map(|_| Arithmetic::Neg),
            attempt(string("eq")).map(|_| Arithmetic::Eq ),
            attempt(string("gt")).map(|_| Arithmetic::Gt ),
            attempt(string("lt")).map(|_| Arithmetic::Lt ),
            attempt(string("and")).map(|_| Arithmetic::And),
            attempt(string("or")).map(|_| Arithmetic::Or ),
            attempt(string("not")).map(|_| Arithmetic::Not),
        ))
    }
}
parser! {
    fn segment_[Input]()(Input) -> Segment
    where [ Input: Stream<Token = char> ]
    {
        choice((
            attempt(string("argument")).map(|_| Segment::Argument),
            attempt(string("local")).map(|_| Segment::Local),
            attempt(string("static")).map(|_| Segment::Static),
            attempt(string("constant")).map(|_| Segment::Constant),
            attempt(string("this")).map(|_| Segment::This),
            attempt(string("that")).map(|_| Segment::That),
            attempt(string("pointer")).map(|_| Segment::Pointer),
            attempt(string("temp")).map(|_| Segment::Temp),
        ))
    }
}
fn is_sign(c: char) -> bool {
    ":._".chars().any(|x| c == x)
}
fn is_head(c: char) -> bool {
    is_sign(c) || c.is_ascii_alphabetic()
}
fn is_tail(c: char) -> bool {
    is_head(c) || c.is_ascii_digit()
}
parser! {
    fn symbol_[Input]()(Input) -> Symbol
    where [
        Input: Stream<Token = char>,
    ]
    {
        recognize((
            satisfy(is_head),
            many::<String, _, _>(satisfy(is_tail)),
        ))
    }
}
parser! {
    fn non_nega_i_[Input]()(Input) -> i32
    where [ Input: Stream<Token = char> ] {
        from_str(many1::<String, _, _>(digit()))
    }
}
parser! {
    fn spaces1_[Input]()(Input) -> ()
    where [ Input: Stream<Token = char> ] {
        skip_many1(space())
    }
}
