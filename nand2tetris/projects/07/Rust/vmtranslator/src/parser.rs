use combine::{ParseError, Parser, Stream, attempt, between, choice, eof, from_str, look_ahead, many, many1, parser, satisfy, token};
use combine::parser::char::{digit, string};
use combine::parser::token::any;
use combine::error::StreamError;
use std::str::FromStr;
use std::error::Error;

use crate::Command;

impl FromStr for Command {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(a_command().parse(s)?.0)
    }
}

parser! {
    fn a_command[Input]()(Input) -> Command
    where [ Input: Stream<Token = char> ]
    {
        string("push constant ").with(many(any())).map(|s| Command { raw: s })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use combine::EasyParser;

    #[test]
    fn t_a_() {
        assert_eq!(0, 0);
    }
}
