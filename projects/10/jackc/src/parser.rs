use crate::prelude::*;

#[derive(Parser)]
#[grammar = "jack.pest"]
struct Jack;

pub fn run(io: IO) {
    let file = Jack::parse(Rule::file, &io.input)
        .expect("Failed to parse input")
        .next()
        .unwrap();

    let mut writer = io.writer;
    let mut stack = Stack::new();

    stack.scan(file, &mut writer);

    writer.flush().unwrap()
}

impl Stack {
    fn scan(&mut self, pair: Pair<Rule>, writer: &mut Writer) {
        for p in pair.into_inner() {
            let rule = p.as_rule();
            if !is_ignored(&rule) {
                self.push((&p).into(), writer)
            }
            if !is_terminal(&rule) {
                self.scan(p, writer)
            }
            if !is_ignored(&rule) {
                self.pop(writer);
            }
        }
    }
}

impl From<&Pair<'_, Rule>> for Element {
    fn from(pair: &Pair<Rule>) -> Self {
        let rule = pair.as_rule();
        match rule {
            Rule::string_constant => Element::new(pair, |s| s.trim_matches('"')),
            Rule::op => Element::new(pair, |s| symbol_map().get(s).unwrap_or(&s)),
            _ if is_terminal(&rule) => Element::new(pair, identity),
            _ => Element::tag(pair),
        }
    }
}

fn is_terminal(rule: &Rule) -> bool {
    use Rule::*;
    matches!(
        rule,
        integer_constant
            | string_constant
            | class_var_kind
            | type_
            | subroutine_kind
            | subroutine_type
            | class_name
            | subroutine_name
            | var_name
            | op
            | unary_op
            | keyword_constant
    )
}

fn is_ignored(rule: &Rule) -> bool {
    use Rule::*;
    matches!(rule, EOI | statement | index_expression | subroutine_call)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::Convert;
    use std::collections::HashMap;

    /// The output of this program
    struct Output(String);
    /// The given solution
    struct Solution(String);
    /// The adjustable form between the output and the given solution
    struct ToAdjust(String);

    impl Convert<String> for Output {
        fn convert(self) -> String {
            self.blur().adjust()
        }
    }
    impl Convert<String> for Solution {
        fn convert(self) -> String {
            self.trim().adjust()
        }
    }

    impl Output {
        /// Reduce information on the output to match the given solution
        fn blur(self) -> ToAdjust {
            let map: HashMap<_, _> = [
                // keyword
                ("classVarKind>", "keyword>"),
                ("subroutineKind>", "keyword>"),
                ("keywordConstant>", "keyword>"),
                // symbol
                ("op>", "symbol>"),
                ("unaryOp>", "symbol>"),
                // identifier
                ("className>", "identifier>"),
                ("type>", "identifier>"),
                ("subroutineType>", "identifier>"),
                ("subroutineName>", "identifier>"),
                ("varName>", "identifier>"),
            ]
            .into();

            let mut res = self.0;
            for (k, v) in map {
                res = res.replace(k, v)
            }

            ToAdjust(res)
        }
    }

    impl Solution {
        /// Remove semantic meaningless elements from the given solution
        fn trim(self) -> ToAdjust {
            let list = vec![
                "keyword> class",
                "keyword> var",
                "keyword> let",
                "keyword> do",
                "keyword> if",
                "keyword> else",
                "keyword> while",
                "keyword> return",
                "symbol> {",
                "symbol> }",
                "symbol> (",
                "symbol> )",
                "symbol> [",
                "symbol> ]",
                "symbol> .",
                "symbol> ,",
                "symbol> ;",
            ];

            let res = self
                .0
                .lines()
                .filter(|line| !list.iter().any(|item| line.contains(item)))
                .collect::<Vec<&str>>()
                .join("\n");

            ToAdjust(res)
        }
    }

    impl ToAdjust {
        /// Ignore nonsense differences between the output and the given solution
        fn adjust(self) -> String {
            let list = vec![
                // whether a type or a keyword
                "> int",
                "> char",
                "> boolean",
                "> void",
                // double meaning in the given solution
                "symbol> =",
            ];

            self.0
                .lines()
                .filter(|line| !list.iter().any(|item| line.contains(item)))
                .collect::<Vec<&str>>()
                .join("\n")
        }
    }

    impl From<String> for Output {
        fn from(s: String) -> Self {
            Self(s)
        }
    }
    impl From<String> for Solution {
        fn from(s: String) -> Self {
            Self(s)
        }
    }

    #[test]
    fn test() {
        crate::tests::test::<Output, Solution, String>(run, "")
    }
}
