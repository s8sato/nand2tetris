use crate::prelude::*;

#[derive(Parser)]
#[grammar = "jackT.pest"]
struct JackT;

pub fn run(io: IO) {
    let file = JackT::parse(Rule::file, &io.input)
        .expect("Failed to parse input")
        .next()
        .unwrap();

    let mut writer = io.writer;
    let mut stack = Stack::new();

    for pair in file.into_inner() {
        match pair.as_rule() {
            Rule::tokens => {
                stack.push(Element::tag(&pair), &mut writer);
                for token in pair.into_inner() {
                    let token = token.into_inner().next().unwrap();
                    stack.push(token.into(), &mut writer);
                    stack.pop(&mut writer);
                }
                stack.pop(&mut writer);
            }
            Rule::EOI => (),
            _ => unreachable!(),
        }
    }

    writer.flush().unwrap()
}

impl From<Pair<'_, Rule>> for Element {
    fn from(token: Pair<'_, Rule>) -> Self {
        match token.as_rule() {
            Rule::symbol => Element::new(&token, |s| symbol_map().get(s).unwrap_or(&s)),
            Rule::string_constant => Element::new(&token, |s| s.trim_matches('"')),
            _ => Element::new(&token, identity),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;

    impl Convert<String> for String {
        /// Ignore indentation
        fn convert(self) -> Self {
            self.lines()
                .map(|line| line.strip_prefix("  ").unwrap_or(line))
                .collect::<Vec<&str>>()
                .join("\n")
        }
    }

    #[test]
    fn test() {
        crate::tests::test::<String, String, String>(run, "T")
    }
}
