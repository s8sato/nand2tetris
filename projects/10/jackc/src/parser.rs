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
            match p.as_rule() {
                Rule::EOI => break,
                _ => self.push((&p).into(), writer),
            }
            if !is_terminal(&p) {
                self.scan(p, writer)
            }
            self.pop(writer);
        }
    }
}

impl From<&Pair<'_, Rule>> for Element {
    fn from(pair: &Pair<Rule>) -> Self {
        use Rule::*;
        match pair.as_rule() {
            string_constant => Element::new(pair, |s| s.trim_matches('"')),
            op => Element::new(pair, |s| symbol_map().get(s).unwrap_or(&s)),
            _ if is_terminal(pair) => Element::new(pair, identity),
            _ => Element::tag(pair),
        }
    }
}

fn is_terminal(pair: &Pair<Rule>) -> bool {
    use Rule::*;
    match pair.as_rule() {
        integer_constant | class_var_kind | type_ | subroutine_kind | subroutine_type
        | class_name | subroutine_name | var_name | unary_op | keyword_constant
        | string_constant | op => true,
        _ => false,
    }
}

mod tests {
    use super::*;

    #[test]
    fn test() {
        let dirs = vec!["../ArrayTest", "../ExpressionLessSquare", "../Square"]
            .into_iter()
            .map(|s| PathBuf::from(s));

        for dir in dirs {
            let in_files = dir
                .read_dir()
                .expect("Failed to read directory")
                .map(|entry| entry.unwrap().path())
                .filter(|path| path.extension().map(|ext| ext.to_str()) == Some(Some("jack")))
                .collect::<Vec<PathBuf>>();

            for in_file in in_files {
                dbg!(&in_file);
                let input = fs::read_to_string(&in_file).unwrap();
                let out_file = "_test.out.xml";
                let writer = BufWriter::new(fs::File::create(out_file).unwrap());
                run(IO { input, writer });
                let output = fs::read_to_string(&out_file).unwrap();

                let solution_file = in_file.with_extension("xml");
                let solution = fs::read_to_string(&solution_file).unwrap();

                // TODO assert_eq!(blur(output), trim(solution));
                fs::remove_file(out_file).unwrap();
            }
        }
    }
}
