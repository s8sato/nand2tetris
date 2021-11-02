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
    stack.push_t(
        Element {
            tag: "tokens".to_owned(),
            body: None,
        },
        &mut writer,
    );

    for pair in file.into_inner() {
        match pair.as_rule() {
            Rule::token => {
                stack.push_t(pair.into(), &mut writer);
                stack.pop(&mut writer);
            }
            Rule::EOI => (),
            _ => unreachable!(),
        }
    }
    stack.pop(&mut writer);
    writer.flush().unwrap()
}

impl Stack {
    fn push_t(&mut self, element: Element, writer: &mut Writer) {
        // self.indent(writer);
        write!(writer, "<{}>", element.tag).unwrap();
        self.delimit(element.body.is_some(), writer);
        self.0.push(element)
    }
}

impl From<Pair<'_, Rule>> for Element {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let token = pair.into_inner().next().unwrap();
        match token.as_rule() {
            Rule::string_constant => Element::new(&token, |s| s.trim_matches('"')),
            Rule::symbol => Element::new(&token, |s| symbol_map().get(s).unwrap_or(&s)),
            _ => Element::new(&token, identity),
        }
    }
}

#[cfg(test)]
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

                let mut file_stem = in_file.file_stem().unwrap_or_default().to_owned();
                file_stem.push("T");
                let solution_file = in_file.with_file_name(file_stem).with_extension("xml");
                let solution = fs::read_to_string(&solution_file).unwrap();

                assert_eq!(output, solution);
                fs::remove_file(out_file).unwrap();
            }
        }
    }
}
