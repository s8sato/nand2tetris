use crate::prelude::*;

#[derive(Parser)]
#[grammar = "jackT.pest"]
struct JackT;

pub fn run(io: IO) {
    let file = JackT::parse(Rule::file, &io.input)
        .expect("Unsuccessful parse")
        .next()
        .unwrap();

    let mut writer = io.writer;
    let mut stack = Stack::new();
    stack.push_t(Element::tag("tokens"), &mut writer);

    for token in file.into_inner() {
        match token.as_rule() {
            Rule::token => {
                stack.push_t(token.into(), &mut writer);
                stack.pop(&mut writer);
            }
            Rule::EOI => (),
            _ => unreachable!(),
        }
    }
    stack.pop(&mut writer);
    writer.flush().unwrap()
}

impl From<Pair<'_, Rule>> for Element {
    fn from(token: Pair<'_, Rule>) -> Self {
        let token_str = token.as_str();
        let map: HashMap<_, _> = [("&", "&amp;"), (">", "&gt;"), ("<", "&lt;")].into();
        match token.into_inner().next().unwrap().as_rule() {
            Rule::keyword => Element::new("keyword", token_str),
            Rule::symbol => Element::new("symbol", map.get(&token_str).unwrap_or(&token_str)),
            Rule::integer_constant => Element::new("integerConstant", token_str),
            Rule::string_constant => Element::new("stringConstant", token_str.trim_matches('"')),
            Rule::identifier => Element::new("identifier", token_str),
            _ => unreachable!(),
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
