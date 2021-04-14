use std::fs;
use std::io::{Write, BufWriter};
use std::path::{Path, PathBuf};
use std::error::Error;

mod parser;
mod encoder;

pub struct Config {
    pub in_files: Vec<PathBuf>,
    pub out_file: PathBuf,
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, &str> {
        if args.len() != 2 {
            return Err("Specify exactly one argument")
        }
        let path = Path::new(&args[1]);
        let in_files = if path.is_file() {
            vec![path.to_owned()]
        } else {
            path.read_dir().expect("Failed to read directory.")
            .map(|entry| entry.unwrap().path())
            .filter(|path| path.extension().map(|ext| ext.to_str()) == Some(Some("vm")))
            .map(|path| path.to_path_buf())
            .collect()
        };
        let out_file = if path.is_file() {
            path.with_extension("asm")
        } else {
            path.join(path.file_name().unwrap().to_str().unwrap()).with_extension("asm")
        };
        Ok(Config { in_files, out_file })
    }
}

pub fn run(config: &Config) -> Result<(), Box<dyn Error>> {
    let mut writer = BufWriter::new(
        fs::File::create(&*config.out_file)?
    );
    let mut encoder = encoder::Encoder::default();

    writeln!(writer, "{}", encoder.bootstrap())?;

    for in_file in config.in_files.iter() {
        encoder.file_stem = in_file.as_path().file_stem().unwrap().to_str().unwrap().to_string();
        encoder.function = String::new();

        for line in fs::read_to_string(&*in_file)?.lines()
        // remove comments
        .map(|x| x.split("//").next().unwrap().trim().to_string())
        // remove empty lines
        .filter(|x| !x.is_empty()) {
            writeln!(writer, "{}", encoder.exec(line.parse::<Command>()?))?
        }
    }
    writer.flush()?;
    Ok(())
}

pub enum Command {
    Arithmetic(Arithmetic),
    Push { segment: Segment, idx: i32 },
    Pop { segment: Segment, idx: i32 },
    Label(Symbol),
    Goto(Symbol),
    IfGoto(Symbol),
    Function { f: Symbol, n_locals: i32 },
    Call { f: Symbol, n_args: i32 },
    Return,
}

pub enum Arithmetic {
    Add,
    Sub,
    Neg,
    Eq,
    Gt,
    Lt,
    And,
    Or,
    Not,
}

pub enum Segment {
    Constant,
    Local,
    Argument,
    This,
    That,
    Pointer,
    Temp,
    Static,
}

pub type Symbol = String;
