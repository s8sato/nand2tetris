use std::fs;
use std::io::{Write, BufWriter};
use std::path::{Path, PathBuf};
use std::error::Error;

mod parser;
mod encoder;

#[derive(Debug)]
pub struct Config {
    pub in_files: Vec<PathBuf>,
    pub out_file: PathBuf,
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, &'static str> {
        if args.len() != 2 {
            return Err("Specify exactly one argument")
        }
        let path = Path::new(&args[1]);
        let in_files = if path.is_file() {
            vec![path.to_owned()]
        } else {
            path.read_dir().expect("Failed to read directory.")
            .map(|entry| entry.unwrap().path().to_path_buf()).collect()
        };
        let out_file = path.with_extension("asm");
        Ok(Config { in_files, out_file })
    }
}

pub fn run(config: &Config) -> Result<(), Box<dyn Error>> {
    dbg!(&config);
    let mut writer = BufWriter::new(
        fs::File::create(&*config.out_file)?
    );
    for in_file in config.in_files.iter() {
        let input = Input { input: fs::read_to_string(&*in_file)? };
        let output = input.extract().parse::<Command>()?.encode();
        write!(writer, "{}", output)?;
    }
    writer.flush()?;

    Ok(())
}

struct Input {
    input: String,
}

impl Input {
    fn extract(self) -> String {
        self.input
    }
}

pub struct Command {
    pub raw: String,
}
