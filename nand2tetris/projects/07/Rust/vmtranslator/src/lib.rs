// use std::fs;
// use std::io::{Write, BufWriter};
use std::path::{Path, PathBuf};
use std::error::Error;

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

    Ok(())
}
