use std::fs;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::error::Error;

mod parser;
mod encoder;

pub struct Config {
    pub in_file: Box<PathBuf>,
    pub out_file: Box<PathBuf>,
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, &'static str> {
        if args.len() != 2 {
            return Err("Specify exactly one argument.")
        }
        let in_file = Box::new(Path::new(&args[1]).to_owned());
        let out_file = Box::new(in_file.with_extension("hack"));
        // let out_file = Box::new(Path::new("out.hack").to_owned()); // TODO Remove this line
        Ok(Config { in_file, out_file })
    }
}

#[derive(Debug, PartialEq)]
pub enum Command {
    A(Addr),
    C { dest: Option<Dest>,
        comp: Comp,
        jump: Option<Jump>,
    }
}

#[derive(Debug, PartialEq)]
pub struct Addr(u16);

#[derive(Debug, PartialEq)]
pub struct Dest(String);

#[derive(Debug, PartialEq)]
pub struct Comp(String);

#[derive(Debug, PartialEq)]
pub struct Jump(String);

pub fn run(config: &Config) -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string(&*config.in_file)?;
    let input = extract(input);
    
    // Prepare empty file
    OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(&*config.out_file)?;

    let mut out_handle = OpenOptions::new()
        .append(true)
        .open(&*config.out_file)?;

    for line in input.lines() {
        let line = line.parse::<Command>()?.encode();
        writeln!(out_handle, "{:0>1$b}", line, 16)?;
    }

    Ok(())
}

fn extract(mut input: String) -> String {
    input.retain(|c| c != ' ');
    input.lines()
        .map(|x| x.split("//").next().unwrap())
        .filter(|x| !x.is_empty())
        .fold(String::new(), |acc, x| acc + x + "\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_test() {
        let before = "\
command // comment
// comment
 c o m m a n d / / c o m m e n t 
 / / c o m m e n t 
        ".to_string();
        let after = "command\ncommand\n".to_string();
        assert_eq!(extract(before), after);
    }
}
