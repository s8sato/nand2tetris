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
        let out_file = Box::new(Path::new("out.hack").to_owned()); // TODO Remove this line
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

    for line in input.iter() { 
        match line.body.parse::<Command>() {
            Err(e) => {
                eprint!("in line {}, ", line.index);
                return Err(e)
            }
            Ok(cmd) => writeln!(out_handle, "{:0>1$b}", cmd.encode(), 16)?
        }
    }

    Ok(())
}

fn extract(mut input: String) -> Vec<Line> {
    input.retain(|c| c != ' ');
    input.lines().enumerate()
        .map(|(i, x)| Line { index: i+1, body: x.split("//").next().unwrap().to_string() })
        .filter(|l| !l.body.is_empty())
        .collect()
}

#[derive(Debug, PartialEq)]
struct Line {
    index: usize,
    body: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extraction() {
        let trial = extract(String::from("\
command // comment
// comment
 c o m m a n d / / c o m m e n t 
 / / c o m m e n t 
"));
        let expect = vec![
            Line { index: 1, body: "command".to_string() },
            Line { index: 3, body: "command".to_string() },
        ];
        assert_eq!(trial, expect);
    }
}
