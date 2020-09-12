use std::fs;
use std::io::{Write, BufWriter};
use std::path::{Path, PathBuf};
use std::error::Error;

mod parser;
mod encoder;
mod symbol_table;

use symbol_table::SymbolTable;

use std::time::Instant;

pub struct Config {
    pub in_file: Box<PathBuf>,
    pub out_file: Box<PathBuf>,
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, &'static str> {
        if args.len() != 2 {
            return Err("Specify exactly one argument")
        }
        let in_file = Box::new(Path::new(&args[1]).to_owned());
        let out_file = Box::new(in_file.with_extension("hack"));
        Ok(Config { in_file, out_file })
    }
}

#[derive(Debug, PartialEq)]
pub struct Label(Symbol);

#[derive(Debug, PartialEq)]
pub enum Command {
    V(Symbol),
    A(Addr),
    C { dest: Option<Dest>,
        comp: Comp,
        jump: Option<Jump>,
    },
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Symbol(String);

#[derive(Debug, PartialEq, Clone)]
pub struct Addr(u16);

#[derive(Debug, PartialEq)]
pub struct Dest(String);

#[derive(Debug, PartialEq)]
pub struct Comp(String);

#[derive(Debug, PartialEq)]
pub struct Jump(String);

pub fn run(config: &Config) -> Result<(), Box<dyn Error>> {

    let instant = Instant::now();
    let input = fs::read_to_string(&*config.in_file)?;
    println!("{:<9} {:?}", "file_read", instant.elapsed());

    let instant = Instant::now();
    let mut lines = extract(input);
    println!("{:<9} {:?}", "extract", instant.elapsed());

    // 1st pass
    let instant = Instant::now();
    let mut labeler = Labeler::new();
    for line in lines.iter_mut() {
        labeler.label(line)?;
    }
    lines.retain(|l| l.index != 0);
    println!("{:<9} {:?}", "1st_pass", instant.elapsed());

    let instant = Instant::now();
    let mut cmds = Vec::new();
    for line in lines.iter() {
        let cmd = line.parse()?;
        cmds.push(cmd);
    }
    println!("{:<9} {:?}", "parse2cmd", instant.elapsed());

    let file = fs::File::create(&*config.out_file)?;
    let mut writer = BufWriter::new(file);
    let mut solver = Solver::new(labeler.table);

    let instant = Instant::now();
    for cmd in cmds.iter_mut() { 
        solver.solve(cmd);
        writeln!(writer, "{:0>1$b}", cmd.encode(), 16)?;
    }
    writer.flush()?;
    solver.check()?;
    println!("{:<9} {:?}", "enc&write", instant.elapsed());

    Ok(())
}

#[derive(Debug, PartialEq)]
struct Line {
    index: usize,
    body: String,
}

impl Line {
    fn parse(&self) -> Result<Command, Box<dyn Error>>{
        match self.body.parse::<Command>() {
            Err(e) => {
                eprint!("in line {}, ", self.index);
                return Err(e)
            },
            Ok(cmd) => {
                Ok(cmd)
            },
        }
    }
}

fn extract(mut input: String) -> Vec<Line> {
    input.retain(|c| c != ' ');
    input.lines().enumerate()
        .map(|(i, x)| Line { index: i+1, body: x.split("//").next().unwrap().to_string() })
        .filter(|l| !l.body.is_empty())
        .collect()
}

impl Addr {
    pub fn new(i: u16) -> Result<Self, ()> {
        let entry = Addr(i);
        entry.check()?;
        Ok(entry)
    }
    fn check(&self) -> Result<(), ()> {
        let Addr(i) = *self;
        if i < 1 << 15 {
            return Ok(())
        }
        eprintln!("Out of address range: {}", i);
        Err(())
    }
    fn inc(&mut self) {
        let Addr(i) = self;
        *i += 1;
    }
}

#[derive(Debug, PartialEq)]
struct Labeler {
    table: SymbolTable,
    next_rom_addr: Addr,
}

impl Labeler {
    fn new() -> Self {
        Self {
            table: symbol_table::new(),
            next_rom_addr: Addr(0),
        }
    }
    fn label(&mut self, line: &mut Line) -> Result<(), Box<dyn Error>> {
        match line.body.parse::<Label>() {
            Ok(Label(sym)) => {
                if let Some(_) = self.table.insert(sym.clone(), self.next_rom_addr.clone()) {
                    let Symbol(s) = sym;
                    return Err(format!("Duplicate definition of '{}'", s).into())
                }
                line.index = 0;
            },
            Err(_) => {
                if let Err(_) = self.next_rom_addr.check() {
                    return Err("Program too large".into())
                }
                self.next_rom_addr.inc();
            },
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
struct Solver {
    table: SymbolTable,
    next_ram_addr: Addr,
}

impl Solver {
    fn new(table: SymbolTable) -> Self {
        Self {
            table: table,
            next_ram_addr: Addr(0x0010),
        }
    }
    fn check(&self) -> Result<(), Box<dyn Error>> {
        let Addr(i) = self.next_ram_addr;
        if  1 << 14 < i {
            return Err("Too many variables".into())
        }
        Ok(())
    }
    fn solve(&mut self, cmd: &mut Command) {
        if let Command::V(sym) = cmd {
            let solution = match self.table.get(sym) {
                Some(addr) => addr.clone(),
                None => {
                    let solution = self.next_ram_addr.clone();
                    self.table.insert(sym.clone(), solution.clone());
                    self.next_ram_addr.inc();
                    solution
                },
            };
            *cmd = Command::A(solution);
        };
    }
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
    #[test]
    fn line_parse_err() {
        let line = Line { index: 10, body: "// a comment survived the extraction".to_string() };
        let trial = line.parse();
        assert!(trial.is_err());
        eprintln!("{:?}", trial);
    }
    #[test]
    fn fst_pass_err_duplicate_label() {
        let mut labeler = Labeler::new();
        let mut line = Line { index: 10, body: "(SP)".to_string() };
        let trial = labeler.label(&mut line);
        assert!(trial.is_err());
        eprintln!("{:?}", trial);
    }
    #[test]
    fn fst_pass_err_program_too_large() {
        let mut labeler = Labeler::new();
        let mut lines = (1..=0x8000).map(|i| {
                Line { index: i, body: "@0".to_string() }
            }).collect::<Vec<Line>>();
        for line in lines.iter_mut() {
            labeler.label(line).unwrap();
        }
        let mut line = Line { index: 0x8001, body: "@0".to_string() };
        let trial = labeler.label(&mut line);
        assert!(trial.is_err());
        eprintln!("{:?}", trial);
    }
    #[test]
    fn fst_pass_ok() {
        let mut labeler = Labeler::new();
        let mut lines = vec![
            Line { index: 01, body: "@32767"  .to_string() },
            Line { index: 02, body: "D=A"     .to_string() },
            Line { index: 03, body: "(LOOP)"  .to_string() },
            Line { index: 04, body: "D=D-1"   .to_string() },
            Line { index: 05, body: "@END"    .to_string() },
            Line { index: 06, body: "D;JEQ"   .to_string() },
            Line { index: 07, body: "@LOOP"   .to_string() },
            Line { index: 08, body: "0;JMP"   .to_string() },
            Line { index: 09, body: "(END)"   .to_string() },
            Line { index: 10, body: "@END"    .to_string() },
            Line { index: 11, body: "0;JMP"   .to_string() },
        ];
        for line in lines.iter_mut() {
            labeler.label(line).unwrap()
        }
        lines.retain(|l| l.index != 0);
        assert_eq!(lines.len(), 9);
        assert_eq!(labeler.table.get(&Symbol(String::from("LOOP"))), Some(&Addr(2)));
        assert_eq!(labeler.table.get(&Symbol(String::from("END" ))), Some(&Addr(7)));
    }
    #[test]
    fn solve_cmd_v() {
        let mut solver = Solver::new(symbol_table::new());
        let mut cmd = Command::V(Symbol(String::from("variable")));
        solver.solve(&mut cmd);
        let expect = Command::A(Addr(16));
        assert_eq!(cmd, expect);
    }
    #[test]
    fn solve_label() {
        let mut lines = vec![
            Line { index: 01, body: "@32767"  .to_string() },
            Line { index: 02, body: "D=A"     .to_string() },
            Line { index: 03, body: "(LOOP)"  .to_string() },
            Line { index: 04, body: "D=D-1"   .to_string() },
            Line { index: 05, body: "@END"    .to_string() },
            Line { index: 06, body: "D;JEQ"   .to_string() },
            Line { index: 07, body: "@LOOP"   .to_string() },
            Line { index: 08, body: "0;JMP"   .to_string() },
            Line { index: 09, body: "(END)"   .to_string() },
            Line { index: 10, body: "@END"    .to_string() },
            Line { index: 11, body: "0;JMP"   .to_string() },
        ];
        let mut labeler = Labeler::new();
        for line in lines.iter_mut() {
            labeler.label(line).unwrap()
        }
        let mut solver = Solver::new(labeler.table);
        let mut cmd = Command::V(Symbol("LOOP".to_string()));
        solver.solve(&mut cmd);
        assert_eq!(cmd, Command::A(Addr(2)));
        let mut cmd = Command::V(Symbol("END" .to_string()));
        solver.solve(&mut cmd);
        assert_eq!(cmd, Command::A(Addr(7)));
    }
    #[test]
    fn solve_err_too_many_variables() {
        let mut solver = Solver::new(symbol_table::new());
        for i in 0x0010..=0x3FFF {
            let mut cmd = Command::V(Symbol(format!("var{}", i)));
            solver.solve(&mut cmd);
        }
        let trial = solver.check();
        assert!(trial.is_ok());
        let mut cmd = Command::V(Symbol(format!("var{}", 0x4000)));
        solver.solve(&mut cmd);
        let trial = solver.check();
        assert!(trial.is_err());
        eprintln!("{:?}", trial);
    }
}
