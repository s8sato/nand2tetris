use clap::Parser;
use std::fs;
use std::io::BufWriter;

/// Intermediate program that converts `.jack` into `.xml` containing metadata for identifiers
#[derive(clap::Parser)]
#[clap(version = env!("CARGO_PKG_VERSION"), author = env!("CARGO_PKG_AUTHORS"))]
struct Opts {
    /// Input `.jack` file
    in_file: std::path::PathBuf,
}

fn main() {
    let opts = Opts::parse();
    jackc::IO::from(opts).run()
}

impl From<Opts> for jackc::IO {
    fn from(opts: Opts) -> Self {
        let input = fs::read_to_string(&opts.in_file).expect("Failed to read file");
        let out_file = opts.in_file.with_extension("xml");
        let writer = BufWriter::new(fs::File::create(out_file).unwrap());

        Self { input, writer }
    }
}
