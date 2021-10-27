use clap::Parser;
use jackc::{parser, prelude::*, tokenizer};

/// Intermediate program which parse or tokenize `.jack` to `.xml`
#[derive(clap::Parser)]
#[clap(version = env!("CARGO_PKG_VERSION"), author = env!("CARGO_PKG_AUTHORS"))]
struct Opts {
    /// Tokenize instead of parse
    #[clap(short, long)]
    tokenize: bool,
    /// Input `.jack` file
    in_file: PathBuf,
}

fn main() {
    let opts = Opts::parse();
    if opts.tokenize {
        return tokenizer::run(opts.into());
    }
    parser::run(opts.into())
}

impl From<Opts> for IO {
    fn from(opts: Opts) -> Self {
        let input = fs::read_to_string(&opts.in_file).expect("Failed to read file");

        let mut file_stem = opts.in_file.file_stem().unwrap_or_default().to_owned();
        if opts.tokenize {
            file_stem.push("T");
        }
        let out_file = opts
            .in_file
            .with_file_name(file_stem)
            .with_extension("out.xml");
        let writer = BufWriter::new(fs::File::create(out_file).unwrap());

        Self { input, writer }
    }
}
