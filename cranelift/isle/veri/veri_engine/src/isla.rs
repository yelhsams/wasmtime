use anyhow::Context as _;
use clap::Parser;
use cranelift_codegen::print_errors::pretty_error;
use cranelift_codegen::Context;
use cranelift_reader::{parse_sets_and_triple, parse_test, ParseOptions};
use std::fs::File;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

#[derive(Parser)]
struct Options {
    /// Configure Cranelift settings.
    #[clap(long = "set")]
    settings: Vec<String>,

    /// Specify the Cranelift target.
    #[clap(long = "target")]
    target: String,

    /// Specify an input file to be used. Use '-' for stdin.
    file: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let options = Options::parse();

    // Parse input CLIF test file.
    let buffer = read_to_string(&options.file)?;
    let test_file = parse_test(&buffer, ParseOptions::default())
        .with_context(|| format!("failed to parse {}", options.file.to_string_lossy()))?;

    // Determine ISA settings.
    let parsed = parse_sets_and_triple(&options.settings, &options.target)?;
    let fisa = parsed.as_fisa();
    let isa = fisa.isa.or(test_file.isa_spec.unique_isa());

    let isa = match isa {
        None => anyhow::bail!("compilation requires a target isa"),
        Some(isa) => isa,
    };

    // Compile functions.
    for (func, _) in test_file.functions {
        let mut context = Context::for_function(func);
        let mut mem = vec![];
        let _compiled_code = context
            .compile_and_emit(isa, &mut mem, &mut Default::default())
            .map_err(|err| anyhow::anyhow!("{}", pretty_error(&err.func, err.inner)))?;
        println!("bytes: {:?}", mem);
    }

    Ok(())
}

/// Read an entire file into a string.
fn read_to_string<P: AsRef<Path>>(path: P) -> anyhow::Result<String> {
    let mut buffer = String::new();
    let path = path.as_ref();
    if path == Path::new("-") {
        let stdin = io::stdin();
        let mut stdin = stdin.lock();
        stdin
            .read_to_string(&mut buffer)
            .context("failed to read stdin to string")?;
    } else {
        let mut file = File::open(path)?;
        file.read_to_string(&mut buffer)
            .with_context(|| format!("failed to read {} to string", path.display()))?;
    }
    Ok(buffer)
}
