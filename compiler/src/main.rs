mod lex;
mod util;

use clap::command;
use clap::Parser;
use std::fs;
use std::fs::OpenOptions;
use std::io;
use std::io::Write;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
  infile: String,
  #[arg(short, long)]
  tokens_file: Option<String>,
}

fn main() -> io::Result<()> {
  let args = Args::parse();

  let content = fs::read_to_string(&args.infile)?;

  let (tokens, errors) = lex::lex().input(&content).call();

  let tokens_result = tokens.to_string();
  if let Some(tokens_file) = args.tokens_file {
    let mut file = OpenOptions::new()
      .write(true)
      .create(true)
      .truncate(true)
      .open(&*tokens_file)?;

    file.write(tokens_result.as_bytes())?;
  } else {
    println!("{}", tokens_result);
  }
  eprintln!("{}", errors);

  Ok(())
}
