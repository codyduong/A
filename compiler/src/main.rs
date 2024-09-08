mod scanner;
mod util;

use clap::command;
use clap::Parser;
use std::fs;
use std::io;

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

  let (tokens, errors) = scanner::scanner().input(&content).call();

  let tokens_result = tokens.to_string();
  if let Some(tokens_file) = args.tokens_file {
    fs::write::<&str, std::string::String>(&*tokens_file, tokens_result)?;
  } else {
    println!("{}", tokens_result);
  }
  eprintln!("{}", errors);

  Ok(())
}
