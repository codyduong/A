mod lex;
mod util;
mod parse;
pub(crate) mod ast;

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
  #[arg(short, long, default_value_t=false)]
  parse: bool,
}

fn main() -> io::Result<()> {
  let args = Args::parse();

  let parse = args.parse;
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
    if !parse {
      println!("{}", tokens_result);
    }
  }

  eprintln!("{}", errors);

  if parse {
    let mut parser = crate::parse::Parser::new(&tokens);
    match parser.parse_program() {
      Ok(_program) => {
        // todo next step
      }
      Err(_) => {
        eprintln!("syntax error\nParse failed")
      }
    }
  }

  Ok(())
}
