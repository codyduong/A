pub(crate) mod ast;
mod lex;
mod parse;
mod util;

use clap::command;
use clap::Parser;
use std::fs;
use std::fs::File;
use std::fs::OpenOptions;
use std::io;
use std::io::Write;
use std::path::Path;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
  infile: String,
  #[arg(short, long)]
  tokens_file: Option<String>,
  #[arg(short, long, default_value_t = false)]
  parse: bool,
  #[arg(short, long)]
  unparse: Option<String>,
}

fn main() -> io::Result<()> {
  let args = Args::parse();

  let parse = args.parse;
  let unparse = args.unparse;

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
    if !parse && unparse.is_none() {
      println!("{}", tokens_result);
    }
  }

  eprintln!("{}", errors);

  if parse || unparse.is_some() {
    let mut parser = crate::parse::Parser::new(&tokens);
    match parser.parse_program() {
      Ok(program) => {
        // lol! when i don't use narrowing correctly...
        if let Some(unparse_path) = unparse {
          let path = Path::new(&unparse_path);
          let mut file = File::create(&path)?;
          writeln!(file, "{}", program)?;
        }
      }
      Err(_) => {
        eprintln!("syntax error\nParse failed")
      }
    }
  }

  Ok(())
}
