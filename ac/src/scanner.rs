use crate::util::Metadata;
use crate::util::Token;
use crate::util::TokenWithMetadata;
use crate::util::KEYWORDS;
use bon::builder;

#[builder]
fn parse_string_literal(
  chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
  line: &mut usize,
  column: &mut usize,
) -> Result<TokenWithMetadata, ScannerError> {
  let start = Metadata::new(*line, *column);
  let mut value = String::new();
  chars.next(); // consume starting quote
  *column += 1;

  while let Some(&c) = chars.peek() {
    let end = Metadata::new(*line, *column);
    match c {
      '"' => {
        chars.next(); // consume ending quote
        *column += 1;
        return Ok(TokenWithMetadata {
          token: Token::StringLiteral(value),
          start,
          end,
        });
      }
      '\n' => {
        return Err(ScannerError::UnterminatedString { start, end });
      }
      // we only support \\, \n, \t, \"
      '\\' => {
        if let Some(next_c) = chars.nth(1) {
          *column += 1;
          match next_c {
            '\\' => value.push('\\'),
            'n' => value.push('\n'),
            't' => value.push('\t'),
            '"' => value.push('"'),
            _ => {
              return Err(ScannerError::IllegalEscape {
                char: next_c,
                start,
                end: Metadata::new(*line, *column),
              });
            }
          }
        } else {
          return Err(ScannerError::UnterminatedString { start, end });
        }
        *column += 1;
      }
      _ => {
        if !c.is_ascii() {
          return Err(ScannerError::IllegalChar { char: c, start, end });
        }
        value.push(c);
        chars.next();
        *column += 1;
      }
    }
  }

  Err(ScannerError::UnterminatedString {
    start,
    end: Metadata::new(*line, *column),
  })
}

#[builder]
fn parse_keyword_or_identifier(
  chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
  line: &mut usize,
  column: &mut usize,
) -> Result<TokenWithMetadata, ScannerError> {
  let start = Metadata::new(*line, *column);
  let mut value = String::new();

  while let Some(&c) = chars.peek() {
    if c.is_alphanumeric() {
      value.push(c);
      chars.next();
      *column += 1;
    } else {
      match c {
        '_' => {
          value.push(c);
          chars.next();
          *column += 1;
        }
        ' ' | '\t' | '\n' => break,
        _ => {
          return Err(ScannerError::IllegalChar {
            char: c,
            start,
            end: Metadata::new(*line, *column),
          })
        }
      };
    }
  }

  let end = Metadata::new(*line, *column);

  for (keyword, token) in KEYWORDS.iter() {
    if *keyword == value {
      return Ok(TokenWithMetadata {
        token: token.clone(),
        start,
        end,
      });
    }
  }

  Ok(TokenWithMetadata {
    token: Token::Identifier(value),
    start,
    end,
  })
}

#[builder]
pub(crate) fn scanner(input: &str) -> (Vec<TokenWithMetadata>, Vec<ScannerError>) {
  let mut tokens = Vec::new();
  let mut errors = Vec::new();
  let mut line = 1;
  let mut column = 1;
  let mut chars = input.chars().peekable();
  let start = Metadata::new(line, column);

  // @codyduong TODO we can parallelize this since there are no multiline tokens
  while let Some(&c) = chars.peek() {
    let end = Metadata::new(line, column);
    match c {
      ' ' | '\t' => {
        column += 1;
        chars.next();
      }
      '\n' => {
        line += 1;
        column = 1;
        chars.next();
      }
      '"' => {
        match parse_string_literal()
          .chars(&mut chars)
          .line(&mut line)
          .column(&mut column)
          .call()
        {
          Ok(token) => tokens.push(token),
          Err(err) => errors.push(err),
        };
      }
      // Todo INTS
      // Todo Symbols
      // Todo comments skip whole lines
      _ => {
        let start = start.clone();
        if c.is_ascii() {
          match parse_keyword_or_identifier()
            .chars(&mut chars)
            .line(&mut line)
            .column(&mut column)
            .call()
          {
            Ok(token) => tokens.push(token),
            Err(err) => errors.push(err),
          }
        } else {
          errors.push(ScannerError::IllegalChar { char: c, start, end });
          chars.next();
          column += 1;
        }
      }
    };
  }

  (tokens, errors)
}

#[derive(Debug, PartialEq)]
enum ScannerError {
  IllegalChar { char: char, start: Metadata, end: Metadata },
  UnterminatedString { start: Metadata, end: Metadata },
  IllegalEscape { char: char, start: Metadata, end: Metadata },
  IntegerOverflow { start: Metadata, end: Metadata },
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn test_keywords() {
    for (keyword, expected_token_kind) in KEYWORDS.iter() {
      let input = *keyword;
      let results = scanner().input(input).call();
      let tokens = results.0;
      let errors = results.1;
      assert_eq!(tokens.len(), 1);
      assert_eq!(errors.len(), 0);
      assert_eq!(tokens[0].token, *expected_token_kind, "Failed on keyword '{}'", input);
    }
  }

  #[test]
  fn test_identifiers() {
    let input = "\
baz foo_bar _foo_bar                                                        \n\
foo123 foo456 foo7890                                                       \n\
";
    let results = scanner().input(&input).call();
    let tokens = results.0;
    let errors = results.1;
    assert_eq!(tokens.len(), 6);
    assert_eq!(errors.len(), 0);
    #[rustfmt::skip]
    assert_eq!(*tokens.get(0).unwrap(), TokenWithMetadata { token: Token::Identifier("baz".to_string()), start: Metadata::new(1, 1), end: Metadata::new(1, 4) } );
    #[rustfmt::skip]
    assert_eq!(*tokens.get(1).unwrap(), TokenWithMetadata { token: Token::Identifier("foo_bar".to_string()), start: Metadata::new(1, 5), end: Metadata::new(1, 12) } );
    #[rustfmt::skip]
    assert_eq!(*tokens.get(2).unwrap(), TokenWithMetadata { token: Token::Identifier("_foo_bar".to_string()), start: Metadata::new(1, 13), end: Metadata::new(1, 21) } );
    #[rustfmt::skip]
    assert_eq!(*tokens.get(3).unwrap(), TokenWithMetadata { token: Token::Identifier("foo123".to_string()), start: Metadata::new(2, 1), end: Metadata::new(2, 7) } );
    #[rustfmt::skip]
    assert_eq!(*tokens.get(4).unwrap(), TokenWithMetadata { token: Token::Identifier("foo456".to_string()), start: Metadata::new(2, 8), end: Metadata::new(2, 14) } );
    #[rustfmt::skip]
    assert_eq!(*tokens.get(5).unwrap(), TokenWithMetadata { token: Token::Identifier("foo7890".to_string()), start: Metadata::new(2, 15), end: Metadata::new(2, 22) } );
  }

  #[test]
  fn test_string_literals() {
    let input = "\
\"\"                                                                        \n\
\"&!88\"                                                                    \n\
\"use \\n to denote a newline character\"                                   \n\
\"use \\\" for a quote and \\\\ for a backslash\"                           \n\
\"  \\\\ \\n\\t\\\"  \"      \"foo\"                                        \n\
";
    let results = scanner().input(&input).call();
    let tokens = results.0;
    let errors = results.1;

    println!("{:#?}", tokens);

    assert_eq!(tokens.len(), 6);
    assert_eq!(errors.len(), 0);
    #[rustfmt::skip]
    assert_eq!(*tokens.get(0).unwrap(), TokenWithMetadata { token: Token::StringLiteral("".to_string()), start: Metadata::new(1, 1), end: Metadata::new(1, 2) } );
    #[rustfmt::skip]
    assert_eq!(*tokens.get(1).unwrap(), TokenWithMetadata { token: Token::StringLiteral("&!88".to_string()), start: Metadata::new(2, 1), end: Metadata::new(2, 6) } );
    #[rustfmt::skip]
    assert_eq!(*tokens.get(2).unwrap(), TokenWithMetadata { token: Token::StringLiteral("use \n to denote a newline character".to_string()), start: Metadata::new(3, 1), end: Metadata::new(3, 38) } );
    #[rustfmt::skip]
    assert_eq!(*tokens.get(3).unwrap(), TokenWithMetadata { token: Token::StringLiteral("use \" for a quote and \\ for a backslash".to_string()), start: Metadata::new(4, 1), end: Metadata::new(4, 43) } );
    #[rustfmt::skip]
    assert_eq!(*tokens.get(4).unwrap(), TokenWithMetadata { token: Token::StringLiteral("  \\ \n\t\"  ".to_string()), start: Metadata::new(5, 1), end: Metadata::new(5, 15) } );
    #[rustfmt::skip]
    assert_eq!(*tokens.get(5).unwrap(), TokenWithMetadata { token: Token::StringLiteral("foo".to_string()), start: Metadata::new(5, 22), end: Metadata::new(5, 26) } );
  }

  // #[test]
  // fn fail_string_literal_unterminated() {
  //   assert_eq!(Ok(()), scanner().call());
  // }

  // #[test]
  // fn fail_string_literal_bad_escape() {
  //   assert_eq!(Ok(()), scanner().call());
  // }

  // #[test]
  // fn fail_integer_overflow() {
  //   assert_eq!(Ok(()), scanner().call());
  // }
}
