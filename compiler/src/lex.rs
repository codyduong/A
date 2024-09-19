use crate::util::Metadata;
use crate::util::Token;
use crate::util::TokenWithMetadata;
use crate::util::VecWrapper;
use crate::util::KEYWORDS;
use bon::builder;

/**
 * A postscript
 * 
 * - This uses just straight up char matching, no lex-generator
 *   -> rust has a crate for this which is https://crates.io/crates/pest
 *   -> a previous students impl uses this https://github.com/brasswood/rust-cmmc
 * - In retrospect probably should've looked up this before writing all this
 * - The code here is awfully data oriented, while a struct may have been better
 *   - passing mut references for col and usize is an undesireable side-effect IMHO, but w/e
 * 
 */

#[builder]
fn parse_string_literal(
  chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
  line: &mut usize,
  column: &mut usize,
) -> Result<TokenWithMetadata, LexError> {
  let start = Metadata::new(*line, *column);
  let mut value = String::new();
  let mut illegal_escapes: Vec<(char, Metadata)> = Vec::new();
  chars.next();
  *column += 1;

  while let Some(&c) = chars.peek() {
    match c {
      '"' => {
        chars.next();
        *column += 1;
        let end = Metadata::new(*line, *column);
        if illegal_escapes.len() > 0 {
          return Err(LexError::IllegalEscapeInString {
            start,
            end,
            at: illegal_escapes,
          });
        }
        return Ok(TokenWithMetadata {
          token: Token::StringLiteral(value),
          start,
          end,
        });
      }
      '\r' | '\n' => {
        break;
      }
      // we only support \\, \n, \t, \"
      '\\' => {
        *column += 1;
        if let Some(next_c) = chars.nth(1) {
          match next_c {
            '\\' => value.push('\\'),
            'n' => value.push('\n'),
            't' => value.push('\t'),
            '"' => value.push('"'),
            _ => {
              illegal_escapes.push((next_c, Metadata::new(*line, *column)));
              if next_c == '\n' || next_c == '\r' {
                break;
              }
            }
          }
          *column += 1;
        } else {
          illegal_escapes.push((' ', Metadata::new(*line, *column)));
          break;
        }
      }
      _ => {
        // we actually support non ascii according to oracle in strings
        // if !c.is_ascii() {
        //   return Err(LexError::IllegalChar { char: c, start, end });
        // }
        value.push(c);
        chars.next();
        *column += 1;
      }
    }
  }

  if illegal_escapes.len() > 0 {
    return Err(LexError::UnterminatedStringWithIllegalEscape {
      start,
      end: Metadata::new(*line, *column),
      at: illegal_escapes,
    });
  }

  Err(LexError::UnterminatedString {
    start,
    end: Metadata::new(*line, *column),
  })
}

#[builder]
fn parse_keyword_or_identifier(
  chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
  line: &mut usize,
  column: &mut usize,
) -> Result<TokenWithMetadata, LexError> {
  let start = Metadata::new(*line, *column);
  let mut value = String::new();

  while let Some(&c) = chars.peek() {
    match c {
      c if c.is_alphanumeric() => {
        value.push(c);
        chars.next();
        *column += 1;
      }
      '_' => {
        value.push(c);
        chars.next();
        *column += 1;
      }
      // since ? is only valid as part of eh, we have to explicitly add this case
      '?' => {
        if value == "eh" {
          chars.next();
          *column += 1;
          return Ok(TokenWithMetadata {
            token: Token::Eh,
            start,
            end: Metadata::new(*line, *column),
          });
        }
        break;
      }
      _ => break,
    }
  }

  let end = Metadata::new(*line, *column);

  let str = &*value.clone();
  if let Some(keyword) = KEYWORDS.get(str) {
    return Ok(TokenWithMetadata {
      token: keyword.clone(),
      start,
      end,
    });
  }

  Ok(TokenWithMetadata {
    token: Token::Identifier(value),
    start,
    end,
  })
}

#[builder]
fn parse_int(
  chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
  line: &mut usize,
  column: &mut usize,
) -> Result<TokenWithMetadata, LexError> {
  let start = Metadata::new(*line, *column);
  let mut number_str = String::new();

  while let Some(&c) = chars.peek() {
    if c.is_ascii_digit() {
      number_str.push(c);
      chars.next();
      *column += 1;
    } else {
      break;
    }
  }

  let end = Metadata::new(*line, *column);
  match number_str.parse::<i32>() {
    Ok(number) => Ok(TokenWithMetadata {
      token: Token::IntLiteral(number.into()),
      start,
      end,
    }),
    Err(_e) => return Err(LexError::IntegerOverflow { start, end }),
  }
}

// at a certain point this should be refactored. i will now leave this message
// for the rest of the lifetime of this project
#[builder]
pub(crate) fn lex(input: &str) -> (VecWrapper<TokenWithMetadata>, VecWrapper<LexError>) {
  let mut tokens = Vec::new();
  let mut errors: Vec<LexError> = Vec::new();
  let mut line = 1;
  let mut column = 1;
  let mut chars = input.chars().peekable();

  // @codyduong TODO we can parallelize this since there are no multiline tokens
  while let Some(&c) = chars.peek() {
    let start = Metadata::new(line, column);
    match c {
      c if c.is_whitespace() => match c {
        '\n' => {
          line += 1;
          column = 1;
          chars.next();
        }
        '\r' => {
          column = 1;
          chars.next();
          if chars.peek().is_some_and(|c| *c != '\n') {
            // println!("Orphaned \\r, treating it as a \\r\\n");
            line += 1;
          }
        }
        _ => {
          column += 1;
          chars.next();
        }
      },
      '#' => {
        column += 1;
        chars.next();
        while let Some(&c) = chars.peek() {
          match c {
            '\n' => {
              line += 1;
              column = 1;
              chars.next();
              break;
            }
            _ => {
              column += 1;
              chars.next();
            }
          }
        }
      }
      ':' => {
        column += 1;
        chars.next();
        tokens.push(TokenWithMetadata {
          token: Token::Colon,
          start,
          end: Metadata::new(line, column),
        });
      }
      ',' => {
        column += 1;
        chars.next();
        tokens.push(TokenWithMetadata {
          token: Token::Comma,
          start,
          end: Metadata::new(line, column),
        });
      }
      '{' => {
        column += 1;
        chars.next();
        tokens.push(TokenWithMetadata {
          token: Token::LeftCurly,
          start,
          end: Metadata::new(line, column),
        });
      }
      '(' => {
        column += 1;
        chars.next();
        tokens.push(TokenWithMetadata {
          token: Token::LeftParen,
          start,
          end: Metadata::new(line, column),
        });
      }
      '&' => {
        column += 1;
        chars.next();
        tokens.push(TokenWithMetadata {
          token: Token::Ref,
          start,
          end: Metadata::new(line, column),
        });
      }
      '}' => {
        column += 1;
        chars.next();
        tokens.push(TokenWithMetadata {
          token: Token::RightCurly,
          start,
          end: Metadata::new(line, column),
        });
      }
      ')' => {
        column += 1;
        chars.next();
        tokens.push(TokenWithMetadata {
          token: Token::RightParen,
          start,
          end: Metadata::new(line, column),
        });
      }
      ';' => {
        column += 1;
        chars.next();
        tokens.push(TokenWithMetadata {
          token: Token::Semicolon,
          start,
          end: Metadata::new(line, column),
        });
      }
      '/' => {
        column += 1;
        chars.next();
        tokens.push(TokenWithMetadata {
          token: Token::Slash,
          start,
          end: Metadata::new(line, column),
        });
      }
      '*' => {
        column += 1;
        chars.next();
        tokens.push(TokenWithMetadata {
          token: Token::Star,
          start,
          end: Metadata::new(line, column),
        });
      }
      '+' => {
        // +, ++
        column += 1;
        chars.next();
        if chars.peek().is_some_and(|c| *c == '+') {
          column += 1;
          chars.next();
          tokens.push(TokenWithMetadata {
            token: Token::Increment,
            start,
            end: Metadata::new(line, column),
          });
        } else {
          tokens.push(TokenWithMetadata {
            token: Token::Cross,
            start,
            end: Metadata::new(line, column),
          });
        }
      }
      '>' => {
        // >, >=
        column += 1;
        chars.next();
        if chars.peek().is_some_and(|c| *c == '=') {
          column += 1;
          chars.next();
          tokens.push(TokenWithMetadata {
            token: Token::GreaterThanEqual,
            start,
            end: Metadata::new(line, column),
          });
        } else {
          tokens.push(TokenWithMetadata {
            token: Token::GreaterThan,
            start,
            end: Metadata::new(line, column),
          });
        }
      }
      '<' => {
        // <, <=
        column += 1;
        chars.next();
        if chars.peek().is_some_and(|c| *c == '=') {
          column += 1;
          chars.next();
          tokens.push(TokenWithMetadata {
            token: Token::LessThanEqual,
            start,
            end: Metadata::new(line, column),
          });
        } else {
          tokens.push(TokenWithMetadata {
            token: Token::LessThan,
            start,
            end: Metadata::new(line, column),
          });
        }
      }
      '!' => {
        // !, !=
        column += 1;
        chars.next();
        if chars.peek().is_some_and(|c| *c == '=') {
          column += 1;
          chars.next();
          tokens.push(TokenWithMetadata {
            token: Token::NotEquals,
            start,
            end: Metadata::new(line, column),
          });
        } else {
          tokens.push(TokenWithMetadata {
            token: Token::Not,
            start,
            end: Metadata::new(line, column),
          });
        }
      }
      '=' => {
        // =, ==
        column += 1;
        chars.next();
        if chars.peek().is_some_and(|c| *c == '=') {
          column += 1;
          chars.next();
          tokens.push(TokenWithMetadata {
            token: Token::Equals,
            start,
            end: Metadata::new(line, column),
          });
        } else {
          tokens.push(TokenWithMetadata {
            token: Token::Assign,
            start,
            end: Metadata::new(line, column),
          });
        }
      }
      '-' => {
        // -, --, ->
        column += 1;
        chars.next();
        let c = chars.peek();
        match c {
          Some('-') => {
            column += 1;
            chars.next();
            tokens.push(TokenWithMetadata {
              token: Token::Decrement,
              start,
              end: Metadata::new(line, column),
            });
          }
          Some('>') => {
            column += 1;
            chars.next();
            tokens.push(TokenWithMetadata {
              token: Token::RightArrow,
              start,
              end: Metadata::new(line, column),
            });
          }
          _ => {
            tokens.push(TokenWithMetadata {
              token: Token::Dash,
              start,
              end: Metadata::new(line, column),
            });
          }
        }
      }
      '?' => {
        errors.push(LexError::IllegalChar {
          char: c,
          at: Metadata::new(line, column),
        });
        chars.next();
        column += 1;
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
      c if c.is_ascii_digit() => match parse_int().chars(&mut chars).line(&mut line).column(&mut column).call() {
        Ok(token) => tokens.push(token),
        Err(err) => errors.push(err),
      },
      c if c.is_ascii_alphanumeric() || c == '_' => {
        match parse_keyword_or_identifier()
          .chars(&mut chars)
          .line(&mut line)
          .column(&mut column)
          .call()
        {
          Ok(token) => tokens.push(token),
          Err(err) => errors.push(err),
        }
      }
      _ => {
        errors.push(LexError::IllegalChar {
          char: c,
          at: Metadata::new(line, column),
        });
        chars.next();
        column += 1;
      }
    };
  }
  tokens.push(TokenWithMetadata {
    token: Token::EOF,
    start: Metadata::new(line, column),
    end: Metadata::new(line, column),
  });

  (tokens.into(), errors.into())
}

#[derive(Debug, PartialEq)]
pub(crate) enum LexError {
  IllegalChar {
    char: char,
    at: Metadata,
  },
  UnterminatedString {
    start: Metadata,
    end: Metadata,
  },
  UnterminatedStringWithIllegalEscape {
    start: Metadata,
    end: Metadata,
    at: Vec<(char, Metadata)>,
  },
  IllegalEscapeInString {
    start: Metadata,
    end: Metadata,
    at: Vec<(char, Metadata)>,
  },
  IntegerOverflow {
    start: Metadata,
    end: Metadata,
  },
}

impl std::fmt::Display for LexError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      LexError::IllegalChar { char, at } => {
        write!(
          f,
          "FATAL [{},{}]-[{},{}]: Illegal character {}",
          at.line,
          at.column,
          at.line,
          at.column + 1,
          char
        )
      }
      LexError::UnterminatedString { start, end } => write!(
        f,
        "FATAL [{},{}]-[{},{}]: Unterminated string literal detected",
        start.line, start.column, end.line, end.column
      ),
      LexError::UnterminatedStringWithIllegalEscape { start, end, at: _ } => write!(
        f,
        "FATAL [{},{}]-[{},{}]: Unterminated string literal with bad escape sequence detected",
        start.line, start.column, end.line, end.column
      ),
      LexError::IllegalEscapeInString { start, end, at: _ } => write!(
        f,
        "FATAL [{},{}]-[{},{}]: String literal with bad escape sequence detected",
        start.line, start.column, end.line, end.column
      ),
      LexError::IntegerOverflow { start, end } => write!(
        f,
        "FATAL [{},{}]-[{},{}]: Integer literal overflow",
        start.line, start.column, end.line, end.column
      ),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn test_keywords() {
    for (keyword, expected_token_kind) in KEYWORDS.iter() {
      let input = *keyword;
      let results = lex().input(input).call();
      let tokens = results.0;
      let errors = results.1;

      println!("Tokens: {}\nErrors: {}\n", tokens, errors);

      assert_eq!(tokens.len(), 2);
      assert_eq!(errors.len(), 0);
      assert_eq!(tokens[0].token, *expected_token_kind, "Failed on keyword '{}'", input);

      assert_eq!(tokens[1].token, Token::EOF);
    }
  }

  #[test]
  fn test_identifiers() {
    let input = "\
baz foo_bar _foo_bar                                                        \n\
foo123 foo456 foo7890                                                       \n\
eheh                                                                        \n\
eheh?                                                                       \n\
while1                                                                      \n\
";
    let results = lex().input(&input).call();
    let tokens = results.0;
    let errors = results.1;

    println!("Tokens: {}\nErrors: {}\n", tokens, errors);

    assert_eq!(tokens.len(), 10);
    assert_eq!(errors.len(), 1);

    #[rustfmt::skip]
    assert_eq!(tokens[0], TokenWithMetadata { token: Token::Identifier("baz".to_string()), start: Metadata::new(1, 1), end: Metadata::new(1, 4) } );
    #[rustfmt::skip]
    assert_eq!(tokens[1], TokenWithMetadata { token: Token::Identifier("foo_bar".to_string()), start: Metadata::new(1, 5), end: Metadata::new(1, 12) } );
    #[rustfmt::skip]
    assert_eq!(tokens[2], TokenWithMetadata { token: Token::Identifier("_foo_bar".to_string()), start: Metadata::new(1, 13), end: Metadata::new(1, 21) } );
    #[rustfmt::skip]
    assert_eq!(tokens[3], TokenWithMetadata { token: Token::Identifier("foo123".to_string()), start: Metadata::new(2, 1), end: Metadata::new(2, 7) } );
    #[rustfmt::skip]
    assert_eq!(tokens[4], TokenWithMetadata { token: Token::Identifier("foo456".to_string()), start: Metadata::new(2, 8), end: Metadata::new(2, 14) } );
    #[rustfmt::skip]
    assert_eq!(tokens[5], TokenWithMetadata { token: Token::Identifier("foo7890".to_string()), start: Metadata::new(2, 15), end: Metadata::new(2, 22) } );
    #[rustfmt::skip]
    assert_eq!(tokens[6], TokenWithMetadata { token: Token::Identifier("eheh".to_string()), start: Metadata::new(3, 1), end: Metadata::new(3, 5) } );
    #[rustfmt::skip]
    assert_eq!(tokens[7], TokenWithMetadata { token: Token::Identifier("eheh".to_string()), start: Metadata::new(4, 1), end: Metadata::new(4, 5) } );
    #[rustfmt::skip]
    assert_eq!(errors[0], LexError::IllegalChar { char: '?', at: Metadata::new(4, 5) } );
    #[rustfmt::skip]
    assert_eq!(tokens[8], TokenWithMetadata { token: Token::Identifier("while1".to_string()), start: Metadata::new(5, 1), end: Metadata::new(5, 7) } );

    assert_eq!(tokens.last().unwrap().token, Token::EOF);
  }

  #[test]
  fn test_string_literals() {
    let input = "\
\"\"                                                                        \n\
\"&!10\"                                                                    \n\
\"use \\n to denote a newline character\"                                   \n\
\"use \\\" for a quote and \\\\ for a backslash\"                           \n\
\"  \\\\ \\n\\t\\\"  \"      \"foo\"                                        \n\
\"this line has an unterminated string                                      \n\
\"\\f this line has an unterminated invalid escape                          \n\
\"\\f this line has an invalid escape\"                                     \n\
";
    let results = lex().input(&input).call();
    let tokens = results.0;
    let errors = results.1;

    println!("Tokens: {}\nErrors: {}\n", tokens, errors);

    assert_eq!(tokens.len(), 7);
    assert_eq!(errors.len(), 3);

    #[rustfmt::skip]
    assert_eq!(tokens[0], TokenWithMetadata { token: Token::StringLiteral("".to_string()), start: Metadata::new(1, 1), end: Metadata::new(1, 3) } );
    #[rustfmt::skip]
    assert_eq!(tokens[1], TokenWithMetadata { token: Token::StringLiteral("&!10".to_string()), start: Metadata::new(2, 1), end: Metadata::new(2, 7) } );
    #[rustfmt::skip]
    assert_eq!(tokens[2], TokenWithMetadata { token: Token::StringLiteral("use \n to denote a newline character".to_string()), start: Metadata::new(3, 1), end: Metadata::new(3, 39) } );
    #[rustfmt::skip]
    assert_eq!(tokens[3], TokenWithMetadata { token: Token::StringLiteral("use \" for a quote and \\ for a backslash".to_string()), start: Metadata::new(4, 1), end: Metadata::new(4, 44) } );
    #[rustfmt::skip]
    assert_eq!(tokens[4], TokenWithMetadata { token: Token::StringLiteral("  \\ \n\t\"  ".to_string()), start: Metadata::new(5, 1), end: Metadata::new(5, 16) } );
    #[rustfmt::skip]
    assert_eq!(tokens[5], TokenWithMetadata { token: Token::StringLiteral("foo".to_string()), start: Metadata::new(5, 22), end: Metadata::new(5, 27) } );
    #[rustfmt::skip]
    assert_eq!(errors[0], LexError::UnterminatedString { start: Metadata::new(6, 1), end: Metadata::new(6, 76) } );
    #[rustfmt::skip]
    assert_eq!(errors[1], LexError::UnterminatedStringWithIllegalEscape { start: Metadata::new(7, 1), end: Metadata::new(7, 75), at: vec![('f', Metadata::new(7, 3))] } );
    #[rustfmt::skip]
    assert_eq!(errors[2], LexError::IllegalEscapeInString { start: Metadata::new(8, 1), end: Metadata::new(8, 37), at: vec![('f', Metadata::new(8, 3))] } );

    assert_eq!(tokens.last().unwrap().token, Token::EOF);
  }

  #[test]
  fn test_int_literals() {
    let input = "\
2147483647                                                                  \n\
2147483648                                                                  \n\
-2147483647                                                                 \n\
-2147483648                                                                 \n\
";
    let results = lex().input(&input).call();
    let tokens = results.0;
    let errors = results.1;

    println!("Tokens: {}\nErrors: {}\n", tokens, errors);

    assert_eq!(tokens.len(), 5);
    assert_eq!(errors.len(), 2);

    #[rustfmt::skip]
    assert_eq!(tokens[0], TokenWithMetadata { token: Token::IntLiteral(2147483647), start: Metadata::new(1, 1), end: Metadata::new(1, 11) } );
    #[rustfmt::skip]
    assert_eq!(errors[0], LexError::IntegerOverflow { start: Metadata::new(2, 1), end: Metadata::new(2, 11) } );
    #[rustfmt::skip]
    assert_eq!(tokens[1], TokenWithMetadata { token: Token::Dash, start: Metadata::new(3, 1), end: Metadata::new(3, 2) } );
    #[rustfmt::skip]
    assert_eq!(tokens[2], TokenWithMetadata { token: Token::IntLiteral(2147483647), start: Metadata::new(3, 2), end: Metadata::new(3, 12) } );
    #[rustfmt::skip]
    assert_eq!(tokens[3], TokenWithMetadata { token: Token::Dash, start: Metadata::new(4, 1), end: Metadata::new(4, 2) } );
    #[rustfmt::skip]
    assert_eq!(errors[1], LexError::IntegerOverflow { start: Metadata::new(4, 2), end: Metadata::new(4, 12) } );

    assert_eq!(tokens.last().unwrap().token, Token::EOF);
  }

  #[test]
  fn test_symbols() {
    let input = "\
=    :    ,    +    -                                                       \n\
==    >    >=    {    <    <=                                               \n\
(    !    &    !=    --    ++                                               \n\
}    )    ;    /    *    ->                                                 \n\
";
    let results = lex().input(&input).call();
    let tokens = results.0;
    let errors = results.1;

    println!("Tokens: {}\nErrors: {}\n", tokens, errors);

    assert_eq!(tokens.len(), 24);
    assert_eq!(errors.len(), 0);

    #[rustfmt::skip]
    assert_eq!(tokens[0], TokenWithMetadata { token: Token::Assign, start: Metadata::new(1, 1), end: Metadata::new(1, 2) } );
    #[rustfmt::skip]
    assert_eq!(tokens[1], TokenWithMetadata { token: Token::Colon, start: Metadata::new(1, 6), end: Metadata::new(1, 7) } );
    #[rustfmt::skip]
    assert_eq!(tokens[2], TokenWithMetadata { token: Token::Comma, start: Metadata::new(1, 11), end: Metadata::new(1, 12) } );
    #[rustfmt::skip]
    assert_eq!(tokens[3], TokenWithMetadata { token: Token::Cross, start: Metadata::new(1, 16), end: Metadata::new(1, 17) } );
    #[rustfmt::skip]
    assert_eq!(tokens[4], TokenWithMetadata { token: Token::Dash, start: Metadata::new(1, 21), end: Metadata::new(1, 22) } );

    #[rustfmt::skip]
    assert_eq!(tokens[5], TokenWithMetadata { token: Token::Equals, start: Metadata::new(2, 1), end: Metadata::new(2, 3) } );
    #[rustfmt::skip]
    assert_eq!(tokens[6], TokenWithMetadata { token: Token::GreaterThan, start: Metadata::new(2, 7), end: Metadata::new(2, 8) } );
    #[rustfmt::skip]
    assert_eq!(tokens[7], TokenWithMetadata { token: Token::GreaterThanEqual, start: Metadata::new(2, 12), end: Metadata::new(2, 14) } );
    #[rustfmt::skip]
    assert_eq!(tokens[8], TokenWithMetadata { token: Token::LeftCurly, start: Metadata::new(2, 18), end: Metadata::new(2, 19) } );
    #[rustfmt::skip]
    assert_eq!(tokens[9], TokenWithMetadata { token: Token::LessThan, start: Metadata::new(2, 23), end: Metadata::new(2, 24) } );
    #[rustfmt::skip]
    assert_eq!(tokens[10], TokenWithMetadata { token: Token::LessThanEqual, start: Metadata::new(2, 28), end: Metadata::new(2, 30) } );

    #[rustfmt::skip]
    assert_eq!(tokens[11], TokenWithMetadata { token: Token::LeftParen, start: Metadata::new(3, 1), end: Metadata::new(3, 2) } );
    #[rustfmt::skip]
    assert_eq!(tokens[12], TokenWithMetadata { token: Token::Not, start: Metadata::new(3, 6), end: Metadata::new(3, 7) } );
    #[rustfmt::skip]
    assert_eq!(tokens[13], TokenWithMetadata { token: Token::Ref, start: Metadata::new(3, 11), end: Metadata::new(3, 12) } );
    #[rustfmt::skip]
    assert_eq!(tokens[14], TokenWithMetadata { token: Token::NotEquals, start: Metadata::new(3, 16), end: Metadata::new(3, 18) } );
    #[rustfmt::skip]
    assert_eq!(tokens[15], TokenWithMetadata { token: Token::Decrement, start: Metadata::new(3, 22), end: Metadata::new(3, 24) } );
    #[rustfmt::skip]
    assert_eq!(tokens[16], TokenWithMetadata { token: Token::Increment, start: Metadata::new(3, 28), end: Metadata::new(3, 30) } );

    #[rustfmt::skip]
    assert_eq!(tokens[17], TokenWithMetadata { token: Token::RightCurly, start: Metadata::new(4, 1), end: Metadata::new(4, 2) } );
    #[rustfmt::skip]
    assert_eq!(tokens[18], TokenWithMetadata { token: Token::RightParen, start: Metadata::new(4, 6), end: Metadata::new(4, 7) } );
    #[rustfmt::skip]
    assert_eq!(tokens[19], TokenWithMetadata { token: Token::Semicolon, start: Metadata::new(4, 11), end: Metadata::new(4, 12) } );
    #[rustfmt::skip]
    assert_eq!(tokens[20], TokenWithMetadata { token: Token::Slash, start: Metadata::new(4, 16), end: Metadata::new(4, 17) } );
    #[rustfmt::skip]
    assert_eq!(tokens[21], TokenWithMetadata { token: Token::Star, start: Metadata::new(4, 21), end: Metadata::new(4, 22) } );
    #[rustfmt::skip]
    assert_eq!(tokens[22], TokenWithMetadata { token: Token::RightArrow, start: Metadata::new(4, 26), end: Metadata::new(4, 28) } );

    assert_eq!(tokens.last().unwrap().token, Token::EOF);
  }
}
