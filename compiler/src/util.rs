use once_cell::sync::Lazy;
use regex::Regex;
use std::{
  collections::HashMap,
  fmt,
  ops::{Add, Deref, DerefMut, Sub},
};

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Metadata {
  pub(crate) line: usize,
  pub(crate) column: usize,
}

impl Metadata {
  pub(crate) fn new(line: usize, column: usize) -> Self {
    Self { line, column }
  }
}

impl Add for Metadata {
  type Output = Self;

  fn add(self, other: Self) -> Self::Output {
    Self {
      line: self.line + other.line,
      column: self.column + other.column,
    }
  }
}

impl Sub for Metadata {
  type Output = Self;

  fn sub(self, other: Self) -> Self::Output {
    Self {
      line: self.line - other.line,
      column: self.column - other.column,
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct TokenWithMetadata {
  pub(crate) token: Token,
  pub(crate) start: Metadata,
  pub(crate) end: Metadata,
}

impl std::fmt::Display for TokenWithMetadata {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self.token {
      Token::Assign => write!(f, "ASSIGN [{},{}]", self.start.line, self.start.column),
      Token::Colon => write!(f, "COLON [{},{}]", self.start.line, self.start.column),
      Token::Semicolon => write!(f, "SEMICOL [{},{}]", self.start.line, self.start.column),
      Token::Comma => write!(f, "COMMA [{},{}]", self.start.line, self.start.column),
      Token::Cross => write!(f, "CROSS [{},{}]", self.start.line, self.start.column),
      Token::Dash => write!(f, "DASH [{},{}]", self.start.line, self.start.column),
      Token::Star => write!(f, "STAR [{},{}]", self.start.line, self.start.column),
      Token::Slash => write!(f, "SLASH [{},{}]", self.start.line, self.start.column),
      Token::Equals => write!(f, "EQUALS [{},{}]", self.start.line, self.start.column),
      Token::NotEquals => write!(f, "NOTEQUALS [{},{}]", self.start.line, self.start.column),
      Token::GreaterThan => write!(f, "GREATER [{},{}]", self.start.line, self.start.column),
      Token::GreaterThanEqual => write!(f, "GREATEREQ [{},{}]", self.start.line, self.start.column),
      Token::LessThan => write!(f, "LESS [{},{}]", self.start.line, self.start.column),
      Token::LessThanEqual => write!(f, "LESSEQ [{},{}]", self.start.line, self.start.column),
      Token::LeftCurly => write!(f, "LCURLY [{},{}]", self.start.line, self.start.column),
      Token::RightCurly => write!(f, "RCURLY [{},{}]", self.start.line, self.start.column),
      Token::LeftParen => write!(f, "LPAREN [{},{}]", self.start.line, self.start.column),
      Token::RightParen => write!(f, "RPAREN [{},{}]", self.start.line, self.start.column),
      Token::Not => write!(f, "NOT [{},{}]", self.start.line, self.start.column),
      Token::Ref => write!(f, "REF [{},{}]", self.start.line, self.start.column),
      Token::Decrement => write!(f, "POSTDEC [{},{}]", self.start.line, self.start.column),
      Token::Increment => write!(f, "POSTINC [{},{}]", self.start.line, self.start.column),
      Token::RightArrow => write!(f, "ARROW [{},{}]", self.start.line, self.start.column),
      Token::And => write!(f, "AND [{},{}]", self.start.line, self.start.column),
      Token::Bool => write!(f, "BOOL [{},{}]", self.start.line, self.start.column),
      Token::Custom => write!(f, "CUSTOM [{},{}]", self.start.line, self.start.column),
      Token::Else => write!(f, "ELSE [{},{}]", self.start.line, self.start.column),
      Token::Eh => write!(f, "EH [{},{}]", self.start.line, self.start.column),
      Token::False => write!(f, "FALSE [{},{}]", self.start.line, self.start.column),
      Token::FromConsole => write!(f, "FROMCONSOLE [{},{}]", self.start.line, self.start.column),
      Token::If => write!(f, "IF [{},{}]", self.start.line, self.start.column),
      Token::Immutable => write!(f, "IMMUTABLE [{},{}]", self.start.line, self.start.column),
      Token::Int => write!(f, "INT [{},{}]", self.start.line, self.start.column),
      Token::Or => write!(f, "OR [{},{}]", self.start.line, self.start.column),
      Token::Otherwise => write!(f, "OTHERWISE [{},{}]", self.start.line, self.start.column),
      Token::Means => write!(f, "MEANS [{},{}]", self.start.line, self.start.column),
      Token::ToConsole => write!(f, "TOCONSOLE [{},{}]", self.start.line, self.start.column),
      Token::Return => write!(f, "RETURN [{},{}]", self.start.line, self.start.column),
      Token::True => write!(f, "TRUE [{},{}]", self.start.line, self.start.column),
      Token::Void => write!(f, "VOID [{},{}]", self.start.line, self.start.column),
      Token::While => write!(f, "WHILE [{},{}]", self.start.line, self.start.column),
      Token::StringLiteral(value) => write!(
        f,
        "STRINGLIT:\"{}\" [{},{}]",
        escape_string(value),
        self.start.line,
        self.start.column
      ),
      Token::IntLiteral(value) => write!(f, "INTLIT:{} [{},{}]", value, self.start.line, self.start.column),
      Token::Identifier(value) => write!(f, "ID:{} [{},{}]", value, self.start.line, self.start.column),
      Token::EOF => write!(f, "EOF [{},{}]", self.end.line, self.end.column),
    }
  }
}

fn escape_string(s: &str) -> std::borrow::Cow<'_, str> {
  let re = Regex::new(r#"\\|\n|\t|\""#).unwrap();

  re.replace_all(s, |caps: &regex::Captures| {
    match &caps[0] {
      "\\" => "\\\\",
      "\n" => "\\n",
      "\t" => "\\t",
      "\"" => "\\\"",
      _ => unreachable!(),
    }
    .to_string()
  })
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Token {
  /* Literals */
  StringLiteral(String),
  IntLiteral(i32),
  Identifier(String),

  /* Symbols */
  Assign,           // =
  Colon,            // :
  Semicolon,        // ;
  Comma,            // ,
  Cross,            // +
  Dash,             // -
  Star,             // *
  Slash,            // /
  Equals,           // ==
  NotEquals,        // !=
  GreaterThan,      // >
  GreaterThanEqual, // >=
  LessThan,         // <
  LessThanEqual,    // <=
  LeftCurly,        // {
  RightCurly,       // }
  LeftParen,        // (
  RightParen,       // )
  Not,              // !
  Ref,              // &,
  Decrement,        // --
  Increment,        // ++
  RightArrow,       // ->

  /* Keywords */
  And,
  Bool,
  Custom,
  Else,
  Eh,
  False,
  FromConsole,
  If,
  Immutable,
  Int,
  Or,
  Otherwise,
  Means,
  ToConsole,
  Return,
  True,
  Void,
  While,

  // hmm from a semantics standpoint this doesn't make sense? like EOF isn't really a token,
  // but this is a useful idea to keep where our EOF is in some struct...
  // also maybe useful if we have an AST span across files? but im not familiar with how
  // that works in a real language
  EOF,
}

// maybe use a better method... https://www.youtube.com/watch?v=DMQ_HcNSOAI
pub(crate) static KEYWORDS: Lazy<HashMap<&str, Token>> = Lazy::new(|| {
  let mut m = HashMap::new();
  m.insert("and", Token::And);
  m.insert("bool", Token::Bool);
  m.insert("custom", Token::Custom);
  m.insert("else", Token::Else);
  m.insert("eh?", Token::Eh);
  m.insert("false", Token::False);
  m.insert("fromconsole", Token::FromConsole);
  m.insert("if", Token::If);
  m.insert("immutable", Token::Immutable);
  m.insert("int", Token::Int);
  m.insert("or", Token::Or);
  m.insert("otherwise", Token::Otherwise);
  m.insert("means", Token::Means);
  m.insert("toconsole", Token::ToConsole);
  m.insert("return", Token::Return);
  m.insert("true", Token::True);
  m.insert("void", Token::Void);
  m.insert("while", Token::While);
  m
});

pub(crate) struct VecWrapper<T> {
  items: Vec<T>,
}

impl<T> From<VecWrapper<T>> for Vec<T> {
  fn from(value: VecWrapper<T>) -> Self {
    value.items
  }
}

impl<T> Into<VecWrapper<T>> for Vec<T> {
  fn into(self) -> VecWrapper<T> {
    VecWrapper { items: self }
  }
}

impl<T: fmt::Display> fmt::Display for VecWrapper<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for item in &self.items {
      writeln!(f, "{}", item)?;
    }
    Ok(())
  }
}

impl<T: fmt::Debug> fmt::Debug for VecWrapper<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_list().entries(&self.items).finish()
  }
}

impl<T> Deref for VecWrapper<T> {
  type Target = Vec<T>;

  fn deref(&self) -> &Self::Target {
    &self.items
  }
}

impl<T> DerefMut for VecWrapper<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.items
  }
}

impl<T> IntoIterator for VecWrapper<T> {
  type Item = T;
  type IntoIter = std::vec::IntoIter<T>;

  fn into_iter(self) -> Self::IntoIter {
    self.items.into_iter()
  }
}

impl<'a, T> IntoIterator for &'a VecWrapper<T> {
  type Item = &'a T;
  type IntoIter = std::slice::Iter<'a, T>;

  fn into_iter(self) -> Self::IntoIter {
    self.items.iter()
  }
}

impl<'a, T> IntoIterator for &'a mut VecWrapper<T> {
  type Item = &'a mut T;
  type IntoIter = std::slice::IterMut<'a, T>;

  fn into_iter(self) -> Self::IntoIter {
    self.items.iter_mut()
  }
}
