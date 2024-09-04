use once_cell::sync::Lazy;
use std::{
  collections::HashMap,
  ops::{Add, Sub},
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

#[derive(Debug, PartialEq)]
pub(crate) struct TokenWithMetadata {
  pub(crate) token: Token,
  pub(crate) start: Metadata,
  pub(crate) end: Metadata,
}

impl std::fmt::Display for TokenWithMetadata {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self.token {
      // @codyduong TODO these have names that are not matching, read lexical spec and fix
      Token::Assign => write!(f, "assign: [{} {}]", self.start.line, self.start.column),
      Token::Colon => write!(f, "colon: [{} {}]", self.start.line, self.start.column),
      Token::Semicolon => write!(f, "semicolon: [{} {}]", self.start.line, self.start.column),
      Token::Comma => write!(f, "comma: [{} {}]", self.start.line, self.start.column),
      Token::Plus => write!(f, "plus: [{} {}]", self.start.line, self.start.column),
      Token::Minus => write!(f, "minus: [{} {}]", self.start.line, self.start.column),
      Token::Times => write!(f, "times: [{} {}]", self.start.line, self.start.column),
      Token::Divide => write!(f, "divide: [{} {}]", self.start.line, self.start.column),
      Token::Equals => write!(f, "equals: [{} {}]", self.start.line, self.start.column),
      Token::NotEquals => write!(f, "notequals: [{} {}]", self.start.line, self.start.column),
      Token::GreaterThan => write!(f, "greaterthan: [{} {}]", self.start.line, self.start.column),
      Token::GreaterThanEqual => write!(f, "greaterthanequal: [{} {}]", self.start.line, self.start.column),
      Token::LessThan => write!(f, "lessthan: [{} {}]", self.start.line, self.start.column),
      Token::LessThanEqual => write!(f, "lessthanequal: [{} {}]", self.start.line, self.start.column),
      Token::LeftCurly => write!(f, "leftcurly: [{} {}]", self.start.line, self.start.column),
      Token::RightCurly => write!(f, "rightcurly: [{} {}]", self.start.line, self.start.column),
      Token::LeftParen => write!(f, "leftparen: [{} {}]", self.start.line, self.start.column),
      Token::RightParen => write!(f, "rightparen: [{} {}]", self.start.line, self.start.column),
      Token::Not => write!(f, "not: [{} {}]", self.start.line, self.start.column),
      Token::BitAnd => write!(f, "bitand: [{} {}]", self.start.line, self.start.column),
      Token::Decrement => write!(f, "decrement: [{} {}]", self.start.line, self.start.column),
      Token::Increment => write!(f, "increment: [{} {}]", self.start.line, self.start.column),
      Token::RightArrow => write!(f, "rightarrow: [{} {}]", self.start.line, self.start.column),
      // Token::Comment => write!(f, "comment: [{} {}]", self.start.line, self.start.column),
      Token::And => write!(f, "and: [{} {}]", self.start.line, self.start.column),
      Token::Bool => write!(f, "bool: [{} {}]", self.start.line, self.start.column),
      Token::Custom => write!(f, "custom: [{} {}]", self.start.line, self.start.column),
      Token::Else => write!(f, "else: [{} {}]", self.start.line, self.start.column),
      Token::Eh => write!(f, "eh: [{} {}]", self.start.line, self.start.column),
      Token::False => write!(f, "false: [{} {}]", self.start.line, self.start.column),
      Token::FromConsole => write!(f, "fromconsole: [{} {}]", self.start.line, self.start.column),
      Token::If => write!(f, "if: [{} {}]", self.start.line, self.start.column),
      Token::Immutable => write!(f, "immutable: [{} {}]", self.start.line, self.start.column),
      Token::Int => write!(f, "int: [{} {}]", self.start.line, self.start.column),
      Token::Or => write!(f, "or: [{} {}]", self.start.line, self.start.column),
      Token::Otherwise => write!(f, "otherwise: [{} {}]", self.start.line, self.start.column),
      Token::Means => write!(f, "means: [{} {}]", self.start.line, self.start.column),
      Token::ToConsole => write!(f, "toconsole: [{} {}]", self.start.line, self.start.column),
      Token::Return => write!(f, "return: [{} {}]", self.start.line, self.start.column),
      Token::True => write!(f, "true: [{} {}]", self.start.line, self.start.column),
      Token::Void => write!(f, "void: [{} {}]", self.start.line, self.start.column),
      Token::While => write!(f, "while: [{} {}]", self.start.line, self.start.column),
      Token::StringLiteral(value) => write!(f, "STRINGLIT:{}", value),
      Token::IntLiteral(value) => write!(f, "INTLIT:{}", value),
      Token::Identifier(value) => write!(f, "ID:{}", value),
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Token {
  /* Literals */
  StringLiteral(String),
  IntLiteral(i64),
  Identifier(String),

  /* Symbols */
  Assign,           // =
  Colon,            // :
  Semicolon,        // ;
  Comma,            // ,
  Plus,             // +
  Minus,            // -
  Times,            // *
  Divide,           // /
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
  BitAnd,           // &, this is distinct from logical and
  Decrement,        // --
  Increment,        // ++
  RightArrow,       // ->
  // Comment,       // #, not a token that we need to a AST node for

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
}

// maybe use a better method... https://www.youtube.com/watch?v=DMQ_HcNSOAI
pub(crate) static KEYWORDS: Lazy<HashMap<&'static str, Token>> = Lazy::new(|| {
  let mut m = HashMap::new();
  m.insert("and", Token::And);
  m.insert("bool", Token::Bool);
  m.insert("custom", Token::Custom);
  m.insert("else", Token::Else);
  m.insert("eh", Token::Eh);
  m.insert("false", Token::False);
  m.insert("fromConsole", Token::FromConsole);
  m.insert("if", Token::If);
  m.insert("immutable", Token::Immutable);
  m.insert("int", Token::Int);
  m.insert("or", Token::Or);
  m.insert("otherwise", Token::Otherwise);
  m.insert("means", Token::Means);
  m.insert("toConsole", Token::ToConsole);
  m.insert("return", Token::Return);
  m.insert("true", Token::True);
  m.insert("void", Token::Void);
  m.insert("while", Token::While);
  m
});
