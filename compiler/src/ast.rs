use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Program(pub(crate) Vec<Decl>);

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Decl {
  VarDecl(VarDecl),
  FnDecl(FnDecl),
  ClassDecl { name: String, body: Vec<Decl> },
}

impl From<VarDecl> for Decl {
  fn from(value: VarDecl) -> Self {
    Self::VarDecl(value)
  }
}

impl From<FnDecl> for Decl {
  fn from(value: FnDecl) -> Self {
    Self::FnDecl(value)
  }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct VarDecl {
  pub(crate) name: String,
  pub(crate) var_type: Type,
  pub(crate) init: Option<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct FnDecl {
  pub(crate) name: String,
  pub(crate) params: Vec<Param>,
  pub(crate) ret_type: Type,
  pub(crate) body: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Param {
  pub(crate) name: String,
  pub(crate) param_type: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Type {
  Int { mutable: bool, is_ref: bool },
  Bool { mutable: bool, is_ref: bool },
  Void { mutable: bool, is_ref: bool },
  Custom { name: String, mutable: bool, is_ref: bool },
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Stmt {
  VarDecl(VarDecl),
  Assign {
    target: Expr,
    value: Expr,
  },
  // idk what the equivalent "normal" this is supposed to be? a ternary?
  MeansOtherwise {
    loc: Var,
    means: Expr,
    otherwise: Expr,
  },
  Call {
    function: Var,
    args: Vec<Expr>,
  },
  If {
    cond: Expr,
    then_block: Block,
    else_block: Option<Block>,
  },
  While {
    cond: Expr,
    body: Block,
  },
  ToConsole(Expr),
  FromConsole(Var),
  Return(Option<Expr>),
}

impl From<VarDecl> for Stmt {
  fn from(value: VarDecl) -> Self {
    Self::VarDecl(value)
  }
}

pub(crate) type Block = Vec<Stmt>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr {
  BinaryOp {
    left: Box<Expr>,
    op: BinaryOp,
    right: Box<Expr>,
  },
  UnaryOp {
    op: UnaryOp,
    expr: Box<Expr>,
  },
  Literal(Literal),
  Var(Var),
  Call {
    function: Var,
    args: Vec<Expr>,
  },
}

impl From<Literal> for Expr {
  fn from(value: Literal) -> Self {
    Self::Literal(value)
  }
}

impl From<Var> for Expr {
  fn from(value: Var) -> Self {
    Self::Var(value)
  }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Var {
  pub(crate) name: String,
  pub(crate) parent: Option<Box<Var>>, // if this var belongs to another var
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
  And,
  Or,
  Equals,
  NotEquals,
  GreaterThan,
  GreaterThanEqual,
  LessThan,
  LessThanEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum UnaryOp {
  Not,
  Negate,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Literal {
  None,
  Int(i32),
  Bool(bool),
  String(String),
}

impl From<i32> for Literal {
  fn from(value: i32) -> Self {
    Self::Int(value)
  }
}

/**
 * this code is not very DRY... LOL!
 *
 * im not making a proc macro for this. if it works, don't touch it.
 * tech debt is not real.
 */

trait PrettyPrint {
  fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent_level: usize) -> fmt::Result;
}

impl fmt::Display for Param {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}: {}", self.name, self.param_type)
  }
}

impl fmt::Display for Var {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(parent) = &self.parent {
      write!(f, "{}->{}", parent, self.name)
    } else {
      write!(f, "{}", self.name)
    }
  }
}

impl fmt::Display for Literal {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Literal::None => write!(f, "eh"),
      Literal::Int(value) => write!(f, "{}", value),
      Literal::Bool(value) => write!(f, "{}", value),
      Literal::String(value) => write!(f, "\"{}\"", value),
    }
  }
}

impl fmt::Display for UnaryOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let op_str = match self {
      UnaryOp::Not => "not ",
      UnaryOp::Negate => "-",
    };
    write!(f, "{}", op_str)
  }
}

impl fmt::Display for BinaryOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let op_str = match self {
      BinaryOp::Add => "+",
      BinaryOp::Sub => "-",
      BinaryOp::Mul => "*",
      BinaryOp::Div => "/",
      BinaryOp::And => "and",
      BinaryOp::Or => "or",
      BinaryOp::Equals => "==",
      BinaryOp::NotEquals => "!=",
      BinaryOp::GreaterThan => ">",
      BinaryOp::GreaterThanEqual => ">=",
      BinaryOp::LessThan => "<",
      BinaryOp::LessThanEqual => "<=",
    };
    write!(f, "{}", op_str)
  }
}

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let (type_name, mutable, is_ref) = match self {
      Type::Int { mutable, is_ref } => ("int", mutable, is_ref),
      Type::Bool { mutable, is_ref } => ("bool", mutable, is_ref),
      Type::Void { mutable, is_ref } => ("void", mutable, is_ref),
      Type::Custom { name, mutable, is_ref } => (name.as_str(), mutable, is_ref),
    };

    // Format the type
    if *mutable {
      write!(f, "immutable ")?;
    }
    if *is_ref {
      write!(f, "&")?;
    }
    write!(f, "{}", type_name)
  }
}

impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.fmt_with_indent(f, 0)
  }
}

impl PrettyPrint for Expr {
  fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent_level: usize) -> fmt::Result {
    match self {
      Expr::BinaryOp { left, op, right } => {
        write!(f, "(")?;
        left.fmt_with_indent(f, indent_level)?;
        write!(f, " {} ", op)?;
        right.fmt_with_indent(f, indent_level)?;
        write!(f, ")")
      }
      Expr::UnaryOp { op, expr } => {
        write!(f, "({}", op)?;
        expr.fmt_with_indent(f, indent_level)?;
        write!(f, ")")
      }
      Expr::Literal(lit) => write!(f, "{}", lit),
      Expr::Var(var) => write!(f, "{}", var),
      Expr::Call { function, args } => {
        write!(f, "{}(", function)?;
        for (i, arg) in args.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          arg.fmt_with_indent(f, indent_level)?;
        }
        write!(f, ")")
      }
    }
  }
}

impl PrettyPrint for VarDecl {
  fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent_level: usize) -> fmt::Result {
    for _ in 0..indent_level {
      write!(f, "{}", INDENT)?;
    }
    write!(f, "{}: {}", self.name, self.var_type)?;
    if let Some(init_expr) = &self.init {
      write!(f, " = ")?;
      init_expr.fmt_with_indent(f, indent_level)?;
    }
    writeln!(f, ";")
  }
}

impl fmt::Display for VarDecl {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.fmt_with_indent(f, 0)
  }
}

impl PrettyPrint for FnDecl {
  fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent_level: usize) -> fmt::Result {
    for _ in 0..indent_level {
      write!(f, "{}", INDENT)?;
    }
    write!(f, "{}: (", self.name)?;
    for (i, param) in self.params.iter().enumerate() {
      if i > 0 {
        write!(f, ", ")?;
      }
      write!(f, "{}", param)?;
    }
    write!(f, ") -> {}", self.ret_type)?;
    writeln!(f, " {{")?;
    for stmt in &self.body {
      stmt.fmt_with_indent(f, indent_level + 1)?;
    }
    for _ in 0..indent_level {
      write!(f, "{}", INDENT)?;
    }
    writeln!(f, "}}")
  }
}

impl fmt::Display for FnDecl {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.fmt_with_indent(f, 0)
  }
}

const INDENT: &str = "    ";

impl PrettyPrint for Stmt {
  fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent_level: usize) -> fmt::Result {
    match self {
      Stmt::VarDecl(var_decl) => var_decl.fmt_with_indent(f, indent_level),
      Stmt::Assign { target, value } => {
        for _ in 0..indent_level {
          write!(f, "{}", INDENT)?;
        }
        target.fmt_with_indent(f, 0)?;
        write!(f, " = ")?;
        value.fmt_with_indent(f, 0)?;
        writeln!(f, ";")
      }
      Stmt::MeansOtherwise { loc, means, otherwise } => {
        for _ in 0..indent_level {
          write!(f, "{}", INDENT)?;
        }
        write!(f, "{} means ", loc)?;
        means.fmt_with_indent(f, 0)?;
        write!(f, " otherwise ")?;
        otherwise.fmt_with_indent(f, 0)?;
        writeln!(f, ";")
      }
      Stmt::Call { function, args } => {
        for _ in 0..indent_level {
          write!(f, "{}", INDENT)?;
        }
        write!(f, "{}(", function)?;
        for (i, arg) in args.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          arg.fmt_with_indent(f, 0)?;
        }
        writeln!(f, ");")
      }
      Stmt::If {
        cond,
        then_block,
        else_block,
      } => {
        for _ in 0..indent_level {
          write!(f, "{}", INDENT)?;
        }
        write!(f, "if (")?;
        cond.fmt_with_indent(f, 0)?;
        writeln!(f, ") {{")?;
        for stmt in then_block {
          stmt.fmt_with_indent(f, indent_level + 1)?;
        }
        for _ in 0..indent_level {
          write!(f, "{}", INDENT)?;
        }
        writeln!(f, "}}")?;
        if let Some(else_block) = else_block {
          for _ in 0..indent_level {
            write!(f, "{}", INDENT)?;
          }
          writeln!(f, "else {{")?;
          for stmt in else_block {
            stmt.fmt_with_indent(f, indent_level + 1)?;
          }
          for _ in 0..indent_level {
            write!(f, "{}", INDENT)?;
          }
          writeln!(f, "}}")?;
        }
        Ok(())
      }
      Stmt::While { cond, body } => {
        for _ in 0..indent_level {
          write!(f, "{}", INDENT)?;
        }
        write!(f, "while (")?;
        cond.fmt_with_indent(f, 0)?;
        writeln!(f, ") {{")?;
        for stmt in body {
          stmt.fmt_with_indent(f, indent_level + 1)?;
        }
        for _ in 0..indent_level {
          write!(f, "{}", INDENT)?;
        }
        writeln!(f, "}}")
      }
      Stmt::ToConsole(expr) => {
        for _ in 0..indent_level {
          write!(f, "{}", INDENT)?;
        }
        write!(f, "toconsole ")?;
        expr.fmt_with_indent(f, 0)?;
        writeln!(f, ";")
      }
      Stmt::FromConsole(var) => {
        for _ in 0..indent_level {
          write!(f, "{}", INDENT)?;
        }
        writeln!(f, "fromconsole {};", var)
      }
      Stmt::Return(Some(expr)) => {
        for _ in 0..indent_level {
          write!(f, "{}", INDENT)?;
        }
        write!(f, "return ")?;
        expr.fmt_with_indent(f, 0)?;
        writeln!(f, ";")
      }
      Stmt::Return(None) => {
        for _ in 0..indent_level {
          write!(f, "{}", INDENT)?;
        }
        writeln!(f, "return;")
      }
    }
  }
}

// For Decl
impl PrettyPrint for Decl {
  fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent_level: usize) -> fmt::Result {
    match self {
      Decl::VarDecl(var_decl) => var_decl.fmt_with_indent(f, indent_level),
      Decl::FnDecl(fn_decl) => fn_decl.fmt_with_indent(f, indent_level),
      Decl::ClassDecl { name, body } => {
        for _ in 0..indent_level {
          write!(f, "{}", INDENT)?;
        }
        writeln!(f, "{}: custom {{", name)?;
        for decl in body {
          decl.fmt_with_indent(f, indent_level + 1)?;
        }
        for _ in 0..indent_level {
          write!(f, "{}", INDENT)?;
        }
        writeln!(f, "}};")
      }
    }
  }
}

impl fmt::Display for Decl {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.fmt_with_indent(f, 0)
  }
}

impl fmt::Display for Program {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for decl in &self.0 {
      decl.fmt_with_indent(f, 0)?;
    }
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use crate::util::TokenWithMetadata;

  #[test]
  fn test_all() {
    let part_input = "\
id: int;
id: &int;
id: immutable int;
id: immutable &int;
id: bool;
id: &bool;
id: immutable bool;
id: immutable &bool;
id: void;
id: &void;
id: immutable void;
id: immutable &void;
id: custom_type;
id: &custom_type;
id: immutable custom_type;
id: immutable &custom_type;

empty_function:()->int{}
empty_function:()->&int{}
empty_function:()->immutable int{}
empty_function:()->immutable &int{}
empty_function:()->bool{}
empty_function:()->&bool{}
empty_function:()->immutable bool{}
empty_function:()->immutable &bool{}
empty_function:()->void{}
empty_function:()->&void{}
empty_function:()->immutable void{}
empty_function:()->immutable &void{}
empty_function:()->custom_type{}
empty_function:()->&custom_type{}
empty_function:()->immutable custom_type{}
empty_function:()->immutable &custom_type{}

my_function:(
  id: int,
  id: &int,
  id: immutable int,
  id: immutable &int,
  id: bool,
  id: &bool,
  id: immutable bool,
  id: immutable &bool,
  id: void,
  id: &void,
  id: immutable void,
  id: immutable &void,
  id: custom_type,
  id: &custom_type,
  id: immutable custom_type,
  id: immutable &custom_type
) -> void {
  id: int;
  id: &int;
  id: immutable int;
  id: immutable &int;
  id: bool;
  id: &bool;
  id: immutable bool;
  id: immutable &bool;
  id: void;
  id: &void;
  id: immutable void;
  id: immutable &void;
  id: custom_type;
  id: &custom_type;
  id: immutable custom_type;
  id: immutable &custom_type;

  call_me();
  call->me(foo, bar->baz, !foo + bar->baz - (-1) * hi / true and false or eh? >= eh? or \"string\" <= eh? or eh? == eh? or eh? > eh? or eh? < eh? );
  test_expr_nest(test_expr_nest(test_expr_nest()));
  if(expr) {}
  if(expr) {} else {}
  while(expr) {}
  foo = foo;
  foo = 1;
  foo = true;
  foo = false;
  foo = eh?;
  foo++;
  foo--;
  foo means business otherwise !foo + bar->baz - (-1) * hi / true and false or eh? >= eh? or \"string\" <= eh? or eh? == eh? or eh? > eh? or eh? < eh?;
  toconsole !foo + bar->baz - (-1) * hi / true and false or eh? >= eh? or \"string\" <= eh? or eh? == eh? or eh? > eh? or eh? < eh?;
  fromconsole foo;
  return;
  return !foo + bar->baz - (-1) * hi / true and false or eh? >= eh? or \"string\" <= eh? or eh? == eh? or eh? > eh? or eh? < eh?;
}
    ";

    let input = format!("{} my_class : custom {{{}}}; ", part_input, part_input);
    let results = crate::lex::lex().input(&input).call();
    let tokens = results.0;
    let errors = results.1;

    println!("Tokens: {}\nErrors: {}\n", tokens, errors);

    assert_eq!(errors.len(), 0);

    let tokens = Into::<Vec<TokenWithMetadata>>::into(tokens);
    let mut parser = crate::parse::Parser::new(&tokens);
    let program = parser.parse_program().unwrap();

    println!("Program:\n{}", program);

    // yeah i dont really feel like testing this. so panic to view output... it looks fine?
    panic!("");
  }
}
