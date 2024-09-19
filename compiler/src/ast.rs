#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Decl {
  VarDecl(VarDecl),
  FnDecl(FnDecl),
  ClassDecl {
    name: String,
    body: Vec<Decl>,
  },
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
