use crate::ast::BinaryOp;
use crate::ast::Block;
use crate::ast::Decl;
use crate::ast::Expr;
use crate::ast::FnDecl;
use crate::ast::Literal;
use crate::ast::Param;
use crate::ast::Program;
use crate::ast::Stmt;
use crate::ast::Type;
use crate::ast::UnaryOp;
use crate::ast::Var;
use crate::ast::VarDecl;
use crate::util::Token;
use crate::util::TokenWithMetadata;
// use bon::bon;
// use bon::builder;

/**
 * Postscript:
 * 
 * yeah this is it where soaks in that i shouldve used something like pest. or even like any grammar generator
 * even reading the output from another scanner generator would've been better than this. eh? it was fun.
 *
 *  Some notes
 * - Associativeness is not preserved within the overall AST, only within Expression ASTs (where its binary)
 *    - also it says Assignments are Right Associative? which doesn't really make sense? we don't have any
 *      have expressiveness like `a = b = c`, and expressions cannot do assigments anyways.
 * - Order of expr precedence is:

NOT, NEGATE (!, -) (right associative)
MULTIPLY (*), DIVIDE (/) (left-associative)
ADD (+), SUBTRACT (-) (left-associative)
COMPARE and EQUALITY (>, >=, <, <= and ==, !=) (non-associative)
AND (&&) (left-associative)
OR (||) (left-associative)

 * - PostInc and PostDec are simply syntactic sugar around `loc = loc + 1` or `loc = loc - 1`
 *   as they have not been specified as being acceptable grammars in an expr
 * - You can have `immutable void` and `immutable void` which are nonsensical (this is fine i guess),
 *   from a philosophical standpoint its syntactically correct but meaningless, like "sleeping furiously"
 *
 */

pub(crate) struct Parser<'a> {
  tokens: &'a [TokenWithMetadata],
  current: usize,
}

// #[bon]
impl<'a> Parser<'a> {
  pub(crate) fn new(tokens: &'a [TokenWithMetadata]) -> Self {
    Self { tokens, current: 0 }
  }

  fn current_token(&self) -> Option<&TokenWithMetadata> {
    self.tokens.get(self.current)
  }

  fn peek_token(&self) -> Option<&TokenWithMetadata> {
    self.tokens.get(self.current + 1)
  }

  fn peek_tokens(&self, i: usize) -> Option<&TokenWithMetadata> {
    self.tokens.get(self.current + i)
  }

  fn advance(&mut self) {
    self.current += 1;
  }

  fn expect_token(&mut self, expected: Token) -> Result<(), ParseError> {
    match self.current_token() {
      Some(TokenWithMetadata { token, .. }) if *token == expected => {
        self.advance();
        Ok(())
      }
      _ => match expected {
        Token::Semicolon => Err(ParseError::ExpectedSemicolon {
          received: self.current_token().cloned(),
        }),
        Token::Identifier(_) => Err(ParseError::ExpectedIdentifier {
          received: self.current_token().cloned(),
        }),
        _ => Err(ParseError::ExpectedToken {
          expected: expected,
          received: self.current_token().cloned(),
        }),
      },
    }
  }

  pub(crate) fn parse_program(&mut self) -> Result<Program, ParseError> {
    let mut decls = Vec::new();
    while self.current_token().is_some_and(|t| t.token != Token::EOF) {
      decls.push(self.parse_decl()?);
    }
    Ok(Program(decls))
  }

  fn parse_decl(&mut self) -> Result<Decl, ParseError> {
    let name = self.current_token();
    let colon = self.peek_token();

    if !name.is_some_and(|t| matches!(t.token, Token::Identifier(_))) {
      return Err(ParseError::ExpectedIdentifier {
        received: name.cloned(),
      });
    }

    if !colon.is_some_and(|t| t.token == Token::Colon) {
      return Err(ParseError::ExpectedToken {
        expected: Token::Colon,
        received: colon.cloned(),
      });
    }

    match self.peek_tokens(2).map(|t| &t.token) {
      Some(Token::Custom) => self.parse_class_decl().map(Into::into),
      Some(Token::LeftParen) => self.parse_fn_decl().map(Into::into),
      Some(_) => self.parse_var_decl().map(Into::into),
      _ => Err(ParseError::EOFError),
    }
  }

  fn parse_var_decl(&mut self) -> Result<VarDecl, ParseError> {
    let name = self.parse_identifier()?;
    self.expect_token(Token::Colon)?;
    let var_type = self.parse_type()?;
    let init = if self.current_token().is_some_and(|t| t.token == Token::Assign) {
      self.advance();
      Some(self.parse_expr()?)
    } else {
      None
    };
    self.expect_token(Token::Semicolon)?;
    Ok(VarDecl { name, var_type, init })
  }

  fn parse_class_decl(&mut self) -> Result<Decl, ParseError> {
    let name = self.parse_identifier()?;
    self.expect_token(Token::Colon)?;
    self.expect_token(Token::Custom)?;
    self.expect_token(Token::LeftCurly)?;
    let mut body: Vec<Decl> = Vec::new();
    while self.current_token().is_some_and(|t| t.token != Token::RightCurly) {
      // attempt to parse as a vardelc
      let old_pos = self.current;
      match self.parse_var_decl() {
        Ok(vardecl) => body.push(vardecl.into()),
        Err(_) => {
          // backtrack on error
          self.current = old_pos;
          body.push(self.parse_fn_decl()?.into())
        }
      };
    }
    self.expect_token(Token::RightCurly)?;
    self.expect_token(Token::Semicolon)?;
    Ok(Decl::ClassDecl { name, body })
  }

  fn parse_fn_decl(&mut self) -> Result<FnDecl, ParseError> {
    let name = self.parse_identifier()?;
    self.expect_token(Token::Colon)?;
    self.expect_token(Token::LeftParen)?;
    let params = if self.current_token().is_some_and(|t| t.token != Token::RightParen) {
      self.parse_params()?
    } else {
      Vec::new()
    };
    self.expect_token(Token::RightParen)?;
    self.expect_token(Token::RightArrow)?;
    let ret_type = self.parse_type()?;
    self.expect_token(Token::LeftCurly)?;
    let body = self.parse_stmts()?;
    self.expect_token(Token::RightCurly)?;
    Ok(FnDecl {
      name,
      params,
      ret_type,
      body,
    })
  }

  fn parse_params(&mut self) -> Result<Vec<Param>, ParseError> {
    let mut params = Vec::new();
    params.push(self.parse_param()?);
    while self.current_token().is_some_and(|t| t.token == Token::Comma) {
      self.advance();
      params.push(self.parse_param()?);
    }
    Ok(params)
  }

  fn parse_param(&mut self) -> Result<Param, ParseError> {
    let name = self.parse_identifier()?;
    self.expect_token(Token::Colon)?;
    let param_type = self.parse_type()?;
    Ok(Param { name, param_type })
  }

  fn parse_type(&mut self) -> Result<Type, ParseError> {
    let mut mutable = true;
    let mut is_ref = false;

    // consume the immutable if there is one
    if self.current_token().is_some_and(|t| t.token == Token::Immutable) {
      self.advance();
      mutable = false;
    }

    // consume the ref if there is one
    if self.current_token().is_some_and(|t| t.token == Token::Ref) {
      self.advance();
      is_ref = true;
    }

    match self.current_token() {
      Some(t) => match &t.token {
        Token::Int => {
          self.advance();
          Ok(Type::Int { mutable, is_ref })
        }
        Token::Bool => {
          self.advance();
          Ok(Type::Bool { mutable, is_ref })
        }
        Token::Void => {
          self.advance();
          Ok(Type::Void { mutable, is_ref })
        }
        Token::Identifier(name) => {
          let name = name.clone();
          self.advance();
          Ok(Type::Custom { name, mutable, is_ref })
        }
        _ => Err(ParseError::UnexpectedToken {
          received: self.current_token().cloned(),
        }),
      },
      _ => Err(ParseError::EOFError),
    }
  }

  fn parse_stmts(&mut self) -> Result<Block, ParseError> {
    let mut stmts = Vec::new();
    while self.current_token().is_some_and(|t| t.token != Token::RightCurly) {
      stmts.push(self.parse_stmt()?);
    }
    Ok(stmts)
  }

  fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
    match self.current_token().map(|t| t.token.clone()) {
      Some(Token::Identifier(_)) => {
        let before = self.current;

        // attempt to parse decl otherwise backtrack
        match self.parse_var_decl() {
          Ok(r) => return Ok(r.into()),
          Err(_) => {
            self.current = before;
          }
        };

        let loc = self.parse_loc(None)?;

        match self.current_token().map(|t| t.token.clone()) {
          Some(Token::Assign) => {
            self.advance();
            let expr = self.parse_expr()?;
            self.expect_token(Token::Semicolon)?;
            Ok(Stmt::Assign {
              target: loc.into(),
              value: expr,
            })
          }
          Some(Token::Increment) => {
            self.advance();
            self.expect_token(Token::Semicolon)?;
            let loc_expr: Expr = loc.into();
            let boxed_loc = Box::new(loc_expr.clone());
            Ok(Stmt::Assign {
              target: loc_expr,
              value: Expr::BinaryOp {
                left: boxed_loc,
                op: BinaryOp::Add,
                right: Box::new(Into::<Literal>::into(1).into()),
              },
            })
          }
          Some(Token::Decrement) => {
            self.advance();
            self.expect_token(Token::Semicolon)?;
            let loc_expr: Expr = loc.into();
            let boxed_loc = Box::new(loc_expr.clone());
            Ok(Stmt::Assign {
              target: loc_expr,
              value: Expr::BinaryOp {
                left: boxed_loc,
                op: BinaryOp::Sub,
                right: Box::new(Into::<Literal>::into(1).into()),
              },
            })
          }
          Some(Token::Means) => {
            self.advance();
            let means = self.parse_expr()?;
            self.expect_token(Token::Otherwise)?;
            let otherwise = self.parse_expr()?;
            self.expect_token(Token::Semicolon)?;
            Ok(Stmt::MeansOtherwise {
              loc: loc,
              means,
              otherwise,
            })
          }
          Some(Token::LeftParen) => {
            // this is not DRY code, LOL! see 01ffa15f-0dc6-4acd-8879-5bf6bc88bffe
            self.advance();
            let args = if self.current_token().is_some_and(|t| t.token != Token::RightParen) {
              self.parse_arguments()?
            } else {
              Vec::new()
            };
            self.expect_token(Token::RightParen)?;
            self.expect_token(Token::Semicolon)?;
            Ok(Stmt::Call {
              function: loc,
              args: args,
            })
          }
          Some(_) => Err(ParseError::UnexpectedToken {
            received: self.current_token().cloned(),
          }),
          _ => Err(ParseError::EOFError),
        }
      }
      Some(Token::ToConsole) => {
        self.advance();
        let expr = self.parse_expr()?;
        self.expect_token(Token::Semicolon)?;
        Ok(Stmt::ToConsole(expr))
      }
      Some(Token::FromConsole) => {
        self.advance();
        let loc = self.parse_loc(None)?;
        self.expect_token(Token::Semicolon)?;
        Ok(Stmt::FromConsole(loc))
      }
      Some(Token::If) => Ok(self.parse_if()?),
      Some(Token::While) => Ok(self.parse_while()?),
      Some(Token::Return) => {
        self.advance();
        let expr = if self.current_token().is_some_and(|t| t.token != Token::Semicolon) {
          Some(self.parse_expr()?)
        } else {
          None
        };
        self.expect_token(Token::Semicolon)?;
        Ok(Stmt::Return(expr))
      }
      Some(_) => Err(ParseError::UnexpectedToken {
        received: self.current_token().cloned(),
      }),
      _ => Err(ParseError::EOFError),
    }
  }

  fn parse_expr(&mut self) -> Result<Expr, ParseError> {
    self.parse_or()
  }

  fn parse_or(&mut self) -> Result<Expr, ParseError> {
    let mut expr = self.parse_and()?;
    while let Some(TokenWithMetadata { token: Token::Or, .. }) = self.current_token() {
      self.advance();
      let right = self.parse_and()?;
      expr = Expr::BinaryOp {
        left: Box::new(expr),
        op: BinaryOp::Or,
        right: Box::new(right),
      };
    }
    Ok(expr)
  }

  fn parse_and(&mut self) -> Result<Expr, ParseError> {
    let mut expr = self.parse_equality_and_comparison()?;
    while let Some(TokenWithMetadata { token: Token::And, .. }) = self.current_token() {
      self.advance();
      let right = self.parse_equality_and_comparison()?;
      expr = Expr::BinaryOp {
        left: Box::new(expr),
        op: BinaryOp::And,
        right: Box::new(right),
      };
    }
    Ok(expr)
  }

  fn parse_equality_and_comparison(&mut self) -> Result<Expr, ParseError> {
    let mut expr = self.parse_term()?;
    let mut found = false;
    while let Some(TokenWithMetadata {
      token: Token::Equals | Token::NotEquals | Token::GreaterThan | Token::GreaterThanEqual | Token::LessThan | Token::LessThanEqual,
      ..
    }) = self.current_token()
    {
      if found {
        return Err(ParseError::NonAssociative {
          received: self.current_token().cloned(),
        });
      }
      let op = match self.current_token().unwrap().token {
        Token::Equals => BinaryOp::Equals,
        Token::NotEquals => BinaryOp::NotEquals,
        Token::GreaterThan => BinaryOp::GreaterThan,
        Token::GreaterThanEqual => BinaryOp::GreaterThanEqual,
        Token::LessThan => BinaryOp::LessThan,
        Token::LessThanEqual => BinaryOp::LessThanEqual,
        _ => unreachable!(),
      };
      found = true;
      self.advance();
      let right = self.parse_term()?;
      expr = Expr::BinaryOp {
        left: Box::new(expr),
        op,
        right: Box::new(right),
      };
    }
    Ok(expr)
  }

  fn parse_term(&mut self) -> Result<Expr, ParseError> {
    let mut expr = self.parse_factor()?;
    while let Some(TokenWithMetadata {
      token: Token::Cross | Token::Dash,
      ..
    }) = self.current_token()
    {
      let op = match self.current_token().unwrap().token {
        Token::Cross => BinaryOp::Add,
        Token::Dash => BinaryOp::Sub,
        _ => unreachable!(),
      };
      self.advance();
      let right = self.parse_factor()?;
      expr = Expr::BinaryOp {
        left: Box::new(expr),
        op,
        right: Box::new(right),
      };
    }
    Ok(expr)
  }

  fn parse_factor(&mut self) -> Result<Expr, ParseError> {
    let mut expr = self.parse_unary()?;
    while let Some(TokenWithMetadata {
      token: Token::Star | Token::Slash,
      ..
    }) = self.current_token()
    {
      let op = match self.current_token().unwrap().token {
        Token::Star => BinaryOp::Mul,
        Token::Slash => BinaryOp::Div,
        _ => unreachable!(),
      };
      self.advance();
      let right = self.parse_unary()?;
      expr = Expr::BinaryOp {
        left: Box::new(expr),
        op,
        right: Box::new(right),
      };
    }
    Ok(expr)
  }

  fn parse_unary(&mut self) -> Result<Expr, ParseError> {
    if let Some(TokenWithMetadata {
      token: Token::Not | Token::Dash,
      ..
    }) = self.current_token()
    {
      let op = match self.current_token().unwrap().token {
        Token::Not => UnaryOp::Not,
        Token::Dash => UnaryOp::Negate,
        _ => unreachable!(),
      };
      self.advance();
      let expr = self.parse_unary()?;
      return Ok(Expr::UnaryOp {
        op,
        expr: Box::new(expr),
      });
    }
    self.parse_primary()
  }

  fn parse_primary(&mut self) -> Result<Expr, ParseError> {
    match self.current_token() {
      Some(t) => match t.token {
        Token::IntLiteral(value) => {
          let value = value;
          self.advance();
          Ok(Expr::Literal(Literal::Int(value)))
        }
        Token::True => {
          self.advance();
          Ok(Expr::Literal(Literal::Bool(true)))
        }
        Token::False => {
          self.advance();
          Ok(Expr::Literal(Literal::Bool(false)))
        }
        Token::StringLiteral(ref value) => {
          let value = value.clone();
          self.advance();
          Ok(Expr::Literal(Literal::String(value)))
        }
        Token::Eh => {
          self.advance();
          Ok(Expr::Literal(Literal::None))
        }
        Token::Identifier(_) => {
          let loc = self.parse_loc(None)?;
          if self.current_token().is_some_and(|t| t.token == Token::LeftParen) {
            // 01ffa15f-0dc6-4acd-8879-5bf6bc88bffe
            self.advance();
            let args = if self.current_token().is_some_and(|t| t.token != Token::RightParen) {
              self.parse_arguments()?
            } else {
              Vec::new()
            };
            self.expect_token(Token::RightParen)?;
            Ok(Expr::Call { function: loc, args })
          } else {
            Ok(Expr::Var(loc))
          }
        }
        Token::LeftParen => {
          self.advance();
          let expr = self.parse_expr()?;
          self.expect_token(Token::RightParen)?;
          Ok(expr)
        }
        _ => Err(ParseError::UnexpectedToken {
          received: self.current_token().cloned(),
        }),
      },
      _ => Err(ParseError::EOFError),
    }
  }

  fn parse_arguments(&mut self) -> Result<Vec<Expr>, ParseError> {
    let mut args = Vec::new();
    args.push(self.parse_expr()?);

    while self.current_token().is_some_and(|t| t.token == Token::Comma) {
      self.advance();
      args.push(self.parse_expr()?);
    }

    Ok(args)
  }

  fn parse_identifier(&mut self) -> Result<String, ParseError> {
    if let Some(TokenWithMetadata {
      token: Token::Identifier(ref name),
      ..
    }) = self.current_token()
    {
      let name = name.clone();
      self.advance();
      Ok(name)
    } else {
      Err(ParseError::ExpectedIdentifier {
        received: self.current_token().cloned(),
      })
    }
  }

  fn parse_loc(&mut self, parent: Option<Box<Var>>) -> Result<Var, ParseError> {
    let mut name = self.parse_identifier()?;
    let mut parent = parent;

    // hmm recursive struct calls for recursive impl
    while let Some(TokenWithMetadata {
      token: Token::RightArrow,
      ..
    }) = self.current_token()
    {
      self.advance();
      parent = Some(Box::new(Var { name, parent }));
      name = self.parse_identifier()?;
    }

    Ok(Var { name, parent })
  }

  fn parse_while(&mut self) -> Result<Stmt, ParseError> {
    self.expect_token(Token::While)?;
    self.expect_token(Token::LeftParen)?;
    let expr = self.parse_expr()?;
    self.expect_token(Token::RightParen)?;
    self.expect_token(Token::LeftCurly)?;
    let stmts = self.parse_stmts()?;
    self.expect_token(Token::RightCurly)?;

    Ok(Stmt::While {
      cond: expr,
      body: stmts,
    })
  }

  fn parse_if(&mut self) -> Result<Stmt, ParseError> {
    self.expect_token(Token::If)?;
    self.expect_token(Token::LeftParen)?;
    let expr = self.parse_expr()?;
    self.expect_token(Token::RightParen)?;
    self.expect_token(Token::LeftCurly)?;
    let stmts = self.parse_stmts()?;
    self.expect_token(Token::RightCurly)?;

    if self.current_token().is_some_and(|t| t.token == Token::Else) {
      self.advance();
      self.expect_token(Token::LeftCurly)?;
      let else_stmts = self.parse_stmts()?;
      self.expect_token(Token::RightCurly)?;

      return Ok(Stmt::If {
        cond: expr,
        then_block: stmts,
        else_block: Some(else_stmts),
      });
    }

    Ok(Stmt::If {
      cond: expr,
      then_block: stmts,
      else_block: None,
    })
  }
}

#[derive(Debug, PartialEq)]
pub(crate) enum ParseError {
  ExpectedSemicolon {
    received: Option<TokenWithMetadata>,
  },
  ExpectedIdentifier {
    received: Option<TokenWithMetadata>,
  },
  ExpectedToken {
    expected: Token,
    received: Option<TokenWithMetadata>,
  },
  UnexpectedToken {
    received: Option<TokenWithMetadata>,
  },
  NonAssociative {
    received: Option<TokenWithMetadata>,
  },
  EOFError,
}

#[cfg(test)]
mod tests {
  use super::*;
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
    let program = parser.parse_program();

    println!("Program: {:?}", program);

    assert_eq!(true, program.is_ok());

    let mut asts = program.unwrap();

    #[rustfmt::skip]
    let shared_decls: Vec<VarDecl> = vec![
      VarDecl{name: "id".to_string(), var_type: Type::Int {mutable: true, is_ref: false}, init: None},
      VarDecl{name: "id".to_string(), var_type: Type::Int {mutable: true, is_ref: true}, init: None},
      VarDecl{name: "id".to_string(), var_type: Type::Int {mutable: false, is_ref: false}, init: None},
      VarDecl{name: "id".to_string(), var_type: Type::Int {mutable: false, is_ref: true}, init: None},
      VarDecl{name: "id".to_string(), var_type: Type::Bool {mutable: true, is_ref: false}, init: None},
      VarDecl{name: "id".to_string(), var_type: Type::Bool {mutable: true, is_ref: true}, init: None},
      VarDecl{name: "id".to_string(), var_type: Type::Bool {mutable: false, is_ref: false}, init: None},
      VarDecl{name: "id".to_string(), var_type: Type::Bool {mutable: false, is_ref: true}, init: None},
      VarDecl{name: "id".to_string(), var_type: Type::Void {mutable: true, is_ref: false}, init: None},
      VarDecl{name: "id".to_string(), var_type: Type::Void {mutable: true, is_ref: true}, init: None},
      VarDecl{name: "id".to_string(), var_type: Type::Void {mutable: false, is_ref: false}, init: None},
      VarDecl{name: "id".to_string(), var_type: Type::Void {mutable: false, is_ref: true}, init: None},
      VarDecl{name: "id".to_string(), var_type: Type::Custom {name: "custom_type".to_string(), mutable: true, is_ref: false}, init: None},
      VarDecl{name: "id".to_string(), var_type: Type::Custom {name: "custom_type".to_string(), mutable: true, is_ref: true}, init: None},
      VarDecl{name: "id".to_string(), var_type: Type::Custom {name: "custom_type".to_string(), mutable: false, is_ref: false}, init: None},
      VarDecl{name: "id".to_string(), var_type: Type::Custom {name: "custom_type".to_string(), mutable: false, is_ref: true}, init: None},
    ];

    // lol
    let shared_expr = Expr::BinaryOp {
      left: Box::new(Expr::BinaryOp {
        left: Box::new(Expr::BinaryOp {
          left: Box::new(Expr::BinaryOp {
            left: Box::new(Expr::BinaryOp {
              left: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::BinaryOp {
                  left: Box::new(Expr::BinaryOp {
                    left: Box::new(Expr::UnaryOp {
                      op: UnaryOp::Not,
                      expr: Box::new(Expr::Var(Var {
                        name: "foo".to_string(),
                        parent: None,
                      })),
                    }),
                    op: BinaryOp::Add,
                    right: Box::new(Expr::Var(Var {
                      name: "baz".to_string(),
                      parent: Some(Box::new(Var {
                        name: "bar".to_string(),
                        parent: None,
                      })),
                    })),
                  }),
                  op: BinaryOp::Sub,
                  right: Box::new(Expr::BinaryOp {
                    left: Box::new(Expr::BinaryOp {
                      left: Box::new(Expr::UnaryOp {
                        op: UnaryOp::Negate,
                        expr: Box::new(Expr::Literal(Literal::Int(1))),
                      }),
                      op: BinaryOp::Mul,
                      right: Box::new(Expr::Var(Var {
                        name: "hi".to_string(),
                        parent: None,
                      })),
                    }),
                    op: BinaryOp::Div,
                    right: Box::new(Expr::Literal(Literal::Bool(true))),
                  }),
                }),
                op: BinaryOp::And,
                right: Box::new(Expr::Literal(Literal::Bool(false))),
              }),
              op: BinaryOp::Or,
              right: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::Literal(Literal::None)),
                op: BinaryOp::GreaterThanEqual,
                right: Box::new(Expr::Literal(Literal::None)),
              }),
            }),
            op: BinaryOp::Or,
            right: Box::new(Expr::BinaryOp {
              left: Box::new(Expr::Literal(Literal::String("string".to_string()))),
              op: BinaryOp::LessThanEqual,
              right: Box::new(Expr::Literal(Literal::None)),
            }),
          }),
          op: BinaryOp::Or,
          right: Box::new(Expr::BinaryOp {
            left: Box::new(Expr::Literal(Literal::None)),
            op: BinaryOp::Equals,
            right: Box::new(Expr::Literal(Literal::None)),
          }),
        }),
        op: BinaryOp::Or,
        right: Box::new(Expr::BinaryOp {
          left: Box::new(Expr::Literal(Literal::None)),
          op: BinaryOp::GreaterThan,
          right: Box::new(Expr::Literal(Literal::None)),
        }),
      }),
      op: BinaryOp::Or,
      right: Box::new(Expr::BinaryOp {
        left: Box::new(Expr::Literal(Literal::None)),
        op: BinaryOp::LessThan,
        right: Box::new(Expr::Literal(Literal::None)),
      }),
    };

    #[rustfmt::skip]
    let shared_fns: Vec<Decl> = vec![
      FnDecl{name: "empty_function".to_string(), params: vec![], body: vec![], ret_type: Type::Int {mutable: true, is_ref: false}}.into(),
      FnDecl{name: "empty_function".to_string(), params: vec![], body: vec![], ret_type: Type::Int {mutable: true, is_ref: true}}.into(),
      FnDecl{name: "empty_function".to_string(), params: vec![], body: vec![], ret_type: Type::Int {mutable: false, is_ref: false}}.into(),
      FnDecl{name: "empty_function".to_string(), params: vec![], body: vec![], ret_type: Type::Int {mutable: false, is_ref: true}}.into(),
      FnDecl{name: "empty_function".to_string(), params: vec![], body: vec![], ret_type: Type::Bool {mutable: true, is_ref: false}}.into(),
      FnDecl{name: "empty_function".to_string(), params: vec![], body: vec![], ret_type: Type::Bool {mutable: true, is_ref: true}}.into(),
      FnDecl{name: "empty_function".to_string(), params: vec![], body: vec![], ret_type: Type::Bool {mutable: false, is_ref: false}}.into(),
      FnDecl{name: "empty_function".to_string(), params: vec![], body: vec![], ret_type: Type::Bool {mutable: false, is_ref: true}}.into(),
      FnDecl{name: "empty_function".to_string(), params: vec![], body: vec![], ret_type: Type::Void {mutable: true, is_ref: false}}.into(),
      FnDecl{name: "empty_function".to_string(), params: vec![], body: vec![], ret_type: Type::Void {mutable: true, is_ref: true}}.into(),
      FnDecl{name: "empty_function".to_string(), params: vec![], body: vec![], ret_type: Type::Void {mutable: false, is_ref: false}}.into(),
      FnDecl{name: "empty_function".to_string(), params: vec![], body: vec![], ret_type: Type::Void {mutable: false, is_ref: true}}.into(),
      FnDecl{name: "empty_function".to_string(), params: vec![], body: vec![], ret_type: Type::Custom {name: "custom_type".to_string(), mutable: true, is_ref: false}}.into(),
      FnDecl{name: "empty_function".to_string(), params: vec![], body: vec![], ret_type: Type::Custom {name: "custom_type".to_string(), mutable: true, is_ref: true}}.into(),
      FnDecl{name: "empty_function".to_string(), params: vec![], body: vec![], ret_type: Type::Custom {name: "custom_type".to_string(), mutable: false, is_ref: false}}.into(),
      FnDecl{name: "empty_function".to_string(), params: vec![], body: vec![], ret_type: Type::Custom {name: "custom_type".to_string(), mutable: false, is_ref: true}}.into(),

      FnDecl{name: "my_function".to_string(), params: vec![
        Param{name: "id".to_string(), param_type: Type::Int {mutable: true, is_ref: false}},
        Param{name: "id".to_string(), param_type: Type::Int {mutable: true, is_ref: true}},
        Param{name: "id".to_string(), param_type: Type::Int {mutable: false, is_ref: false}},
        Param{name: "id".to_string(), param_type: Type::Int {mutable: false, is_ref: true}},
        Param{name: "id".to_string(), param_type: Type::Bool {mutable: true, is_ref: false}},
        Param{name: "id".to_string(), param_type: Type::Bool {mutable: true, is_ref: true}},
        Param{name: "id".to_string(), param_type: Type::Bool {mutable: false, is_ref: false}},
        Param{name: "id".to_string(), param_type: Type::Bool {mutable: false, is_ref: true}},
        Param{name: "id".to_string(), param_type: Type::Void {mutable: true, is_ref: false}},
        Param{name: "id".to_string(), param_type: Type::Void {mutable: true, is_ref: true}},
        Param{name: "id".to_string(), param_type: Type::Void {mutable: false, is_ref: false}},
        Param{name: "id".to_string(), param_type: Type::Void {mutable: false, is_ref: true}},
        Param{name: "id".to_string(), param_type: Type::Custom {name: "custom_type".to_string(), mutable: true, is_ref: false}},
        Param{name: "id".to_string(), param_type: Type::Custom {name: "custom_type".to_string(), mutable: true, is_ref: true}},
        Param{name: "id".to_string(), param_type: Type::Custom {name: "custom_type".to_string(), mutable: false, is_ref: false}},
        Param{name: "id".to_string(), param_type: Type::Custom {name: "custom_type".to_string(), mutable: false, is_ref: true}},
      ], body: shared_decls.clone().into_iter().map(Into::into).chain(vec![
        Stmt::Call { function: Var{name: "call_me".to_string(), parent: None}, args: vec![]},
        Stmt::Call { function: Var{name: "me".to_string(), parent: Some(Box::new(Var{name: "call".to_string(), parent: None}))}, args: vec![
          Expr::Var(Var{name: "foo".to_string(), parent: None}),
          Expr::Var(Var{name: "baz".to_string(), parent: Some(Box::new(Var{name: "bar".to_string(), parent: None}))}),
          shared_expr.clone()
        ] },
        Stmt::Call { function: Var{name: "test_expr_nest".to_string(), parent: None}, args: vec![
          Expr::Call { function: Var{name: "test_expr_nest".to_string(), parent: None}, args: vec![
            Expr::Call { function: Var{name: "test_expr_nest".to_string(), parent: None}, args: vec![] }
          ]}
        ]},
        Stmt::If { cond: Var{name: "expr".to_string(), parent: None}.into(), then_block: vec![], else_block: None },
        Stmt::If { cond: Var{name: "expr".to_string(), parent: None}.into(), then_block: vec![], else_block: Some(vec![]) },
        Stmt::While { cond: Var{name: "expr".to_string(), parent: None}.into(), body: vec![] },
        Stmt::Assign { target: Var{name: "foo".to_string(), parent: None}.into(), value: Var{name: "foo".to_string(), parent: None}.into() },
        Stmt::Assign { target: Var{name: "foo".to_string(), parent: None}.into(), value: Expr::Literal(Literal::Int(1)).into() },
        Stmt::Assign { target: Var{name: "foo".to_string(), parent: None}.into(), value: Expr::Literal(Literal::Bool(true)).into() },
        Stmt::Assign { target: Var{name: "foo".to_string(), parent: None}.into(), value: Expr::Literal(Literal::Bool(false)).into() },
        Stmt::Assign { target: Var{name: "foo".to_string(), parent: None}.into(), value: Expr::Literal(Literal::None).into() },
        Stmt::Assign { target: Var{name: "foo".to_string(), parent: None}.into(), value: Expr::BinaryOp { left: Box::new(Var{name: "foo".to_string(), parent: None}.into()), op: BinaryOp::Add, right: Box::new(Expr::Literal(Literal::Int(1))) } },
        Stmt::Assign { target: Var{name: "foo".to_string(), parent: None}.into(), value: Expr::BinaryOp { left: Box::new(Var{name: "foo".to_string(), parent: None}.into()), op: BinaryOp::Sub, right: Box::new(Expr::Literal(Literal::Int(1))) } },
        Stmt::MeansOtherwise { loc: Var{name: "foo".to_string(), parent: None}, means: Var{name: "business".to_string(), parent: None}.into(), otherwise: shared_expr.clone() },
        Stmt::ToConsole(shared_expr.clone()),
        Stmt::FromConsole(Var{name: "foo".to_string(), parent: None}),
        Stmt::Return(None),
        Stmt::Return(Some(shared_expr.clone())),
      ]).collect(), ret_type: Type::Void {mutable: true, is_ref: false}}.into()
    ];

    assert_eq!(
      shared_decls
        .clone()
        .into_iter()
        .map(Into::<Decl>::into)
        .collect::<Vec<Decl>>(),
      asts.0.drain(..16).collect::<Vec<Decl>>()
    );
    assert_eq!(
      shared_fns.iter().take(16).cloned().collect::<Vec<Decl>>(),
      asts.0.drain(..16).collect::<Vec<Decl>>()
    );
    assert_eq!(shared_fns[16], asts.0.drain(..1).collect::<Vec<Decl>>()[0]);

    #[rustfmt::skip]
    let class_joined: Vec<Decl> = shared_decls.clone().into_iter().map(Into::<Decl>::into).chain(shared_fns).collect();
    let class: Decl = Decl::ClassDecl {
      name: "my_class".to_string(),
      body: class_joined,
    };

    assert_eq!(class, asts.0.drain(..1).collect::<Vec<Decl>>()[0]);
  }
}
