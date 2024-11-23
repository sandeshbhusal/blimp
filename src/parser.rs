#![allow(dead_code)]

use crate::ast;
use crate::ast::Expr;
use crate::ast::Param;
use crate::ast::ParamList;
use crate::ast::TypeDecl;
use crate::lexer::Token;
use crate::lexer::TokenType;

#[derive(Debug, thiserror::Error)]
pub enum ParserError {
    #[error("Empty Token stream.")]
    EmptyTokenStream,

    #[error("Expected {expected:?}, but found {found:?}")]
    UnexpectedToken { expected: TokenType, found: Token },

    #[error("Error casting {} to i32", .0)]
    I32ParseError(std::num::ParseIntError),

    #[error("Error casting {} to f32", .0)]
    F32ParseError(std::num::ParseFloatError),

    #[error("{:?} is not an operator token at {}", .0.r#type, .0.start)]
    NotAnOperator(Token),

    #[error("{:?} does not outline an expression prefix", .0)]
    NotAnExpressionPrefix(Token),
}

/// Returns the precedence, and whether the operator is left associative
fn get_precedence(token: Token) -> Result<(i32, bool), ParserError> {
    match token.r#type {
        TokenType::Plus => Ok((10, true)),
        TokenType::Minus => Ok((10, true)),
        TokenType::Multiply => Ok((20, true)),
        TokenType::Divide => Ok((20, true)),
        TokenType::Mod => Ok((20, true)),
        TokenType::Lt => Ok((30, true)),
        TokenType::Gt => Ok((30, true)),
        TokenType::Le => Ok((30, true)),
        TokenType::Ge => Ok((30, true)),
        TokenType::Ne => Ok((30, true)),
        TokenType::Eq => Ok((30, true)),
        TokenType::And => Ok((40, true)),
        TokenType::Or => Ok((50, true)),

        // Right-associative operators.
        TokenType::Not => Ok((60, false)),
        TokenType::Assign => Ok((0, false)),

        _ => Err(ParserError::NotAnOperator(token)),
    }
}

impl TryFrom<Token> for ast::Op {
    type Error = ParserError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        let op = match value.r#type {
            TokenType::Plus => Ok(ast::Op::Plus),
            TokenType::Minus => Ok(ast::Op::Minus),
            TokenType::Multiply => Ok(ast::Op::Multiply),
            TokenType::Divide => Ok(ast::Op::Divide),
            TokenType::Mod => Ok(ast::Op::Mod),
            TokenType::Lt => Ok(ast::Op::Lt),
            TokenType::Gt => Ok(ast::Op::Gt),
            TokenType::Le => Ok(ast::Op::Le),
            TokenType::Ge => Ok(ast::Op::Ge),
            TokenType::Ne => Ok(ast::Op::Ne),
            TokenType::Eq => Ok(ast::Op::Eq),
            TokenType::And => Ok(ast::Op::And),
            TokenType::Or => Ok(ast::Op::Or),
            TokenType::Not => Ok(ast::Op::Not),
            TokenType::Assign => Ok(ast::Op::Assign),

            _ => Err(ParserError::NotAnOperator(value)),
        };
        op
    }
}

pub(crate) struct Parser<'a> {
    pub(crate) input_stream: &'a [Token],
}

impl<'a> Parser<'a> {
    pub fn new(input_stream: &'a [Token]) -> Self {
        Self {
            input_stream,
        }
    }

    fn expect(&mut self, token_type: TokenType) -> Result<Token, ParserError> {
        let token = self
            .input_stream
            .first()
            .ok_or(ParserError::EmptyTokenStream)?;

        if token.r#type == token_type {
            self.advance();
            Ok(token.clone())
        } else {
            Err(ParserError::UnexpectedToken {
                expected: token_type,
                found: token.clone(),
            })
        }
    }

    fn advance(&mut self) {
        self.input_stream = &self.input_stream[1..];
    }

    fn parse_identifier(&mut self) -> Result<Expr, ParserError> {
        let token = self.expect(TokenType::Identifier)?;
        Ok(Expr::Identifier(token.content.clone()))
    }

    fn parse_i32(&mut self) -> Result<Expr, ParserError> {
        let token = self.expect(TokenType::Number)?;
        let i32_val = token
            .content
            .parse::<i32>()
            .map_err(ParserError::I32ParseError)?;
        Ok(Expr::I32(i32_val))
    }

    fn parse_f32(&mut self) -> Result<Expr, ParserError> {
        let exponent = self.expect(TokenType::Number)?;
        self.expect(TokenType::Dot)?;
        let mantissa = self.expect(TokenType::Number)?;

        let f32_val = format!("{}.{}", exponent.content, mantissa.content);
        let f32_val = f32_val.parse::<f32>().map_err(ParserError::F32ParseError)?;

        Ok(Expr::F32(f32_val))
    }

    fn parse_func_call(&mut self) -> Result<Expr, ParserError> {
        let func_name = self.expect(TokenType::Identifier)?.content.clone();
        self.expect(TokenType::Lparen)?;

        let mut params = Vec::new();
        while self.input_stream.first().map(|t| t.r#type) != Some(TokenType::Rparen) {
            params.push(self.parse_expr()?);
            if self.input_stream.first().map(|t| t.r#type) != Some(TokenType::Comma) {
                break;
            }
            self.expect(TokenType::Comma)?;
        }

        self.expect(TokenType::Rparen)?;

        Ok(Expr::FuncCall { func_name, params })
    }

    fn oprec_parser(&mut self, precedence: i32) -> Result<Expr, ParserError> {
        // Parse the prefix. Which it itself an expression.
        // parse the operator while it exists.
        // if operator precedence is > precedence, parse the other one, and generate expr, return.
        // if less, break out of loop and return prefix, after putting back the operator into the queue.

        let first_token = self.input_stream.first();
        let mut prefix: Expr = match first_token.map(|t| t.r#type) {
            Some(TokenType::Lparen) => {
                // Parenthesized expression.
                // Parse the "(" and then parse the expression inside it.
                self.expect(TokenType::Lparen)?;
                let expr = self.parse_expr()?;
                self.expect(TokenType::Rparen)?;
                expr
            }

            Some(TokenType::Number) => {
                // Could be int or float.
                // Peek the next token, if it is a "Dot", then parse a float.
                // Otherwise, parse an int.
                let next_token = self.input_stream.get(1).map(|t| t.r#type);
                match next_token {
                    Some(TokenType::Dot) => self.parse_f32()?,
                    _ => self.parse_i32()?,
                }
            }

            Some(TokenType::Identifier) => {
                // Could be variable name, or func call.
                // Peek the next token. if it is a "Lparen", then parse a func call.
                // Otherwise, parse an identifier.
                let next_token = self.input_stream.get(1).map(|t| t.r#type);
                match next_token {
                    Some(TokenType::Lparen) => self.parse_func_call()?,
                    _ => self.parse_identifier()?,
                }
            }

            Some(TokenType::Not) => {
                // Unary negation.
                // Parse the "!" and then parse the expression after it.
                self.expect(TokenType::Not)?;
                let operand = self.parse_expr()?;
                Expr::Unary {
                    op: ast::Op::Not,
                    operand: Box::new(operand),
                }
            }

            Some(TokenType::Minus) => {
                // Unary negative.
                // Parse the "-" and then parse the expression after it.
                self.expect(TokenType::Minus)?;
                let operand = self.parse_expr()?;
                Expr::Unary {
                    op: ast::Op::Minus,
                    operand: Box::new(operand),
                }
            }

            Some(tok) => {
                return Err(ParserError::NotAnExpressionPrefix(
                    first_token.unwrap().clone(),
                ));
            }

            None => {
                return Err(ParserError::EmptyTokenStream);
            }
        };

        while let Some(op) = self.input_stream.first() {
            if op.r#type == TokenType::Comma || op.r#type == TokenType::Rparen {
                break;
            }

            if let Ok((op_precedence, _is_left_associative)) = get_precedence(op.clone()) {
                if op_precedence >= precedence {
                    self.advance();
                    let remaining = self.oprec_parser(op_precedence)?;
                    // TODO: Use left-associativity here.
                    let generated_expr = Expr::Binary {
                        lhs: Box::new(prefix),
                        rhs: Box::new(remaining),
                        op: op.clone().try_into()?,
                    };
                    prefix = generated_expr;
                } else {
                    break;
                }
            } else {
                break;
                // return Err(ParserError::NotAnOperator(op.clone()));
            }
        }

        return Ok(prefix);
    }

    pub fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        self.oprec_parser(0)
    }

    //// ------------------ End of parsing expressions ------------------ ////

    fn parse_param_list(&mut self) -> Result<ParamList, ParserError> {
        self.expect(TokenType::Lparen)?;

        let mut params = Vec::new();

        while self.input_stream.first().map(|t| t.r#type) != Some(TokenType::Rparen) {
            let param_name = self.expect(TokenType::Identifier)?.content.clone();
            self.expect(TokenType::Colon)?;
            let param_type = self.parse_type()?;

            params.push(Param {
                var_name: param_name,
                var_type: param_type,
            });

            if self.input_stream.first().map(|t| t.r#type) != Some(TokenType::Comma) {
                break;
            }
            self.expect(TokenType::Comma)?;
        }

        self.expect(TokenType::Rparen)?;

        Ok(ParamList { params })
    }

    fn parse_type(&mut self) -> Result<TypeDecl, ParserError> {
        let token = self.expect(TokenType::Identifier)?;
        let type_decl = match token.content.as_str() {
            "i32" => TypeDecl::I32,
            "f32" => TypeDecl::F32,
            "bool" => TypeDecl::Bool,
            "vec" => {
                self.expect(TokenType::Lt)?;
                let inner_type = Box::new(self.parse_type()?);
                self.expect(TokenType::Gt)?;
                TypeDecl::Vec(inner_type)
            }
            "fn" => {
                let param_list = Box::new(self.parse_param_list()?);
                self.expect(TokenType::Arrow)?;
                let return_type = Box::new(self.parse_type()?);
                TypeDecl::Func(param_list, return_type)
            }
            "nil" => TypeDecl::Nil,

            string => TypeDecl::UDT(string.to_string()),
        };
        Ok(type_decl)
    }

    fn parse_variable_decl(&mut self) -> Result<ast::VariableDeclaration, ParserError> {
        let var_name = self.expect(TokenType::Identifier)?.content.clone();
        self.expect(TokenType::Colon)?;
        let var_type = self.parse_type()?;
        Ok(ast::VariableDeclaration { var_name, var_type })
    }

    fn parse_block(&mut self) -> Result<ast::Stmt, ParserError> {
        self.expect(TokenType::Lbrace)?;
        let mut statements = Vec::new();
        while self.input_stream.first().map(|t| t.r#type) != Some(TokenType::Rbrace) {
            statements.push(self.parse_statement()?);
        }
        self.expect(TokenType::Rbrace)?;
        Ok(ast::Stmt::Block(statements))
    }

    fn parse_if_statement(&mut self) -> Result<ast::Stmt, ParserError> {
        self.expect(TokenType::KwIf)?;
        let condition = self.parse_expr()?;
        let then_block = Box::new(self.parse_block()?);
        let else_block = if self.input_stream.first().map(|t| t.r#type) == Some(TokenType::KwElse) {
            self.advance();
            Some(Box::new(self.parse_block()?))
        } else {
            None
        };
        Ok(ast::Stmt::IfStatement {
            cond: condition,
            true_branch: then_block,
            false_branch: else_block,
        })
    }

    fn parse_while_statement(&mut self) -> Result<ast::Stmt, ParserError> {
        self.expect(TokenType::KwWhile)?;
        let condition = self.parse_expr()?;
        let body = Box::new(self.parse_block()?);
        Ok(ast::Stmt::WhileStatement {
            cond: Box::new(condition),
            body,
        })
    }

    fn parse_return_statement(&mut self) -> Result<ast::Stmt, ParserError> {
        self.expect(TokenType::KwReturn)?;
        let expr = self.parse_expr()?;
        self.expect(TokenType::SemiColon)?;
        Ok(ast::Stmt::ReturnStatement(expr))
    }

    fn parse_let_statement(&mut self) -> Result<ast::Stmt, ParserError> {
        self.expect(TokenType::KwLet)?;
        let decl = self.parse_variable_decl()?;
        self.expect(TokenType::Assign)?;

        match decl.var_type {
            TypeDecl::Func(param_list, type_decl) => {
                let block = self.parse_block()?;
                return Ok(ast::Stmt::FuncDecl {
                    func_name: decl.var_name,
                    params: *param_list,
                    return_type: *type_decl,
                    body: Box::new(block),
                });
            }
            _ => {
                let expr = self.parse_expr()?;
                self.expect(TokenType::SemiColon)?;
                return Ok(ast::Stmt::LetStatement { decl, init: expr });
            }
        }
    }

    fn parse_set_statement(&mut self) -> Result<ast::Stmt, ParserError> {
        let var_name = self.expect(TokenType::Identifier)?.content.clone();
        self.expect(TokenType::Assign)?;
        let expr = self.parse_expr()?;
        self.expect(TokenType::SemiColon)?;
        Ok(ast::Stmt::SetStatement {
            lvalue: var_name,
            rvalue: expr,
        })
    }

    pub fn parse_statement(&mut self) -> Result<ast::Stmt, ParserError> {
        if let Some(keyword) = self.input_stream.first() {
            let token = keyword.content.clone();

            let stmt = match token.as_str() {
                "let" => self.parse_let_statement()?,
                "if" => self.parse_if_statement()?,
                "while" => self.parse_while_statement()?,
                "return" => self.parse_return_statement()?,
                _ => self.parse_set_statement()?,
            };
            Ok(stmt)
        } else {
            Err(ParserError::EmptyTokenStream)
        }
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use crate::lexer::lexer;

    #[test]
    fn test_parse_i32() {
        let tokens = lexer("123 * 456 + func(x1, x3)").unwrap();
        let mut parser = Parser {
            input_stream: &tokens,
        };
        // dbg!(parser.parse_expr().unwrap());
    }

    #[test]
    fn generate_ast() {
        let program = r#"
        let add_simd: fn(x: vec<i32>, y: vec<i32>) -> nil = {
            let result: vec<i32> = simdadd(x, y);
        }
        "#;

        let tokens = lexer(program).expect("Lexer SHOULD work.");
        let mut parser = Parser {
            input_stream: &tokens,
        };
        match parser.parse_statement() {
            Ok(_stmt) => {
                // dbg!(_stmt);
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[test]
    fn comprehensive_test() {
        let program = r#"
        let gcd: fn(x: i32, y: i32) -> i32 = {
            if y == 0 {
                return x;
            } else {
                let temp: int = y;
                y = y % x;
                x = temp;

                let add_simd: fn(x: vec<i32>, y: vec<i32>) -> nil =  {
                    let result: vec<i32> = simdadd(x, y);
                } 

                while (x > 0) {
                    x = x - 1;
                    x = x + 1;
                    x = x % 1;
                    x = x * 1;
                    x = x / 1;
                }

                return gcd(x, y);
            }
        }
        "#;

        let tokens = lexer(program).expect("Lexer SHOULD work.");
        let mut parser = Parser {
            input_stream: &tokens,
        };
        match parser.parse_statement() {
            Ok(_stmt) => {

            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }
}
