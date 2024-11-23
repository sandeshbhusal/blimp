use std::collections::HashMap;

use crate::ast::{Expr, Stmt, TypeDecl};
use std::mem::discriminant;

#[derive(Debug, Default)]
pub struct Environment {
    pub(crate) variable_types: HashMap<String, TypeDecl>,
    pub(crate) function_types: HashMap<String, TypeDecl>,
    pub(crate) function_params: HashMap<String, Vec<TypeDecl>>,
}

#[derive(Debug, Default)]
pub struct TypeCheckInfo {
    pub(crate) contexts: Vec<Environment>,
}

impl TypeCheckInfo {
    fn new() -> Self {
        let init_env = Environment::default();
        TypeCheckInfo {
            contexts: vec![init_env],
        }
    }

    fn typecheck_expr(&self, expr: &Expr) -> TypeDecl {
        match expr {
            Expr::I32(_) => TypeDecl::I32,
            Expr::F32(_) => TypeDecl::F32,
            Expr::Identifier(identifier) => {
                let last_context = self.contexts.last().unwrap();
                let identifier_type = last_context
                    .variable_types
                    .get(identifier)
                    .expect("Could not find identifier in the environment");
                identifier_type.clone()
            }

            Expr::FuncCall { func_name, params } => {
                // Get the function type from the function_types map.
                // Ensure the number of parameters match the number of parameters in the function type.
                // Ensure the types of the parameters match the types of the parameters in the function type.
                let last_context = self.contexts.last().unwrap();
                let function_type = last_context
                    .function_types
                    .get(func_name)
                    .expect("Could not find function in the environment");

                let function_params = last_context
                    .function_params
                    .get(func_name)
                    .expect("Could not find function in the environment");

                if params.len() != function_params.len() {
                    panic!("Number of parameters in function call does not match the number of parameters in the function type.");
                }

                for (param, param_type) in params.iter().zip(function_params.iter()) {
                    let param_expr_type = self.typecheck_expr(param);
                    if param_expr_type != *param_type {
                        panic!("Type mismatch in function call.");
                    }
                }

                function_type.clone()
            }

            Expr::Binary { lhs, rhs, op } => {
                let lhstype = self.typecheck_expr(lhs);
                let rhstype = self.typecheck_expr(rhs);
                if discriminant(&lhstype) != discriminant(&rhstype) {
                    panic!("Type mismatch in binary expression.");
                }

                // If the op is a relational operator, return bool.
                // Otherwise, return the type of the lhs.
                match op {
                    crate::ast::Op::Plus
                    | crate::ast::Op::Minus
                    | crate::ast::Op::Multiply
                    | crate::ast::Op::Mod
                    | crate::ast::Op::Assign // TODO: Do we want to return types from assignments??
                    | crate::ast::Op::Divide => {
                        if lhstype != TypeDecl::I32 && lhstype != TypeDecl::F32 {
                            panic!("Binary operator can only be applied to int or float.");
                        } else {
                            return lhstype;
                        }
                    }

                    crate::ast::Op::Eq
                    | crate::ast::Op::Ne
                    | crate::ast::Op::Lt
                    | crate::ast::Op::Gt
                    | crate::ast::Op::Le
                    | crate::ast::Op::And
                    | crate::ast::Op::Or
                    | crate::ast::Op::Not
                    | crate::ast::Op::Ge => {
                        return TypeDecl::Bool;
                    }
                }
            }

            Expr::Unary { op, operand } => {
                // Ensure either float or int is present. Other things cannot have unary operators.
                let operand_type = self.typecheck_expr(operand);
                match operand_type {
                    TypeDecl::I32 | TypeDecl::F32 => operand_type,
                    _ => panic!("Unary operator can only be applied to int or float."),
                }
            }
        }
    }

    fn typecheck_statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::FuncDecl {
                func_name,
                params,
                return_type,
                body,
            } => {
                // Typecheck + insert the function decl in the context.
                // Also insert the current function as the function we are operating for.

                // Insert the function name and return type in the function_types map.
                let last_context = self.contexts.last_mut().unwrap();
                last_context
                    .function_types
                    .insert(func_name.clone(), return_type.clone());

                last_context.function_params.insert(
                    func_name.clone(),
                    params
                        .params
                        .iter()
                        .map(|param| param.var_type.clone())
                        .collect(),
                );

                self.typecheck_statement(body);
            }

            Stmt::IfStatement {
                cond,
                true_branch,
                false_branch,
            } => {
                // Ensure the condition is a boolean.
                let cond_type = self.typecheck_expr(cond);
                if cond_type != TypeDecl::Bool {
                    panic!("If condition must be a boolean.");
                }
                // Typecheck the true and false branches.
                self.typecheck_statement(true_branch);
                if let Some(branch) = false_branch {
                    self.typecheck_statement(branch);
                }
            }

            Stmt::LetStatement { decl, init } => {
                // Get the type of the decl and init.
                // Insert the decl in the context identifiers map.
                // Ensure the decl and init types match.
                let decl_type = decl.var_type.clone();
                let init_type = self.typecheck_expr(init);

                if decl_type != init_type {
                    panic!("Type mismatch in let statement.");
                }

                let last_context = self.contexts.last_mut().unwrap();
                last_context
                    .variable_types
                    .insert(decl.var_name.clone(), decl.var_type.clone());
            }

            Stmt::SetStatement { lvalue, rvalue } => {
                // Get the type of the lvalue and rvalue.
                // Lookup the lvalue in the context identifiers map.
                // LValues cannot be functions.
                // TODO: Maybe having a function as lvalue could be useful for function pointers.

                let lvalue_type = self
                    .contexts
                    .last()
                    .unwrap()
                    .variable_types
                    .get(lvalue)
                    .expect("Could not find lvalue in the environment");

                let rvalue_type = self.typecheck_expr(rvalue);

                if lvalue_type != &rvalue_type {
                    panic!("Type mismatch in assignment.");
                }
            }

            Stmt::Block(vec) => {
                for statement in vec {
                    self.typecheck_statement(statement);
                }
            }

            Stmt::WhileStatement { cond, body } => {
                // Ensure the cond is a boolean.
                let cond_type = self.typecheck_expr(cond);
                if cond_type != TypeDecl::Bool {
                    panic!("While condition must be a boolean.");
                }

                self.typecheck_statement(body);
            }

            Stmt::ReturnStatement(expr) => {
                // Make sure the return type matches the function return type in the current scope.
                // Ehh. Leave it alone for now, make sure eth else works.
            }
        }
    }
}

#[cfg(test)]
mod typechecker_tests {
    use crate::lexer;

    #[test]
    fn comprehensive_test() {
        let program = r#"
            let mainfn: fn(x: i32, y: i32, z: i32, m: vec<i32>) -> i32 = {
                let x: i32 = 10;
                let y: i32 = 20;
                let z: i32 = x + y;
                return z;

                if (x + y) > z {
                    return x;
                } else {
                    return y;
                }

                while ( x < y ) {
                    x = x + 1;
                }
            }"#;

        // Lex, parse, and typecheck the program.
        let mut parser = crate::parser::Parser {
            input_stream: &lexer::lexer(program).unwrap(),
        };
        let ast = parser.parse_statement().unwrap();
        let mut typecheck_info = crate::typecheck::TypeCheckInfo::new();
        typecheck_info.typecheck_statement(&ast);
    }
}
