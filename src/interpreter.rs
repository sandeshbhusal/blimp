use crate::ast::Expr;
use crate::value::Value;
use std::collections::HashMap;

pub struct Interpreter<T>
where
    T: Environment,
{
    environment: T,
}

trait Environment {
    fn get(&self, identifier: &str) -> Value;
    fn set(&mut self, identifier: &str, value: Value);
}

impl Environment for HashMap<String, Value> {
    fn get(&self, identifier: &str) -> Value {
        self.get(identifier)
            .expect("Could not find value in environment")
            .clone()
    }

    fn set(&mut self, identifier: &str, value: Value) {
        self.insert(identifier.to_string(), value);
    }
}

impl<T> Interpreter<T>
where
    T: Environment,
{
    pub fn new(environment: T) -> Self {
        Self { environment }
    }

    fn eval_expr(&self, expr: Expr) -> Value {
        match expr {
            Expr::I32(val) => val.into(),
            Expr::F32(val) => val.into(),
            Expr::Identifier(ident) => self.environment.get(&ident),
            Expr::FuncCall { func_name, params } => {
                todo!("Function calls are not implemented yet")
            }
            Expr::Binary { lhs, rhs, op } => {
                let lefteval = self.eval_expr(*lhs);
                let righteval = self.eval_expr(*rhs);

                let value = match op {
                    crate::ast::Op::Plus => lefteval + righteval,
                    crate::ast::Op::Minus => lefteval - righteval,
                    crate::ast::Op::Multiply => lefteval * righteval,
                    crate::ast::Op::Divide => lefteval / righteval,
                    crate::ast::Op::Mod => lefteval % righteval,
                    crate::ast::Op::Eq => (lefteval == righteval).into(),
                    crate::ast::Op::Ne => (!(lefteval == righteval)).into(),
                    crate::ast::Op::Gt => (lefteval > righteval).into(),
                    crate::ast::Op::Ge => (lefteval >= righteval).into(),
                    crate::ast::Op::Lt => (lefteval < righteval).into(),
                    crate::ast::Op::Le => (lefteval <= righteval).into(),
                    // crate::ast::Op::And => lefteval && righteval,
                    // crate::ast::Op::Or => lefteval || righteval,
                    op => panic!("Operator {:?} not implemented", op),
                };

                value
            }

            Expr::Unary { op, operand } => {
                todo!()
            }
        }
    }
}

#[cfg(test)]
mod interptest {
    use crate::lexer::lexer;

    #[test]
    fn check_basic_addition() {
        let expression = " x + y + 2";
        let mut env = std::collections::HashMap::new();
        env.insert("x".to_string(), 1.into());
        env.insert("y".to_string(), 2.into());

        let interpreter = super::Interpreter::new(env);
        let lexer = lexer(expression).unwrap();
        let mut parser = crate::parser::Parser::new(lexer.as_ref());

        let value =
            interpreter.eval_expr(parser.parse_expr().unwrap());

        dbg!(value);
    }
}
