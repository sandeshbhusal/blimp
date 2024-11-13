enum Op {
    Plus,
    Minus,
    Power,
    Divide,
    Multiply,

    Lt,
    Gt,
    Ge,
    Le,
    Eq,
    Ne,

    And,
    Or,
}

pub enum Expr {
    I32(i32),
    F32(f32),
    Identifier(String),
    Bool(bool),
    Binary {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op: Op,
    },
}

enum Stmt {
    ExpressionStatement(Box<Expr>),
}

/// Gather identifiers from an expression.
fn gather_identifiers(expr: &Expr) -> Vec<String> {
    return match expr {
        Expr::I32(_) => vec![],
        Expr::F32(_) => vec![],
        Expr::Identifier(id) => vec![id.clone()],
        Expr::Bool(_) => vec![],
        Expr::Binary { lhs, rhs, op: _ } => {
            let mut col_left = gather_identifiers(lhs);
            let col_right = gather_identifiers(rhs);

            col_left.extend(col_right);
            col_left
        }
    };
}
