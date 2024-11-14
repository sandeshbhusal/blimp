use crate::lexer::TokenType;

// Enum for different operation types
#[derive(Debug)]
pub(crate) enum Op {
    Plus,
    Minus,
    Multiply,
    Divide,
    Mod,
    Lt,
    Gt,
    Le,
    Ge,
    Ne,
    Eq,
    And,
    Or,
    Not,
    Assign
}


// Enum for different expression types
#[derive(Debug)]
pub enum Expr {
    I32(i32),                          // Integer literal
    F32(f32),                        // Float literal
    Identifier(String),                 // Variable identifier
    FuncCall {                          // Function call with name and parameters
        func_name: String,
        params: Vec<Expr>,
    },
    Binary {                            // Binary expression with lhs, rhs and operator
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op: Op,
    },
    Unary {
        op: Op,
        operand: Box<Expr>
    }
}

// Struct for variable declaration with a name and type
pub struct VariableDeclaration {
    pub(crate) var_name: String,
    pub(crate) var_type: TypeDecl,
}

// Enum for type declarations
pub enum TypeDecl {
    I32,
    F32,
    Bool,
    Vec(Box<TypeDecl>),               // Vector of a specific type
    Func(Box<ParamList>, Box<TypeDecl>), // Function type with parameters and return type
    UDT(String),
}

// ParamList is a vector of parameters with their names and types
pub struct ParamList {
    pub(crate) params: Vec<Param>,
}

// Param represents a function parameter with a name and type
pub struct Param {
    pub(crate) var_name: String,
    pub(crate) var_type: TypeDecl,
}

// Enum for different statement types
pub enum Stmt {
    // Function declaration with name, parameters, return type, and body
    FuncDecl {
        func_name: String,
        params: ParamList,
        return_type: TypeDecl,
        body: Box<Stmt>,
    },
    // If statement with condition, true branch, and an optional false branch
    IfStatement {
        cond: Expr,
        true_branch: Box<Stmt>,
        false_branch: Option<Box<Stmt>>,
    },
    // Variable declaration statement with initialization
    LetStatement {
        decl: VariableDeclaration,
        init: Expr,
    },
    // Assignment statement with left-hand side (lvalue) and right-hand side (rvalue)
    SetStatement {
        lvalue: String,
        rvalue: Expr,
    },
    // Block of statements
    Block(Vec<Stmt>),
    // While loop with condition and body
    WhileStatement {
        cond: Box<Expr>,
        body: Box<Stmt>,
    },
    // Return statement.
    ReturnStatement(Expr),
}
