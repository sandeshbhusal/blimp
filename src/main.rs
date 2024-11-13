use cranelift::prelude::Value;
use std::collections::HashMap;

use cranelift::{
    jit::{JITBuilder, JITModule},
    module::{Linkage, Module},
    prelude::{
        settings,
        types::{self, I32},
        AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder,
    },
};

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

enum Expr {
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

fn eval() {}

type rvalType = extern "C" fn(i32, i32) -> i32;
fn compile(expr: Expr) -> rvalType {
    let ids = gather_identifiers(&expr);

    let jit_builder = JITBuilder::new(cranelift::module::default_libcall_names()).unwrap();
    let mut jit_module = JITModule::new(jit_builder);
    let mut context = jit_module.make_context();
    let mut func_context = FunctionBuilderContext::new();

    let mut signatures = jit_module.make_signature();
    for _ in 0..2 {
        signatures.params.push(AbiParam::new(types::I32));
    }
    signatures.returns.push(AbiParam::new(types::I32));

    let func = jit_module
        .declare_function("check", Linkage::Local, &signatures)
        .unwrap();

    context.func.signature = signatures;
    let mut fb = FunctionBuilder::new(&mut context.func, &mut func_context);
    {
        let block = fb.create_block();
        fb.switch_to_block(block);
        fb.append_block_params_for_function_params(block);
        let first = fb.block_params(block)[0];
        let second = fb.block_params(block)[1];
        let rval = fb.ins().iadd(first, second);
        fb.ins().return_(&[rval]);
    }

    jit_module.define_function(func, &mut context).unwrap();
    jit_module.clear_context(&mut context);
    jit_module.finalize_definitions().unwrap();

    let code = jit_module.get_finalized_function(func);
    let ptr_b = unsafe { std::mem::transmute::<_, rvalType>(code) };

    ptr_b
}

fn main() {
    println!("{}", compile(Expr::I32(10))(10, 100000000));

    // let expr = "x1 < x2 || x1 ** 2 == x2";

    // let lhs = Expr::Binary {
    //     lhs: Box::new(Expr::Identifier("x1".into())),
    //     rhs: Box::new(Expr::Identifier("x2".into())),
    //     op: Op::Lt,
    // };

    // let rhs = Expr::Binary {
    //     lhs: Box::new(Expr::Binary {
    //         lhs: Box::new(Expr::Identifier("x1".into())),
    //         rhs: Box::new(Expr::I32(2)),
    //         op: Op::Power,
    //     }),
    //     rhs: Box::new(Expr::Identifier("x2".into())),
    //     op: Op::Eq,
    // };

    // let expr = Expr::Binary { lhs: Box::new(lhs), rhs: Box::new(rhs), op: Op::Or};

    // // In order to generate a function we gather all identifiers from the expression
    // // we want to convert into a function.

    // let found_identifiers = gather_identifiers(&expr);
    // let compiled_func = compile(expr);

    // let mut context = HashMap::new();
    // context.insert("x1", 10);
    // context.insert("x2", 20);

    // eval(compiled_func, context);
}
