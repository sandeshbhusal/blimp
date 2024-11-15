use std::collections::BTreeMap;
use std::time::Instant;

use cranelift::prelude::{types, AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder};
use cranelift::{
    jit::{JITBuilder, JITModule},
    module::{Linkage, Module},
};
use fasteval::Compiler;
use fasteval::Evaler;
use random::Source;

type RvalType = extern "C" fn(f64, f64) -> f64;

mod ast;
mod lexer;
mod parser;
mod codegen;
mod typecheck;
mod interpreter;

fn compile() -> RvalType {
    let jit_builder = JITBuilder::new(cranelift::module::default_libcall_names()).unwrap();
    let mut jit_module = JITModule::new(jit_builder);
    let mut context = jit_module.make_context();
    let mut func_context = FunctionBuilderContext::new();

    let mut signatures = jit_module.make_signature();
    for _ in 0..2 {
        signatures.params.push(AbiParam::new(types::F64));
    }
    signatures.returns.push(AbiParam::new(types::F64));

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
        let rval = fb.ins().fadd(first, second);
        fb.ins().return_(&[rval]);
    }

    jit_module.define_function(func, &mut context).unwrap();
    jit_module.clear_context(&mut context);
    jit_module.finalize_definitions().unwrap();

    let code = jit_module.get_finalized_function(func);
    let ptr_b = unsafe { std::mem::transmute::<_, RvalType>(code) };

    ptr_b
}

const SIZE: usize = 100_000_000;

fn main() -> Result<(), fasteval::Error> {
    let compiled = compile();

    let mut random = random::default(1234);
    let mut vec1rand = vec![];
    let mut vec2rand = vec![];
    for _ in 0..SIZE {
        vec1rand.push(random.read_f64());
        vec2rand.push(random.read_f64());
    }

    let mut cumul_crane = 0u128;
    for i in 0..SIZE {
        let x = vec1rand[i];
        let y = vec2rand[i];

        let start = Instant::now();
        compiled(x, y);
        cumul_crane += start.elapsed().as_micros();
    }

    println!();
    println!("{}", cumul_crane);

    let mut cumul_fastexpr = 0u128;

    let parser = fasteval::Parser::new();
    let mut slab = fasteval::Slab::new();
    let expr_str = "a + b";
    let compiled = parser
        .parse(expr_str, &mut slab.ps)
        .unwrap()
        .from(&slab.ps)
        .compile(&slab.ps, &mut slab.cs);

    let mut map = BTreeMap::new();
    for i in 0..SIZE {
        map.insert("a", vec1rand[i]);
        map.insert("b", vec2rand[i]);

        let start = Instant::now();
        fasteval::eval_compiled!(compiled, &slab, &mut map);
        cumul_fastexpr += start.elapsed().as_micros();
    }

    println!();
    println!("{}", cumul_fastexpr);

    Ok(())
}
