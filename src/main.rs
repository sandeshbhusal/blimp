use cranelift::prelude::{types, AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder};
use cranelift::{
    jit::{JITBuilder, JITModule},
    module::{Linkage, Module},
};

type RvalType = extern "C" fn(i32, i32) -> i32;

fn compile() -> RvalType {
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
    let ptr_b = unsafe { std::mem::transmute::<_, RvalType>(code) };

    ptr_b
}

fn main() {
    println!("{}", compile()(10, 100000000));
}
