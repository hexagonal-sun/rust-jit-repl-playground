use std::{collections::BTreeMap, ffi::c_void};

use cranelift::{prelude::{
    AbiParam, EntityRef, FunctionBuilder, FunctionBuilderContext, InstBuilder, IntCC, Type, Value,
    Variable,
}, codegen::Context};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use rustyline::Editor;

use self::frontend::{parser, Expr};

mod frontend;

pub struct REPL {
    n: u32,
    editor: Editor<()>,
    live_vars: BTreeMap<usize, String>,
    next_var_idx: usize,
    module: JITModule,
    fn_bulider_ctx: FunctionBuilderContext,
    ctx: Context,
    ty: Type,
}

impl REPL {
    pub fn new() -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names()).unwrap();
        let module = JITModule::new(builder);
        let ty = module.target_config().pointer_type();
        let ctx = module.make_context();

        Self {
            ty,
            n: 0,
            editor: rustyline::Editor::<()>::new().unwrap(),
            live_vars: BTreeMap::new(),
            fn_bulider_ctx: FunctionBuilderContext::new(),
            next_var_idx: 0,
            module,
            ctx,
        }
    }

    unsafe extern "C" fn cb(user_data: *mut c_void) -> *const c_void {
        let state = &mut *(user_data as *mut REPL);
        state.read_eval()
    }

    pub fn run(&mut self) {
        // Jit the first function and call it.
        let ptr = self.read_eval();

        unsafe {
            let f = std::mem::transmute::<
                _,
                fn(unsafe extern "C" fn(d: *mut c_void) -> *const c_void, d: *mut c_void) -> (),
            >(ptr);

            f(Self::cb, self as *mut _ as *mut c_void)
        }
    }

    fn read_eval(&mut self) -> *const c_void {
        let stmt_str = self.editor.readline("> ").unwrap();
        let stmt = parser::statement(&stmt_str).unwrap();

        let fn_ptr = self.jit_next_repl_fn(stmt);

        self.n += 1;
        fn_ptr
    }

    /// JIT the next repl function. If we imagine that the current assignments
    /// are 'a' and 'b' and the stmt parsed was `c = 24`, the the jit'd function
    /// would look like:
    ///
    /// ```
    /// fn repl_2(jit_callback, user_data, a, b) {
    ///      <code to execute c = 24>;
    ///      let next_repl_fn = jit_callback(user_data);
    ///      next_repl_fn(jit_callback, user_data, a, b, c);
    /// }
    /// ```
    fn jit_next_repl_fn(&mut self, stmt: Expr) -> *const c_void {
        // The first two parameters are the jit callback and user data.
        self.ctx.func.signature.params.push(AbiParam::new(self.ty));
        self.ctx.func.signature.params.push(AbiParam::new(self.ty));

        // Push any previous assignments and declare variables for them.
        for _ in self.live_vars.iter() {
            self.ctx.func.signature.params.push(AbiParam::new(self.ty));
        }

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.fn_bulider_ctx);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);

        for (idx, _) in self.live_vars.iter() {
            let var = Variable::new(*idx);
            builder.declare_var(var, self.ty);
            builder.def_var(var, builder.block_params(entry_block)[idx + 2]);
        }

        let mut translator = StmtTranslator {
            live_vars: &mut self.live_vars,
            builder: &mut builder,
            next_var_idx: &mut self.next_var_idx,
            module: &mut self.module,
            ty: self.ty,
        };

        translator.translate_expr(stmt);

        // Call back into the JIT.
        let jit_fn_ptr = translator.builder.block_params(entry_block)[0];
        let jit_user_data = translator.builder.block_params(entry_block)[1];

        let mut jit_sig = translator.module.make_signature();
        jit_sig.returns.push(AbiParam::new(self.ty));
        jit_sig.params.push(AbiParam::new(self.ty));

        let jit_sig_ref = translator.builder.import_signature(jit_sig);

        let jit_call = translator.builder
                .ins()
                .call_indirect(jit_sig_ref, jit_fn_ptr, &[jit_user_data]);

        let next_repl_fn_ptr = translator.builder.inst_results(jit_call)[0];

        let mut next_repl_fn_sig = translator.module.make_signature();

        // pass on the jit function pointer and user data.
        next_repl_fn_sig.params.push(AbiParam::new(self.ty));
        next_repl_fn_sig.params.push(AbiParam::new(self.ty));

        for _ in translator.live_vars.iter() {
            next_repl_fn_sig.params.push(AbiParam::new(self.ty));
        }

        let next_repl_fn_sigref = translator.builder.import_signature(next_repl_fn_sig);

        let mut params = vec![jit_fn_ptr, jit_user_data];

        for (idx, _) in translator.live_vars.iter() {
            params.push(translator.builder.use_var(Variable::new(*idx)));
        }

        translator.builder
            .ins()
            .call_indirect(next_repl_fn_sigref, next_repl_fn_ptr, &params);

        translator.builder.ins().return_(&[]);
        translator.builder.seal_block(entry_block);
        translator.builder.finalize();

        let id = self
            .module
            .declare_function(
                &format!("repl_{}", self.n),
                Linkage::Local,
                &self.ctx.func.signature,
            )
            .expect("Could not declare function");

        self.module.define_function(id, &mut self.ctx).expect("Could not define function");
        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions();
        self.module.get_finalized_function(id) as *const c_void
    }

}
struct StmtTranslator<'a> {
    builder: &'a mut FunctionBuilder<'a>,
    live_vars: &'a mut BTreeMap<usize, String>,
    next_var_idx: &'a mut usize,
    module: &'a mut JITModule,
    ty: Type,
}

impl<'a> StmtTranslator<'a> {
    fn translate_expr(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Literal(literal) => {
                let imm: i32 = literal.parse().unwrap();
                self.builder.ins().iconst(self.ty, i64::from(imm))
            }

            Expr::Add(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);
                self.builder.ins().iadd(lhs, rhs)
            }

            Expr::Sub(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);
                self.builder.ins().isub(lhs, rhs)
            }

            Expr::Mul(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);
                self.builder.ins().imul(lhs, rhs)
            }

            Expr::Div(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);
                self.builder.ins().udiv(lhs, rhs)
            }

            Expr::Eq(lhs, rhs) => self.translate_icmp(IntCC::Equal, *lhs, *rhs),
            Expr::Ne(lhs, rhs) => self.translate_icmp(IntCC::NotEqual, *lhs, *rhs),
            Expr::Lt(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThan, *lhs, *rhs),
            Expr::Le(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThanOrEqual, *lhs, *rhs),
            Expr::Gt(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThan, *lhs, *rhs),
            Expr::Ge(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThanOrEqual, *lhs, *rhs),
            Expr::Call(name, args) => self.translate_call(name, args),
            Expr::GlobalDataAddr(name) => self.translate_global_data_addr(name),
            Expr::Identifier(name) => {
                // `use_var` is used to read the value of a variable.
                let var = self
                    .live_vars
                    .iter()
                    .find(|(_, n)| **n == name)
                    .expect("Variable not defined");
                self.builder.use_var(Variable::new(*var.0))
            }
            Expr::Assign(name, expr) => self.translate_assign(name, *expr),
            Expr::IfElse(condition, then_body, else_body) => {
                self.translate_if_else(*condition, then_body, else_body)
            }
            Expr::WhileLoop(condition, loop_body) => {
                self.translate_while_loop(*condition, loop_body)
            }
        }
    }

    fn translate_assign(&mut self, name: String, expr: Expr) -> Value {
        let new_value = self.translate_expr(expr);
        let idx = *self.next_var_idx;

        *self.next_var_idx += 1;
        self.live_vars.insert(idx, name);

        let variable = Variable::new(idx);
        self.builder.declare_var(variable, self.ty);
        self.builder.def_var(variable, new_value);

        new_value
    }

    fn translate_icmp(&mut self, cmp: IntCC, lhs: Expr, rhs: Expr) -> Value {
        let lhs = self.translate_expr(lhs);
        let rhs = self.translate_expr(rhs);
        let c = self.builder.ins().icmp(cmp, lhs, rhs);
        self.builder.ins().bint(self.ty, c)
    }

    fn translate_if_else(
        &mut self,
        condition: Expr,
        then_body: Vec<Expr>,
        else_body: Vec<Expr>,
    ) -> Value {
        let condition_value = self.translate_expr(condition);

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        // If-else constructs in the toy language have a return value.
        // In traditional SSA form, this would produce a PHI between
        // the then and else bodies. Cranelift uses block parameters,
        // so set up a parameter in the merge block, and we'll pass
        // the return values to it from the branches.
        self.builder.append_block_param(merge_block, self.ty);

        // Test the if condition and conditionally branch.
        self.builder.ins().brz(condition_value, else_block, &[]);
        // Fall through to then block.
        self.builder.ins().jump(then_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let mut then_return = self.builder.ins().iconst(self.ty, 0);
        for expr in then_body {
            then_return = self.translate_expr(expr);
        }

        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &[then_return]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        let mut else_return = self.builder.ins().iconst(self.ty, 0);
        for expr in else_body {
            else_return = self.translate_expr(expr);
        }

        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &[else_return]);

        // Switch to the merge block for subsequent statements.
        self.builder.switch_to_block(merge_block);

        // We've now seen all the predecessors of the merge block.
        self.builder.seal_block(merge_block);

        // Read the value of the if-else by reading the merge block
        // parameter.
        let phi = self.builder.block_params(merge_block)[0];

        phi
    }

    fn translate_while_loop(&mut self, condition: Expr, loop_body: Vec<Expr>) -> Value {
        let header_block = self.builder.create_block();
        let body_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        self.builder.ins().jump(header_block, &[]);
        self.builder.switch_to_block(header_block);

        let condition_value = self.translate_expr(condition);
        self.builder.ins().brz(condition_value, exit_block, &[]);
        self.builder.ins().jump(body_block, &[]);

        self.builder.switch_to_block(body_block);
        self.builder.seal_block(body_block);

        for expr in loop_body {
            self.translate_expr(expr);
        }
        self.builder.ins().jump(header_block, &[]);

        self.builder.switch_to_block(exit_block);

        // We've reached the bottom of the loop, so there will be no
        // more backedges to the header to exits to the bottom.
        self.builder.seal_block(header_block);
        self.builder.seal_block(exit_block);

        // Just return 0 for now.
        self.builder.ins().iconst(self.ty, 0)
    }

    fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
        let mut sig = self.module.make_signature();

        for _arg in &args {
            sig.params.push(AbiParam::new(self.ty));
        }

        sig.returns.push(AbiParam::new(self.ty));

        let callee = self
            .module
            .declare_function(&name, Linkage::Preemptible, &sig)
            .expect("problem declaring function");
        let local_callee = self
            .module
            .declare_func_in_func(callee, &mut self.builder.func);

        let mut arg_values = Vec::new();
        for arg in args {
            arg_values.push(self.translate_expr(arg))
        }
        let call = self.builder.ins().call(local_callee, &arg_values);
        self.builder.inst_results(call)[0]
    }

    fn translate_global_data_addr(&mut self, name: String) -> Value {
        let sym = self
            .module
            .declare_data(&name, Linkage::Export, true, false)
            .expect("problem declaring data object");
        let local_id = self
            .module
            .declare_data_in_func(sym, &mut self.builder.func);

        let pointer = self.module.target_config().pointer_type();
        self.builder.ins().symbol_value(pointer, local_id)
    }
}
