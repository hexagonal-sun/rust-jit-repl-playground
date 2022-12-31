Rust JIT REPL playground

This repository contains code to investigate different approaches to
implementing a rust REPL using Cranelift's JIT compiler. For now we are using
the same toy language as used the [jit
demo](https://github.com/bytecodealliance/cranelift-jit-demo). The current
approach is to JIT a function such as:

```rust
fn repl_1(rust_cb: fn(*const c_void) -> *const c_void, user_data: *const c_void, a: u32)
{
    // evaluate the last parsed stmt.
    let b: u32 = {}
    
    // Call back into rust to read the next line, 
    // jit it and return the function pointer.
    let fn_ptr = rust_cb(user_data); // REPL prompt here.
    fn_ptr(rust_cb, user_data, a, b);
}
```

Here we have a binding already active `a`. During `a`'s (`repl_0`) rust callback
we inspected the parsed statement and picked up an additional binding `b`. We
then JIT'd the above function that will pass on `b` onto the next call.
