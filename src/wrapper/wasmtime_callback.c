/*
 * C callback trampoline for wasmtime function callbacks.
 *
 * Instead of using libffi (via ctypes-foreign / Foreign.funptr) to dynamically
 * create C function pointers from OCaml closures, we use a single static C
 * trampoline function. The OCaml closure is stored as a GC root and passed
 * through wasmtime's `env` pointer.
 *
 * This eliminates the dependency on ctypes-foreign and libffi, which is
 * particularly problematic on Windows where libffi requires mingw packages.
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <stdint.h>
#include <wasm.h>
#include <wasi.h>
#include <wasmtime.h>

/*
 * Static callback trampoline matching wasmtime_func_callback_t.
 *
 * The `env` pointer holds a GC root to an OCaml closure with signature:
 *   nativeint -> nativeint -> nativeint
 *   (args_ptr -> results_ptr -> trap_ptr)
 *
 * The closure captures nargs/nresults from its creation context.
 */
static wasm_trap_t *ocaml_wasmtime_callback_trampoline(
    void *env, wasmtime_caller_t *caller, const wasmtime_val_t *args,
    size_t nargs, wasmtime_val_t *results, size_t nresults) {
  CAMLparam0();
  CAMLlocal3(closure, v_args, v_results);

  (void)caller;
  (void)nargs;
  (void)nresults;

  value *root = (value *)env;
  closure = *root;

  v_args = caml_copy_nativeint((intptr_t)args);
  v_results = caml_copy_nativeint((intptr_t)results);

  value trap_nativeint = caml_callback2(closure, v_args, v_results);

  wasm_trap_t *trap = (wasm_trap_t *)(intptr_t)Nativeint_val(trap_nativeint);
  CAMLreturnT(wasm_trap_t *, trap);
}

/*
 * Wrapper around wasmtime_func_new that hardcodes the callback trampoline.
 * This avoids needing to pass a static_funptr through ctypes.
 */
void ocaml_wasmtime_func_new_with_env(wasmtime_context_t *context,
                                      const wasm_functype_t *type, void *env,
                                      void *finalizer,
                                      wasmtime_func_t *ret) {
  (void)finalizer;
  wasmtime_func_new(context, type, ocaml_wasmtime_callback_trampoline, env,
                    NULL, ret);
}

/*
 * Register an OCaml value as a GC root.
 * Returns the root pointer as a nativeint for use as wasmtime's env pointer.
 */
CAMLprim value ocaml_wasmtime_root_create(value v) {
  CAMLparam1(v);
  value *root = (value *)caml_stat_alloc(sizeof(value));
  *root = v;
  caml_register_global_root(root);
  CAMLreturn(caml_copy_nativeint((intptr_t)root));
}

/*
 * Release a GC root previously created by ocaml_wasmtime_root_create.
 */
CAMLprim value ocaml_wasmtime_root_release(value ptr) {
  value *root = (value *)(intptr_t)Nativeint_val(ptr);
  caml_remove_global_root(root);
  caml_stat_free(root);
  return Val_unit;
}
