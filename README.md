# ocaml-wasmtime

OCaml bindings for [wasmtime](https://wasmtime.dev/), a WebAssembly runtime.

Provides a typesafe interface to compile and execute WebAssembly modules
from OCaml, with support for [WASI](https://wasi.dev/) system calls.
Built on the wasmtime v41 C API using ctypes with generated C stubs
(no libffi dependency).

## Supported features

- Compile WAT or WASM bytecode into modules
- Instantiate modules with or without imports
- Call exported functions with typed arguments and results
- Define OCaml functions as WASM imports (callbacks)
- Read and write linear memory
- Inspect module exports (names and kinds) without instantiation
- WASI configuration: argv, env, stdin/stdout/stderr, filesystem preopens
- Capture WASI stdout/stderr into OCaml strings
- Linker-based instantiation for multi-module setups

## Platforms

Requires OCaml >= 5.0. Tested on:

- macOS (aarch64, x86\_64)
- Linux glibc (aarch64, x86\_64)
- Linux musl/Alpine (aarch64, x86\_64)
- Windows (x86\_64 via MinGW)

## Installation

### Via opam pin

This fork is not currently published to the opam registry. To install,
pin directly from the repository:

```
opam pin add libwasmtime https://github.com/tpetr/ocaml-wasmtime.git
opam pin add wasmtime https://github.com/tpetr/ocaml-wasmtime.git
```

### From source

Install the C library:
```
opam install libwasmtime
```

Or download it manually and set `LIBWASMTIME` to point at the extracted
directory (must contain `lib/` and `include/` subdirectories):
```
ocaml scripts/setup.ml
```

Then build and run tests:
```
dune build
dune test
```

## Usage

```ocaml
module W = Wasmtime.Wrappers

let () =
  let engine = W.Engine.create () in
  let store = W.Store.create engine in
  let wat = {|
    (module
      (func $add (param i32 i32) (result i32)
        local.get 0
        local.get 1
        i32.add)
      (export "add" (func $add)))
  |} in
  let wasm = W.Wasmtime.wat_to_wasm ~wat:(W.Byte_vec.of_string wat) in
  let modl = W.Wasmtime.new_module engine ~wasm in
  let instance = W.Wasmtime.new_instance store modl in
  match W.Instance.exports store instance with
  | [ add_extern ] ->
    let add = W.Extern.as_func add_extern in
    let result = W.Wasmtime.func_call1 store add [ Int32 3; Int32 4 ] in
    Printf.printf "3 + 4 = %d\n" (Wasmtime.Val.int_exn result)
  | _ -> failwith "expected one export"
```

### Typed function calls

The `func_call` interface uses GADTs to provide type-safe argument
passing and result extraction:

```ocaml
let add = W.Extern.as_func add_extern in
let result =
  W.Wasmtime.func_call
    ~args:Val.Kind.(t2 Int32 Int32)
    ~results:Val.Kind.(t1 Int32)
    store add (3, 4)
in
Printf.printf "3 + 4 = %d\n" result
```

### OCaml callbacks as WASM imports

```ocaml
let log_func =
  W.Func.of_func
    ~args:Val.Kind.(t1 Int32)
    ~results:Val.Kind.t0
    store
    (fun n -> Printf.printf "wasm says: %d\n" n)
in
let instance =
  W.Wasmtime.new_instance store modl
    ~imports:[ W.Extern.func_as log_func ]
```

### Module export inspection

Retrieve export metadata from a compiled module without instantiation:

```ocaml
let exports = W.Module.exports modl in
List.iter
  (fun (e : W.Module.export) ->
    match e.kind with
    | Func -> Printf.printf "function: %s\n" e.name
    | Memory -> Printf.printf "memory: %s\n" e.name
    | Table -> Printf.printf "table: %s\n" e.name
    | Global -> Printf.printf "global: %s\n" e.name)
  exports
```

### WASI with output capture

```ocaml
let cap = W.Wasi.create_capture () in
W.Wasi.configure ~stdout:(Capture cap) store;
(* ... run WASI module ... *)
let output = W.Wasi.capture_contents cap in
print_string output
```

See the `tests/` directory for complete working examples.

## Origin

This is a fork of [LaurentMazworking/ocaml-wasmtime](https://github.com/LaurentMazare/ocaml-wasmtime)
by Laurent Mazare, updated to wasmtime v41 with a reworked FFI layer.
Thanks to Laurent for the original work.

## License

Apache-2.0
