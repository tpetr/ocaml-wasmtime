(** Low-level FFI for callback GC root management.

    These externals are implemented in wasmtime_callback.c and bypass ctypes
    to avoid the ctypes-foreign / libffi dependency. *)

(** Register an OCaml closure as a GC root.
    Returns the root pointer as a nativeint, suitable for passing
    as wasmtime's env pointer via [Ctypes.ptr_of_raw_address]. *)
external root_create : 'a -> nativeint = "ocaml_wasmtime_root_create"

(** Release a GC root previously created by [root_create]. *)
external root_release : nativeint -> unit = "ocaml_wasmtime_root_release"
