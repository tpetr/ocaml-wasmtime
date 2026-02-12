open! Ctypes

module C (F : Cstubs.FOREIGN) = struct
  open! F

  module Byte_vec = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_byte_vec_t"
    let size = field struct_ "size" size_t
    let data = field struct_ "data" (ptr char)
    let () = seal struct_
    let t : t typ = ptr struct_
    let new_ = foreign "wasm_byte_vec_new" (t @-> size_t @-> string @-> returning void)

    let new_uninitialized =
      foreign "wasm_byte_vec_new_uninitialized" (t @-> size_t @-> returning void)

    let copy = foreign "wasm_byte_vec_copy" (t @-> t @-> returning void)
    let delete = foreign "wasm_byte_vec_delete" (t @-> returning void)
  end

  module Config = struct
    type t = unit ptr

    let t : t typ = ptr void
    let new_ = foreign "wasm_config_new" (void @-> returning t)
    let delete = foreign "wasm_config_delete" (t @-> returning void)
  end

  module Engine = struct
    type t = unit ptr

    let t : t typ = ptr void
    let new_ = foreign "wasm_engine_new" (void @-> returning t)
    let new_with_config = foreign "wasm_engine_new_with_config" (Config.t @-> returning t)
    let delete = foreign "wasm_engine_delete" (t @-> returning void)
  end

  (* wasmtime_store_t is an opaque pointer *)
  module Store = struct
    type t = unit ptr

    let t : t typ = ptr void

    let new_ =
      foreign "wasmtime_store_new"
        (Engine.t @-> ptr void @-> ptr void @-> returning t)

    let delete = foreign "wasmtime_store_delete" (t @-> returning void)
  end

  (* wasmtime_context_t is an opaque pointer *)
  module Context = struct
    type t = unit ptr

    let t : t typ = ptr void
    let of_store = foreign "wasmtime_store_context" (Store.t @-> returning t)
  end

  (* wasmtime_caller_t is an opaque struct pointer *)
  module Caller = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasmtime_caller"
    let t : t typ = ptr struct_
  end

  module Trap = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_trap_t"
    let t : t typ = ptr struct_

    (* v41: wasmtime_trap_new takes (const char*, size_t) *)
    let new_ =
      foreign "wasmtime_trap_new" (string @-> size_t @-> returning t)

    let message = foreign "wasm_trap_message" (t @-> Byte_vec.t @-> returning void)
    let delete = foreign "wasm_trap_delete" (t @-> returning void)
  end

  (* wasmtime_func_t is a small struct (store_id + __private), no delete *)
  module Func = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasmtime_func"
    let store_id = field struct_ "store_id" uint64_t
    let _private = field struct_ "__private" (ptr void)
    let () = seal struct_
    let t : t typ = ptr struct_
  end

  (* wasmtime_table_t has an anonymous inner struct that adds padding:
     struct { uint64_t store_id; uint32_t __private1; }; uint32_t __private2;
     The inner struct is padded to 16 bytes, so __private2 is at offset 16. *)
  module Table = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasmtime_table"
    let store_id = field struct_ "store_id" uint64_t
    let _private1 = field struct_ "__private1" uint32_t
    let _pad = field struct_ "" uint32_t
    let _private2 = field struct_ "__private2" uint32_t
    let () = seal struct_
    let t : t typ = ptr struct_
  end

  (* wasmtime_memory_t has an anonymous inner struct that adds padding:
     struct { uint64_t store_id; uint32_t __private1; }; uint32_t __private2;
     The inner struct is padded to 16 bytes, so __private2 is at offset 16. *)
  module Memory = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasmtime_memory"
    let store_id = field struct_ "store_id" uint64_t
    let _private1 = field struct_ "__private1" uint32_t
    let _pad = field struct_ "" uint32_t
    let _private2 = field struct_ "__private2" uint32_t
    let () = seal struct_
    let t : t typ = ptr struct_
  end

  (* wasmtime_global_t is a small struct *)
  module Global = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasmtime_global"
    let store_id = field struct_ "store_id" uint64_t
    let _private1 = field struct_ "__private1" uint32_t
    let _private2 = field struct_ "__private2" uint32_t
    let _private3 = field struct_ "__private3" uint32_t
    let () = seal struct_
    let t : t typ = ptr struct_
  end

  (* wasmtime_extern_union_t *)
  module Extern_union = struct
    type modl
    type t = modl Ctypes.union

    let t : t typ = union "wasmtime_extern_union"
    let func = field t "func" Func.struct_
    let global = field t "global" Global.struct_
    let table = field t "table" Table.struct_
    let memory = field t "memory" Memory.struct_
    let sharedmemory = field t "sharedmemory" (ptr void)
    let () = seal t
  end

  (* wasmtime_extern_t is a discriminated union *)
  module Extern = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasmtime_extern"
    let kind = field struct_ "kind" uint8_t
    let of_ = field struct_ "of" Extern_union.t
    let () = seal struct_
    let t : t typ = ptr struct_

    let delete = foreign "wasmtime_extern_delete" (t @-> returning void)
  end

  (* wasmtime_externref_t is a struct, not a simple pointer *)
  module Externref = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasmtime_externref"
    let store_id = field struct_ "store_id" uint64_t
    let _private1 = field struct_ "__private1" uint32_t
    let _private2 = field struct_ "__private2" uint32_t
    let _private3 = field struct_ "__private3" (ptr void)
    let () = seal struct_
    let t : t typ = ptr struct_

    let new_ =
      foreign "wasmtime_externref_new"
        (Context.t @-> ptr void @-> ptr void @-> t @-> returning bool)

    let data =
      foreign "wasmtime_externref_data"
        (Context.t @-> t @-> returning (ptr void))

    let clone_ =
      foreign "wasmtime_externref_clone"
        (t @-> t @-> returning void)

    let unroot =
      foreign "wasmtime_externref_unroot"
        (t @-> returning void)
  end

  (* wasmtime_anyref_t *)
  module Anyref = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasmtime_anyref"
    let store_id = field struct_ "store_id" uint64_t
    let _private1 = field struct_ "__private1" uint32_t
    let _private2 = field struct_ "__private2" uint32_t
    let _private3 = field struct_ "__private3" (ptr void)
    let () = seal struct_
    let t : t typ = ptr struct_
  end

  (* wasmtime_valunion_t *)
  module Val_union = struct
    type modl
    type t = modl Ctypes.union

    let t : t typ = union "wasmtime_valunion"
    let i32 = field t "i32" int32_t
    let i64 = field t "i64" int64_t
    let f32 = field t "f32" float
    let f64 = field t "f64" double
    let anyref = field t "anyref" Anyref.struct_
    let externref = field t "externref" Externref.struct_
    let funcref = field t "funcref" Func.struct_
    let () = seal t
  end

  (* wasmtime_val_t *)
  module Val = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasmtime_val"
    let kind = field struct_ "kind" uint8_t
    let of_ = field struct_ "of" Val_union.t
    let () = seal struct_
    let t : t typ = ptr struct_

    let unroot = foreign "wasmtime_val_unroot" (t @-> returning void)
    let clone_ = foreign "wasmtime_val_clone" (t @-> t @-> returning void)
  end

  (* wasm_valtype_t - still used for type-level info *)
  module Val_type = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_valtype_t"
    let t : t typ = ptr struct_
    let new_ = foreign "wasm_valtype_new" (uint8_t @-> returning t)
    let delete = foreign "wasm_valtype_delete" (t @-> returning void)
  end

  module Val_type_vec = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_valtype_vec_t"
    let size = field struct_ "size" size_t
    let data = field struct_ "data" (ptr Val_type.t)
    let () = seal struct_
    let t : t typ = ptr struct_

    let new_ =
      foreign "wasm_valtype_vec_new" (t @-> size_t @-> ptr Val_type.t @-> returning void)

    let delete = foreign "wasm_valtype_vec_delete" (t @-> returning void)
  end

  module Func_type = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_functype_t"
    let t : t typ = ptr struct_

    let new_ =
      foreign "wasm_functype_new" (Val_type_vec.t @-> Val_type_vec.t @-> returning t)

    let new_0_0 = foreign "wasm_functype_new_0_0" (void @-> returning t)
    let delete = foreign "wasm_functype_delete" (t @-> returning void)
  end

  (* wasmtime_module_t is an opaque struct pointer *)
  module Module = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasmtime_module"
    let t : t typ = ptr struct_
    let delete = foreign "wasmtime_module_delete" (t @-> returning void)
  end

  (* wasmtime_instance_t is a small struct *)
  module Instance = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasmtime_instance"
    let store_id = field struct_ "store_id" uint64_t
    let _private = field struct_ "__private" size_t
    let () = seal struct_
    let t : t typ = ptr struct_

    (* wasmtime_instance_export_nth *)
    let export_nth =
      foreign "wasmtime_instance_export_nth"
        (Context.t @-> t @-> size_t
         @-> ptr (ptr char) @-> ptr size_t
         @-> Extern.t
         @-> returning bool)
  end

  (* wasmtime.h specific bits *)
  module Error = struct
    type t = unit ptr

    let t : t typ = ptr void
    let message = foreign "wasmtime_error_message" (t @-> Byte_vec.t @-> returning void)
    let delete = foreign "wasmtime_error_delete" (t @-> returning void)
  end

  module Wasi_config = struct
    type t = unit ptr

    let t : t typ = ptr void
    let new_ = foreign "wasi_config_new" (void @-> returning t)
    let delete = foreign "wasi_config_delete" (t @-> returning void)
    let inherit_argv = foreign "wasi_config_inherit_argv" (t @-> returning void)
    let inherit_env = foreign "wasi_config_inherit_env" (t @-> returning void)
    let inherit_stdin = foreign "wasi_config_inherit_stdin" (t @-> returning void)
    let inherit_stdout = foreign "wasi_config_inherit_stdout" (t @-> returning void)
    let inherit_stderr = foreign "wasi_config_inherit_stderr" (t @-> returning void)

    (* v41: preopen_dir now takes 5 args: config, host_path, guest_path, dir_perms, file_perms *)
    let preopen_dir =
      foreign "wasi_config_preopen_dir"
        (t @-> string @-> string @-> size_t @-> size_t @-> returning bool)
  end

  module Wasmtime = struct
    module Linker = struct
      type t = unit ptr

      let t : t typ = ptr void

      (* v41: linker created from engine, not store *)
      let new_ = foreign "wasmtime_linker_new" (Engine.t @-> returning t)
      let delete = foreign "wasmtime_linker_delete" (t @-> returning void)

      (* v41: define_wasi takes only linker, no wasi_instance *)
      let define_wasi =
        foreign "wasmtime_linker_define_wasi" (t @-> returning Error.t)

      (* v41: define_instance takes context, string+len *)
      let define_instance =
        foreign "wasmtime_linker_define_instance"
          (t @-> Context.t @-> string @-> size_t @-> Instance.t @-> returning Error.t)

      (* v41: instantiate takes context *)
      let instantiate =
        foreign "wasmtime_linker_instantiate"
          (t @-> Context.t @-> Module.t @-> Instance.t @-> ptr Trap.t @-> returning Error.t)

      (* v41: module takes context, string+len *)
      let module_ =
        foreign "wasmtime_linker_module"
          (t @-> Context.t @-> string @-> size_t @-> Module.t @-> returning Error.t)

      (* v41: get_default takes context, string+len, fills in func struct *)
      let get_default =
        foreign "wasmtime_linker_get_default"
          (t @-> Context.t @-> string @-> size_t @-> Func.t @-> returning Error.t)
    end

    module Config = struct
      let debug_info_set =
        foreign "wasmtime_config_debug_info_set" (Config.t @-> bool @-> returning void)

      let max_wasm_stack_set =
        foreign "wasmtime_config_max_wasm_stack_set"
          (Config.t @-> size_t @-> returning void)

      let reference_types_set =
        foreign "wasmtime_config_wasm_reference_types_set"
          (Config.t @-> bool @-> returning void)

      let simd_set =
        foreign "wasmtime_config_wasm_simd_set" (Config.t @-> bool @-> returning void)

      let bulk_memory_set =
        foreign "wasmtime_config_wasm_bulk_memory_set"
          (Config.t @-> bool @-> returning void)

      let multi_value_set =
        foreign "wasmtime_config_wasm_multi_value_set"
          (Config.t @-> bool @-> returning void)

      let memory_reservation_set =
        foreign "wasmtime_config_memory_reservation_set"
          (Config.t @-> uint64_t @-> returning void)

      let memory_guard_size_set =
        foreign "wasmtime_config_memory_guard_size_set"
          (Config.t @-> uint64_t @-> returning void)

      let memory_reservation_for_growth_set =
        foreign "wasmtime_config_memory_reservation_for_growth_set"
          (Config.t @-> uint64_t @-> returning void)

      let epoch_interruption_set =
        foreign "wasmtime_config_epoch_interruption_set"
          (Config.t @-> bool @-> returning void)
    end

    (* v41: wasmtime_context_set_wasi *)
    let context_set_wasi =
      foreign "wasmtime_context_set_wasi"
        (Context.t @-> Wasi_config.t @-> returning Error.t)

    (* v41: wat2wasm takes const char, size_t, wasm_byte_vec_t ptr *)
    let wat2wasm =
      foreign "wasmtime_wat2wasm"
        (string @-> size_t @-> Byte_vec.t @-> returning Error.t)

    (* v41: wasmtime_module_new takes engine, uint8_t ptr, size_t, module ptr ptr *)
    let new_module =
      foreign "wasmtime_module_new"
        (Engine.t @-> ptr uint8_t @-> size_t @-> ptr Module.t @-> returning Error.t)

    (* v41: wasmtime_func_call takes context *)
    let func_call =
      foreign "wasmtime_func_call"
        (Context.t
         @-> Func.t
         @-> ptr Val.struct_
         @-> size_t
         @-> ptr Val.struct_
         @-> size_t
         @-> ptr Trap.t
         @-> returning Error.t)

    (* v41: wasmtime_func_new takes context, fills in func struct *)
    let func_new =
      foreign "wasmtime_func_new"
        (Context.t
         @-> Func_type.t
         @-> static_funptr Ctypes.(
               ptr void @-> Caller.t @-> ptr Val.struct_ @-> size_t
               @-> ptr Val.struct_ @-> size_t @-> returning Trap.t)
         @-> ptr void
         @-> ptr void
         @-> Func.t
         @-> returning void)

    (* v41: wasmtime_instance_new takes context, wasmtime_extern_t array *)
    let new_instance =
      foreign "wasmtime_instance_new"
        (Context.t
         @-> Module.t
         @-> ptr Extern.struct_
         @-> size_t
         @-> Instance.t
         @-> ptr Trap.t
         @-> returning Error.t)

    (* v41: wasmtime_memory_data takes (context, memory) *)
    let memory_data =
      foreign "wasmtime_memory_data"
        (Context.t @-> Memory.t @-> returning (ptr uint8_t))

    let memory_data_size =
      foreign "wasmtime_memory_data_size"
        (Context.t @-> Memory.t @-> returning size_t)

    let memory_size =
      foreign "wasmtime_memory_size"
        (Context.t @-> Memory.t @-> returning uint64_t)

    let memory_grow =
      foreign "wasmtime_memory_grow"
        (Context.t @-> Memory.t @-> uint64_t @-> ptr uint64_t @-> returning Error.t)
  end
end
