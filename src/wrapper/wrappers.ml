open Import

exception Trap of { message : string }

(* v41 val kind constants *)
let wasmtime_i32 = 0
let wasmtime_i64 = 1
let wasmtime_f32 = 2
let wasmtime_f64 = 3
let _wasmtime_funcref = 5
let _wasmtime_externref = 6

(* v41 extern kind constants *)
let wasmtime_extern_func = 0
let _wasmtime_extern_global = 1
let _wasmtime_extern_table = 2
let wasmtime_extern_memory = 3

module Engine = struct
  type t = W.Engine.t

  let create
      ?debug_info
      ?max_wasm_stack
      ?reference_types
      ?simd
      ?bulk_memory
      ?multi_value
      ?memory_reservation
      ?memory_guard_size
      ?memory_reservation_for_growth
      ?consume_fuel
      ()
    =
    let config =
      let t = W.Config.new_ () in
      Option.iter (W.Wasmtime.Config.debug_info_set t) debug_info;
      Option.iter
        (fun sz -> Unsigned.Size_t.of_int sz |> W.Wasmtime.Config.max_wasm_stack_set t)
        max_wasm_stack;
      Option.iter (W.Wasmtime.Config.reference_types_set t) reference_types;
      Option.iter (W.Wasmtime.Config.simd_set t) simd;
      Option.iter (W.Wasmtime.Config.bulk_memory_set t) bulk_memory;
      Option.iter (W.Wasmtime.Config.multi_value_set t) multi_value;
      Option.iter
        (fun sz -> Unsigned.UInt64.of_int sz |> W.Wasmtime.Config.memory_reservation_set t)
        memory_reservation;
      Option.iter
        (fun sz -> Unsigned.UInt64.of_int sz |> W.Wasmtime.Config.memory_guard_size_set t)
        memory_guard_size;
      Option.iter
        (fun sz ->
          Unsigned.UInt64.of_int sz
          |> W.Wasmtime.Config.memory_reservation_for_growth_set t)
        memory_reservation_for_growth;
      Option.iter (W.Wasmtime.Config.consume_fuel_set t) consume_fuel;
      t
    in
    let t = W.Engine.new_with_config config in
    if Ctypes.is_null t then failwith "Engine.new_ returned null";
    Gc.finalise W.Engine.delete t;
    t
end

module Store = struct
  type t = { store : W.Store.t; context : W.Context.t }

  let create engine =
    let store = W.Store.new_ engine Ctypes.null Ctypes.null in
    if Ctypes.is_null store then failwith "Store.new_ returned null";
    let context = W.Context.of_store store in
    Gc.finalise
      (fun store ->
        keep_alive engine;
        W.Store.delete store)
      store;
    { store; context }

  let fail_on_error err =
    if not (Ctypes.is_null err) then begin
      let msg_vec = Ctypes.allocate_n W.Byte_vec.struct_ ~count:1 in
      W.Error.message err msg_vec;
      let s = Ctypes.( !@ ) msg_vec in
      let len = Ctypes.getf s W.Byte_vec.size |> Unsigned.Size_t.to_int in
      let data = Ctypes.getf s W.Byte_vec.data in
      let message = Ctypes.string_from_ptr data ~length:len in
      W.Byte_vec.delete msg_vec;
      W.Error.delete err;
      failwith message
    end

  let set_fuel (store : t) fuel =
    W.Wasmtime.context_set_fuel store.context (Unsigned.UInt64.of_int fuel)
    |> fail_on_error

  let get_fuel (store : t) =
    let out = Ctypes.allocate_n Ctypes.uint64_t ~count:1 in
    W.Wasmtime.context_get_fuel store.context out |> fail_on_error;
    Ctypes.( !@ ) out |> Unsigned.UInt64.to_int
end

module Byte_vec = struct
  type t = W.Byte_vec.t

  let with_finalise ~f =
    let t = Ctypes.allocate_n W.Byte_vec.struct_ ~count:1 in
    f t;
    Gc.finalise W.Byte_vec.delete t;
    t

  let create ~len =
    with_finalise ~f:(fun t ->
      W.Byte_vec.new_uninitialized t (Unsigned.Size_t.of_int len))

  let of_string str =
    with_finalise ~f:(fun t ->
      W.Byte_vec.new_ t (String.length str |> Unsigned.Size_t.of_int) str)

  let length t =
    let t = Ctypes.( !@ ) t in
    Ctypes.getf t W.Byte_vec.size |> Unsigned.Size_t.to_int

  let to_string t =
    let t = Ctypes.( !@ ) t in
    let length = Ctypes.getf t W.Byte_vec.size |> Unsigned.Size_t.to_int in
    let data = Ctypes.getf t W.Byte_vec.data in
    Ctypes.string_from_ptr data ~length
end

module Trap_ = struct
  type t = W.Trap.t

  let maybe_fail (t : t) =
    if not (Ctypes.is_null t) then begin
      let message =
        Byte_vec.with_finalise ~f:(fun message -> W.Trap.message t message)
        |> Byte_vec.to_string
      in
      W.Trap.delete t;
      raise (Trap { message })
    end
end

module Module = struct
  type t = W.Module.t

  type extern_kind = Func | Global | Table | Memory

  type export = { name : string; kind : extern_kind }

  let extern_kind_of_int = function
    | 0 -> Func
    | 1 -> Global
    | 2 -> Table
    | 3 -> Memory
    | n -> Printf.ksprintf failwith "unexpected extern kind %d" n

  let exports t =
    let vec = Ctypes.allocate_n W.Export_type_vec.struct_ ~count:1 in
    W.Module.exports t vec;
    let s = Ctypes.( !@ ) vec in
    let len = Ctypes.getf s W.Export_type_vec.size |> Unsigned.Size_t.to_int in
    let data = Ctypes.getf s W.Export_type_vec.data in
    let result =
      List.init len (fun i ->
        let export_type = Ctypes.( !@ ) (Ctypes.( +@ ) data i) in
        let name_vec = W.Export_type.name export_type in
        let name_s = Ctypes.( !@ ) name_vec in
        let name_len = Ctypes.getf name_s W.Byte_vec.size |> Unsigned.Size_t.to_int in
        let name_data = Ctypes.getf name_s W.Byte_vec.data in
        let name = Ctypes.string_from_ptr name_data ~length:name_len in
        let ext_type = W.Export_type.type_ export_type in
        let kind =
          W.Extern_type.kind ext_type |> Unsigned.UInt8.to_int |> extern_kind_of_int
        in
        { name; kind })
    in
    W.Export_type_vec.delete vec;
    result
end

(* v41 value helpers - using wasmtime_val_t with wasmtime kind constants *)
module V = struct
  let kind_to_c = function
    | Val.Kind.P Int32 -> wasmtime_i32
    | P Int64 -> wasmtime_i64
    | P Float32 -> wasmtime_f32
    | P Float64 -> wasmtime_f64
    | P Any_ref -> _wasmtime_externref
    | P Func_ref -> _wasmtime_funcref

  let kind_of_c = function
    | 0 -> Val.Kind.P Int32
    | 1 -> P Int64
    | 2 -> P Float32
    | 3 -> P Float64
    | 6 -> P Any_ref
    | 5 -> P Func_ref
    | otherwise -> Printf.ksprintf failwith "unexpected Val.kind value %d" otherwise

  let of_ptr ptr =
    let struct_ = Ctypes.( !@ ) ptr in
    let kind = Ctypes.getf struct_ W.Val.kind |> Unsigned.UInt8.to_int |> kind_of_c in
    let of_ = Ctypes.getf struct_ W.Val.of_ in
    match (kind : Val.Kind.packed) with
    | P Int32 -> Val.Int32 (Ctypes.getf of_ W.Val_union.i32 |> Int32.to_int)
    | P Int64 -> Int64 (Ctypes.getf of_ W.Val_union.i64 |> Int64.to_int)
    | P Float32 -> Float32 (Ctypes.getf of_ W.Val_union.f32)
    | P Float64 -> Float64 (Ctypes.getf of_ W.Val_union.f64)
    | P Any_ref -> failwith "externref return values are not yet supported in v41 bindings"
    | P Func_ref -> failwith "func_ref returned results are not supported"

  let to_struct t struct_ =
    let kind = Val.kind t |> kind_to_c |> Unsigned.UInt8.of_int in
    Ctypes.setf struct_ W.Val.kind kind;
    let of_ = Ctypes.getf struct_ W.Val.of_ in
    match t with
    | Int32 i -> Ctypes.setf of_ W.Val_union.i32 (Int32.of_int i)
    | Int64 i -> Ctypes.setf of_ W.Val_union.i64 (Int64.of_int i)
    | Float32 f -> Ctypes.setf of_ W.Val_union.f32 f
    | Float64 f -> Ctypes.setf of_ W.Val_union.f64 f
    | Extern_ref _ -> failwith "externref arguments are not yet supported in v41 bindings"

  let copy t ptr =
    match (t : Val.t) with
    | Extern_ref _ -> failwith "externref values are not yet supported in v41 bindings"
    | _ -> Ctypes.( !@ ) ptr |> to_struct t
end

module Func_type = struct
  (* Each call to val_type must create a FRESH wasm_valtype_t because
     wasm_valtype_vec_new/wasm_functype_new take ownership of the pointers. *)
  let val_type kind =
    V.kind_to_c kind |> Unsigned.UInt8.of_int |> W.Val_type.new_

  let create ~args ~results =
    let vec_list l =
      let count = List.length l in
      let vec = Ctypes.allocate_n W.Val_type.t ~count:(max count 1) in
      List.iteri (fun idx v -> Ctypes.(vec +@ idx <-@ val_type v)) l;
      let out = Ctypes.allocate_n W.Val_type_vec.struct_ ~count:1 in
      W.Val_type_vec.new_ out (Unsigned.Size_t.of_int count) vec;
      out
    in
    let args = vec_list args in
    let results = vec_list results in
    let t = W.Func_type.new_ args results in
    Gc.finalise W.Func_type.delete t;
    t
end

module Func = struct
  (* In v41, wasmtime_func_t is a small value struct. We store a copy on the heap. *)
  type t = W.Func.struct_ Ctypes.ptr

  (* Helper: create a raw callback closure for the C trampoline.
     The closure receives (args_ptr, results_ptr) as nativeints and
     returns a trap pointer as nativeint (0n = no trap). *)
  let make_raw_callback ~nargs ~nresults f =
    fun args_nativeint results_nativeint ->
      try
        let args_val =
          Ctypes.ptr_of_raw_address args_nativeint
          |> Ctypes.from_voidp W.Val.struct_
        in
        let results_val =
          Ctypes.ptr_of_raw_address results_nativeint
          |> Ctypes.from_voidp W.Val.struct_
        in
        let args =
          List.init nargs (fun idx ->
            Ctypes.( +@ ) args_val idx |> V.of_ptr)
        in
        let r = f args in
        if List.length r <> nresults then
          Printf.ksprintf failwith
            "callback returned %d values, expected %d"
            (List.length r) nresults;
        List.iteri
          (fun idx val_ -> V.copy val_ (Ctypes.( +@ ) results_val idx))
          r;
        0n
      with exn ->
        let msg = Printexc.to_string exn ^ "\000" in
        let trap =
          W.Trap.new_ msg (String.length msg |> Unsigned.Size_t.of_int)
        in
        Ctypes.raw_address_of_ptr (Ctypes.to_voidp trap)

  let of_func_list ~args ~results (store : Store.t) f =
    let func_type = Func_type.create ~args ~results in
    let nargs = List.length args in
    let nresults = List.length results in
    let raw_callback = make_raw_callback ~nargs ~nresults f in
    let root = Callback_ffi.root_create raw_callback in
    let env = Ctypes.ptr_of_raw_address root in
    let ret = Ctypes.allocate_n W.Func.struct_ ~count:1 in
    W.Wasmtime.func_new_with_env store.context func_type env Ctypes.null ret;
    (* No delete needed for wasmtime_func_t - it's a value type *)
    Gc.finalise
      (fun _ret ->
        Callback_ffi.root_release root;
        keep_alive (func_type, store))
      ret;
    ret

  let of_func_0_0 (store : Store.t) f =
    let func_type = W.Func_type.new_0_0 () in
    Gc.finalise W.Func_type.delete func_type;
    let raw_callback _args_nativeint _results_nativeint =
      try
        f ();
        0n
      with exn ->
        let msg = Printexc.to_string exn ^ "\000" in
        let trap =
          W.Trap.new_ msg (String.length msg |> Unsigned.Size_t.of_int)
        in
        Ctypes.raw_address_of_ptr (Ctypes.to_voidp trap)
    in
    let root = Callback_ffi.root_create raw_callback in
    let env = Ctypes.ptr_of_raw_address root in
    let ret = Ctypes.allocate_n W.Func.struct_ ~count:1 in
    W.Wasmtime.func_new_with_env store.context func_type env Ctypes.null ret;
    Gc.finalise
      (fun _ret ->
        Callback_ffi.root_release root;
        keep_alive (func_type, store))
      ret;
    ret

  let of_func ~args ~results store f =
    of_func_list
      ~args:(Val.Kind.pack_tuple args)
      ~results:(Val.Kind.pack_tuple results)
      store
      (fun input_tuple ->
        Val.Kind.unwrap_tuple args input_tuple |> f |> Val.Kind.wrap_tuple results)
end

module Memory = struct
  (* In v41, wasmtime_memory_t is a small value struct. *)
  type t = W.Memory.struct_ Ctypes.ptr

  let size_in_pages (store : Store.t) t =
    W.Wasmtime.memory_size store.context t |> Unsigned.UInt64.to_int

  let size_in_bytes (store : Store.t) t =
    W.Wasmtime.memory_data_size store.context t |> Unsigned.Size_t.to_int

  let grow (store : Store.t) t size =
    let prev_size = Ctypes.allocate_n Ctypes.uint64_t ~count:1 in
    let err =
      W.Wasmtime.memory_grow store.context t (Unsigned.UInt64.of_int size) prev_size
    in
    Ctypes.is_null err

  let to_string (store : Store.t) t ~pos ~len =
    if pos < 0 then Printf.sprintf "negative pos %d" pos |> invalid_arg;
    if len < 0 then Printf.sprintf "negative len %d" len |> invalid_arg;
    let size_in_bytes =
      W.Wasmtime.memory_data_size store.context t |> Unsigned.Size_t.to_int
    in
    if pos + len > size_in_bytes then
      Printf.sprintf "pos (%d) + len (%d) > size_in_bytes (%d)" pos len size_in_bytes
      |> invalid_arg;
    let ptr = W.Wasmtime.memory_data store.context t in
    let char_ptr = Ctypes.coerce (Ctypes.ptr Ctypes.uint8_t) (Ctypes.ptr Ctypes.char) ptr in
    let ptr = Ctypes.( +@ ) char_ptr pos in
    Ctypes.string_from_ptr ptr ~length:len

  let get (store : Store.t) t ~pos =
    if pos < 0 then Printf.sprintf "negative pos %d" pos |> invalid_arg;
    let size_in_bytes =
      W.Wasmtime.memory_data_size store.context t |> Unsigned.Size_t.to_int
    in
    if pos >= size_in_bytes then
      Printf.sprintf "pos (%d) >= size_in_bytes (%d)" pos size_in_bytes |> invalid_arg;
    let ptr = W.Wasmtime.memory_data store.context t in
    let v = Ctypes.( !@ ) (Ctypes.( +@ ) ptr pos) in
    Char.chr (Unsigned.UInt8.to_int v)

  let set (store : Store.t) t ~pos chr =
    if pos < 0 then Printf.sprintf "negative pos %d" pos |> invalid_arg;
    let size_in_bytes =
      W.Wasmtime.memory_data_size store.context t |> Unsigned.Size_t.to_int
    in
    if pos >= size_in_bytes then
      Printf.sprintf "pos (%d) >= size_in_bytes (%d)" pos size_in_bytes |> invalid_arg;
    let ptr = W.Wasmtime.memory_data store.context t in
    let p = Ctypes.( +@ ) ptr pos in
    Ctypes.(p <-@ Unsigned.UInt8.of_int (Char.code chr))

  let blit_string (store : Store.t) t ~pos (s : string) =
    let slen = String.length s in
    if slen = 0 then ()
    else begin
      if pos < 0 then Printf.sprintf "negative pos %d" pos |> invalid_arg;
      let size_in_bytes =
        W.Wasmtime.memory_data_size store.context t |> Unsigned.Size_t.to_int
      in
      if pos + slen > size_in_bytes then
        Printf.sprintf "pos (%d) + len (%d) > size_in_bytes (%d)" pos slen size_in_bytes
        |> invalid_arg;
      let ptr = W.Wasmtime.memory_data store.context t in
      let char_ptr =
        Ctypes.coerce (Ctypes.ptr Ctypes.uint8_t) (Ctypes.ptr Ctypes.char) ptr
      in
      let ba =
        Ctypes.bigarray_of_ptr Ctypes.array1 size_in_bytes Bigarray.char char_ptr
      in
      for i = 0 to slen - 1 do
        Bigarray.Array1.set ba (pos + i) (String.get s i)
      done
    end
end

module Extern = struct
  (* In v41, wasmtime_extern_t is a discriminated union struct *)
  type t = W.Extern.struct_ Ctypes.ptr

  let as_memory t =
    let s = Ctypes.( !@ ) t in
    let k = Ctypes.getf s W.Extern.kind |> Unsigned.UInt8.to_int in
    if k <> wasmtime_extern_memory then failwith "Extern.as_memory: not a memory";
    let u = Ctypes.getf s W.Extern.of_ in
    let mem_struct = Ctypes.getf u W.Extern_union.memory in
    let p = Ctypes.allocate W.Memory.struct_ mem_struct in
    p

  let as_func t =
    let s = Ctypes.( !@ ) t in
    let k = Ctypes.getf s W.Extern.kind |> Unsigned.UInt8.to_int in
    if k <> wasmtime_extern_func then failwith "Extern.as_func: not a func";
    let u = Ctypes.getf s W.Extern.of_ in
    let func_struct = Ctypes.getf u W.Extern_union.func in
    let p = Ctypes.allocate W.Func.struct_ func_struct in
    p

  let func_as func =
    let ext = Ctypes.allocate_n W.Extern.struct_ ~count:1 in
    let s = Ctypes.( !@ ) ext in
    Ctypes.setf s W.Extern.kind (Unsigned.UInt8.of_int wasmtime_extern_func);
    let u = Ctypes.getf s W.Extern.of_ in
    Ctypes.setf u W.Extern_union.func (Ctypes.( !@ ) func);
    (* Write back *)
    Ctypes.(ext <-@ s);
    ext
end

module Instance = struct
  type t = W.Instance.struct_ Ctypes.ptr

  let exports (store : Store.t) t =
    let rec loop acc idx =
      let name_ptr = Ctypes.allocate_n (Ctypes.ptr Ctypes.char) ~count:1 in
      let name_len = Ctypes.allocate_n Ctypes.size_t ~count:1 in
      let item = Ctypes.allocate_n W.Extern.struct_ ~count:1 in
      let found =
        W.Instance.export_nth
          store.context t (Unsigned.Size_t.of_int idx) name_ptr name_len item
      in
      if found then loop (item :: acc) (idx + 1)
      else List.rev acc
    in
    loop [] 0
end

module Wasi = struct
  type stdin = Inherit | Bytes of string | File of string

  type capture = {
    cbuf : W.Capture_buf.t;
    mutable result : string option;
  }

  type stdout = Inherit | File of string | Capture of capture
  type stderr = Inherit | File of string | Capture of capture

  let create_capture () =
    let cbuf = W.Capture_buf.new_ () in
    if Ctypes.is_null cbuf then failwith "wasi_capture_buf_new returned null";
    { cbuf; result = None }

  let capture_contents t =
    match t.result with
    | Some s -> s
    | None ->
      let len = W.Capture_buf.len t.cbuf |> Unsigned.Size_t.to_int in
      let s =
        if len > 0 then
          let data = W.Capture_buf.data t.cbuf in
          Ctypes.string_from_ptr
            (Ctypes.coerce Ctypes.(ptr uchar) Ctypes.(ptr char) data)
            ~length:len
        else ""
      in
      W.Capture_buf.free t.cbuf;
      t.result <- Some s;
      s

  let configure
      ?(inherit_argv = false)
      ?(inherit_env = false)
      ?stdin
      ?stdout
      ?stderr
      ?(preopen_dirs = [])
      (store : Store.t)
    =
    let config = W.Wasi_config.new_ () in
    if Ctypes.is_null config then failwith "Wasi_config.new returned null";
    if inherit_argv then W.Wasi_config.inherit_argv config;
    if inherit_env then W.Wasi_config.inherit_env config;
    (match (stdin : stdin option) with
     | Some Inherit -> W.Wasi_config.inherit_stdin config
     | Some (Bytes bytes) ->
       (* Allocate byte vec without GC finalizer: C takes ownership *)
       let t = Ctypes.allocate_n W.Byte_vec.struct_ ~count:1 in
       W.Byte_vec.new_ t (String.length bytes |> Unsigned.Size_t.of_int) bytes;
       W.Wasi_config.set_stdin_bytes config t
     | Some (File path) ->
       if not (W.Wasi_config.set_stdin_file config path) then
         failwith (Printf.sprintf "wasi_config_set_stdin_file failed: %s" path)
     | None -> ());
    (match (stdout : stdout option) with
     | Some Inherit -> W.Wasi_config.inherit_stdout config
     | Some (File path) ->
       if not (W.Wasi_config.set_stdout_file config path) then
         failwith (Printf.sprintf "wasi_config_set_stdout_file failed: %s" path)
     | Some (Capture cap) ->
       W.Capture_buf.set_stdout config cap.cbuf
     | None -> ());
    (match (stderr : stderr option) with
     | Some Inherit -> W.Wasi_config.inherit_stderr config
     | Some (File path) ->
       if not (W.Wasi_config.set_stderr_file config path) then
         failwith (Printf.sprintf "wasi_config_set_stderr_file failed: %s" path)
     | Some (Capture cap) ->
       W.Capture_buf.set_stderr config cap.cbuf
     | None -> ());
    List.iter
      (fun (host_path, guest_path) ->
        (* dir_perms = READ|WRITE = 3, file_perms = READ|WRITE = 3 *)
        let _ok =
          W.Wasi_config.preopen_dir config host_path guest_path
            (Unsigned.Size_t.of_int 3) (Unsigned.Size_t.of_int 3)
        in
        ())
      preopen_dirs;
    (* Ownership of config is passed to context_set_wasi *)
    let fail_on_error error =
      if not (Ctypes.is_null error) then begin
        let message =
          Byte_vec.with_finalise ~f:(fun message -> W.Error.message error message)
          |> Byte_vec.to_string
        in
        W.Error.delete error;
        failwith message
      end
    in
    W.Wasmtime.context_set_wasi store.context config |> fail_on_error
end

module Wasmtime = struct
  let fail_on_error error =
    if not (Ctypes.is_null error) then begin
      let message =
        Byte_vec.with_finalise ~f:(fun message -> W.Error.message error message)
        |> Byte_vec.to_string
      in
      W.Error.delete error;
      failwith message
    end

  let wat_to_wasm ~wat =
    let wat_str = Byte_vec.to_string wat in
    let wat_len = Byte_vec.length wat in
    Byte_vec.with_finalise ~f:(fun wasm ->
      W.Wasmtime.wat2wasm wat_str (Unsigned.Size_t.of_int wat_len) wasm
      |> fail_on_error)

  let new_module engine ~wasm =
    let wasm_data = Byte_vec.to_string wasm in
    let wasm_len = Byte_vec.length wasm in
    let buf = Ctypes.allocate_n Ctypes.uint8_t ~count:wasm_len in
    for i = 0 to wasm_len - 1 do
      Ctypes.(buf +@ i <-@ Unsigned.UInt8.of_int (Char.code wasm_data.[i]))
    done;
    let modl = Ctypes.allocate W.Module.t (Ctypes.from_voidp W.Module.struct_ Ctypes.null) in
    W.Wasmtime.new_module engine buf (Unsigned.Size_t.of_int wasm_len) modl
    |> fail_on_error;
    let modl = Ctypes.( !@ ) modl in
    if Ctypes.is_null modl then failwith "new_module returned null";
    Gc.finalise
      (fun modl ->
        keep_alive engine;
        W.Module.delete modl)
      modl;
    modl

  let new_instance ?(imports = []) (store : Store.t) modl =
    let instance = Ctypes.allocate_n W.Instance.struct_ ~count:1 in
    let trap =
      Ctypes.allocate W.Trap.t (Ctypes.from_voidp W.Trap.struct_ Ctypes.null)
    in
    let n_imports = List.length imports in
    let imports_arr = Ctypes.allocate_n W.Extern.struct_ ~count:(max n_imports 1) in
    List.iteri
      (fun idx ext -> Ctypes.(imports_arr +@ idx <-@ Ctypes.( !@ ) ext))
      imports;
    W.Wasmtime.new_instance
      store.context
      modl
      imports_arr
      (Unsigned.Size_t.of_int n_imports)
      instance
      trap
    |> fail_on_error;
    keep_alive imports_arr;
    Ctypes.( !@ ) trap |> Trap_.maybe_fail;
    instance

  let func_call_list (store : Store.t) func args ~n_outputs =
    let trap =
      Ctypes.allocate W.Trap.t (Ctypes.from_voidp W.Trap.struct_ Ctypes.null)
    in
    let n_args = List.length args in
    let args_ = Ctypes.allocate_n W.Val.struct_ ~count:(max n_args 1) in
    List.iteri (fun idx val_ -> V.copy val_ (Ctypes.( +@ ) args_ idx)) args;
    let outputs = Ctypes.allocate_n W.Val.struct_ ~count:(max n_outputs 1) in
    W.Wasmtime.func_call
      store.context
      func
      args_
      (Unsigned.Size_t.of_int n_args)
      outputs
      (Unsigned.Size_t.of_int n_outputs)
      trap
    |> fail_on_error;
    Ctypes.( !@ ) trap |> Trap_.maybe_fail;
    List.init n_outputs (fun idx -> Ctypes.( +@ ) outputs idx |> V.of_ptr)

  let func_call0 store func args =
    match func_call_list store func args ~n_outputs:0 with
    | [] -> ()
    | l -> Printf.ksprintf failwith "expected no output, got %d" (List.length l)

  let func_call1 store func args =
    match func_call_list store func args ~n_outputs:1 with
    | [ res ] -> res
    | l -> Printf.ksprintf failwith "expected a single output, got %d" (List.length l)

  let func_call2 store func args =
    match func_call_list store func args ~n_outputs:2 with
    | [ res1; res2 ] -> (res1, res2)
    | l -> Printf.ksprintf failwith "expected two outputs, got %d" (List.length l)

  let func_call ~args ~results store func input_tuple =
    Val.Kind.wrap_tuple args input_tuple
    |> func_call_list store func ~n_outputs:(Val.Kind.arity results)
    |> Val.Kind.unwrap_tuple results

  module Linker = struct
    type t = W.Wasmtime.Linker.t

    let create engine =
      let t = W.Wasmtime.Linker.new_ engine in
      Gc.finalise
        (fun t ->
          keep_alive engine;
          W.Wasmtime.Linker.delete t)
        t;
      t

    let define_wasi t =
      W.Wasmtime.Linker.define_wasi t |> fail_on_error

    let define_instance t (store : Store.t) ~name instance =
      let name_len = String.length name in
      W.Wasmtime.Linker.define_instance
        t store.context name (Unsigned.Size_t.of_int name_len) instance
      |> fail_on_error

    let instantiate t (store : Store.t) modl =
      let instance = Ctypes.allocate_n W.Instance.struct_ ~count:1 in
      let trap =
        Ctypes.allocate W.Trap.t (Ctypes.from_voidp W.Trap.struct_ Ctypes.null)
      in
      W.Wasmtime.Linker.instantiate t store.context modl instance trap
      |> fail_on_error;
      Ctypes.( !@ ) trap |> Trap_.maybe_fail;
      instance

    let module_ t (store : Store.t) ~name modl =
      let name_len = String.length name in
      W.Wasmtime.Linker.module_ t store.context name (Unsigned.Size_t.of_int name_len) modl
      |> fail_on_error

    let get_default t (store : Store.t) ~name =
      let func = Ctypes.allocate_n W.Func.struct_ ~count:1 in
      let name_len = String.length name in
      W.Wasmtime.Linker.get_default
        t store.context name (Unsigned.Size_t.of_int name_len) func
      |> fail_on_error;
      func
  end
end
