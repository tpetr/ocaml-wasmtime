(* A command line tool taking as input a wasm file and executing it. *)
module W = Wasmtime.Wrappers

let run ~filename =
  let engine = W.Engine.create () in
  let store = W.Store.create engine in
  let wasm =
    if Filename.check_suffix filename ".wat"
    then (
      let content = In_channel.with_open_bin filename In_channel.input_all in
      W.Wasmtime.wat_to_wasm ~wat:(W.Byte_vec.of_string content))
    else if Filename.check_suffix filename ".wasm"
    then In_channel.with_open_bin filename In_channel.input_all |> W.Byte_vec.of_string
    else failwith "the filename has to end with .wat or .wasm"
  in
  let modl = W.Wasmtime.new_module engine ~wasm in
  W.Wasi.configure
    store
    ~inherit_argv:true
    ~inherit_env:true
    ~inherit_stdin:true
    ~inherit_stderr:true
    ~inherit_stdout:true;
  let linker = W.Wasmtime.Linker.create engine in
  W.Wasmtime.Linker.define_wasi linker;
  W.Wasmtime.Linker.module_ linker store ~name:"foo" modl;
  let wasi_func = W.Wasmtime.Linker.get_default linker store ~name:"foo" in
  W.Wasmtime.func_call0 store wasi_func []

let () =
  match Sys.argv with
  | [| _; filename |] -> run ~filename
  | _ -> Printf.ksprintf failwith "usage: %s file.{wasm,wat}" Sys.argv.(0)
