module W = Wasmtime.Wrappers
module V = Wasmtime.Val

let wat =
  {|
(module
  (table $table (export "table") 10 externref)

  (global $global (export "global") (mut externref) (ref.null extern))

  (func (export "func") (param externref) (result externref)
    local.get 0
  )
)
|}

let () =
  let engine = W.Engine.create ~reference_types:true () in
  let store = W.Store.create engine in
  let wasm = W.Wasmtime.wat_to_wasm ~wat:(W.Byte_vec.of_string wat) in
  let modl = W.Wasmtime.new_module engine ~wasm in
  let instance = W.Wasmtime.new_instance store modl in
  let _table, _global, func =
    match W.Instance.exports store instance with
    | [ table; global; extern_ ] -> (table, global, W.Extern.as_func extern_)
    | exports ->
      Printf.ksprintf failwith "expected three exports, got %d" (List.length exports)
  in
  let externref = Wasmtime.Extern_ref.of_string "hello world!" in
  let _ref = W.Wasmtime.func_call1 store func [ Extern_ref externref ] in
  Printf.printf "externref test passed\n"
