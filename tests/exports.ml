module W = Wasmtime.Wrappers

let wat =
  {|
(module
  (func $add (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.add)
  (memory (export "mem") 1)
  (export "add" (func $add))
)
|}

let string_of_kind = function
  | W.Module.Func -> "func"
  | Global -> "global"
  | Table -> "table"
  | Memory -> "memory"

let () =
  let engine = W.Engine.create () in
  let wasm = W.Wasmtime.wat_to_wasm ~wat:(W.Byte_vec.of_string wat) in
  let modl = W.Wasmtime.new_module engine ~wasm in
  let exports = W.Module.exports modl in
  List.iter
    (fun (e : W.Module.export) ->
      Printf.printf "export: %s (%s)\n" e.name (string_of_kind e.kind))
    exports;
  let n = List.length exports in
  assert (n = 2);
  let mem_export = List.nth exports 0 in
  assert (mem_export.name = "mem");
  assert (mem_export.kind = Memory);
  let add_export = List.nth exports 1 in
  assert (add_export.name = "add");
  assert (add_export.kind = Func);
  Printf.printf "Module.exports test passed (%d exports)\n" n
