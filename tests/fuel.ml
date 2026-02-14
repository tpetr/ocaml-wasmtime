module W = Wasmtime.Wrappers
module V = Wasmtime.Val

(* A simple loop that burns fuel *)
let loop_wat =
  {|
(module
  (func (export "loop") (param $n i32) (result i32)
    (local $i i32)
    (local.set $i (i32.const 0))
    (block $break
      (loop $continue
        (br_if $break (i32.ge_u (local.get $i) (local.get $n)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $continue)
      )
    )
    (local.get $i)
  )
)
|}

let () =
  (* Create engine with fuel enabled *)
  let engine = W.Engine.create ~consume_fuel:true () in
  let store = W.Store.create engine in

  (* Set fuel and verify we can read it back *)
  W.Store.set_fuel store 10000;
  let fuel = W.Store.get_fuel store in
  Printf.printf "initial fuel: %d\n" fuel;
  assert (fuel = 10000);

  (* Compile and instantiate a module with a loop *)
  let wasm = W.Wasmtime.wat_to_wasm ~wat:(W.Byte_vec.of_string loop_wat) in
  let modl = W.Wasmtime.new_module engine ~wasm in
  let instance = W.Wasmtime.new_instance store modl in
  let loop_func =
    match W.Instance.exports store instance with
    | [ extern ] -> W.Extern.as_func extern
    | _ -> failwith "expected a single export"
  in

  (* Call with enough fuel - should succeed *)
  W.Store.set_fuel store 10000;
  let result = W.Wasmtime.func_call1 store loop_func [ V.Int32 5 ] in
  (match result with
   | V.Int32 n ->
     Printf.printf "loop(5) = %d\n" n;
     assert (n = 5)
   | _ -> failwith "unexpected result type");

  (* Verify fuel was consumed *)
  let remaining = W.Store.get_fuel store in
  Printf.printf "fuel remaining after loop(5): %d\n" remaining;
  assert (remaining < 10000);

  (* Call with very little fuel - should trap *)
  W.Store.set_fuel store 1;
  (try
     let _ = W.Wasmtime.func_call1 store loop_func [ V.Int32 1000000 ] in
     failwith "should have trapped"
   with W.Trap { message } ->
     Printf.printf "trapped as expected: %s\n"
       (String.sub message 0 (min 60 (String.length message))));

  Printf.printf "fuel test passed!\n"
