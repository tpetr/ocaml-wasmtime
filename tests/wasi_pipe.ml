module W = Wasmtime.Wrappers

(* A WASI module that reads from stdin and writes what it read to stdout.
   Reads up to 128 bytes from fd 0 (stdin), then writes them to fd 1 (stdout). *)
let echo_wat =
  {|
(module
    (import "wasi_snapshot_preview1" "fd_read"
        (func $fd_read (param i32 i32 i32 i32) (result i32)))
    (import "wasi_snapshot_preview1" "fd_write"
        (func $fd_write (param i32 i32 i32 i32) (result i32)))

    (memory 1)
    (export "memory" (memory 0))

    ;; Layout:
    ;; 0-3:   iov_base (pointer to buffer)
    ;; 4-7:   iov_len  (buffer length)
    ;; 8-11:  nread/nwritten result
    ;; 64+:   data buffer

    (func $main (export "_start")
        ;; Set up iov for reading: buffer at offset 64, length 128
        (i32.store (i32.const 0) (i32.const 64))   ;; iov_base = 64
        (i32.store (i32.const 4) (i32.const 128))   ;; iov_len = 128

        ;; fd_read(stdin=0, iovs=0, iovs_len=1, nread=8)
        (call $fd_read
            (i32.const 0)   ;; fd 0 = stdin
            (i32.const 0)   ;; *iovs
            (i32.const 1)   ;; iovs_len
            (i32.const 8)   ;; nread result location
        )
        drop

        ;; Now write the bytes we read to stdout
        ;; Reuse iov_base (still 64), but set iov_len to nread
        (i32.store (i32.const 4) (i32.load (i32.const 8)))  ;; iov_len = nread

        ;; fd_write(stdout=1, iovs=0, iovs_len=1, nwritten=8)
        (call $fd_write
            (i32.const 1)   ;; fd 1 = stdout
            (i32.const 0)   ;; *iovs
            (i32.const 1)   ;; iovs_len
            (i32.const 8)   ;; nwritten result location
        )
        drop
    )
)
|}

let read_file path =
  let ic = open_in_bin path in
  let result = In_channel.input_all ic in
  close_in ic;
  result

let write_file path contents =
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let run_echo ~configure =
  let engine = W.Engine.create () in
  let store = W.Store.create engine in
  let wasm = W.Wasmtime.wat_to_wasm ~wat:(W.Byte_vec.of_string echo_wat) in
  let modl = W.Wasmtime.new_module engine ~wasm in
  configure store;
  let linker = W.Wasmtime.Linker.create engine in
  W.Wasmtime.Linker.define_wasi linker;
  W.Wasmtime.Linker.module_ linker store ~name:"" modl;
  let wasi_func = W.Wasmtime.Linker.get_default linker store ~name:"" in
  W.Wasmtime.func_call0 store wasi_func []

let check name ~expected ~actual =
  if actual = expected then
    Printf.printf "%s: OK\n" name
  else begin
    Printf.eprintf "FAIL %s: expected %S, got %S\n" name expected actual;
    exit 1
  end

let () =
  (* Test 1: Bytes stdin + File stdout *)
  let stdout_path = Filename.temp_file "wasi_pipe_test" ".txt" in
  run_echo ~configure:(fun store ->
    W.Wasi.configure store ~stdin:(Bytes "hello from ocaml") ~stdout:(File stdout_path));
  check "Bytes stdin + File stdout" ~expected:"hello from ocaml" ~actual:(read_file stdout_path);
  Sys.remove stdout_path;

  (* Test 2: File stdin + File stdout *)
  let stdin_path = Filename.temp_file "wasi_pipe_stdin" ".txt" in
  let stdout_path = Filename.temp_file "wasi_pipe_stdout" ".txt" in
  write_file stdin_path "hello from file";
  run_echo ~configure:(fun store ->
    W.Wasi.configure store ~stdin:(File stdin_path) ~stdout:(File stdout_path));
  check "File stdin + File stdout" ~expected:"hello from file" ~actual:(read_file stdout_path);
  Sys.remove stdin_path;
  Sys.remove stdout_path;

  (* Test 3: Bytes stdin + Capture stdout *)
  let out = W.Wasi.create_capture () in
  run_echo ~configure:(fun store ->
    W.Wasi.configure store ~stdin:(Bytes "hello to capture") ~stdout:(Capture out));
  check "Bytes stdin + Capture stdout"
    ~expected:"hello to capture" ~actual:(W.Wasi.capture_contents out);

  (* Test 4: Bytes stdin + Capture stderr (write to fd 2) *)
  let stderr_wat =
    {|
(module
    (import "wasi_snapshot_preview1" "fd_read"
        (func $fd_read (param i32 i32 i32 i32) (result i32)))
    (import "wasi_snapshot_preview1" "fd_write"
        (func $fd_write (param i32 i32 i32 i32) (result i32)))
    (memory 1)
    (export "memory" (memory 0))
    (func $main (export "_start")
        (i32.store (i32.const 0) (i32.const 64))
        (i32.store (i32.const 4) (i32.const 128))
        (call $fd_read (i32.const 0) (i32.const 0) (i32.const 1) (i32.const 8))
        drop
        (i32.store (i32.const 4) (i32.load (i32.const 8)))
        ;; Write to fd 2 (stderr) instead of fd 1
        (call $fd_write (i32.const 2) (i32.const 0) (i32.const 1) (i32.const 8))
        drop))
|}
  in
  let err = W.Wasi.create_capture () in
  let engine = W.Engine.create () in
  let store = W.Store.create engine in
  let wasm = W.Wasmtime.wat_to_wasm ~wat:(W.Byte_vec.of_string stderr_wat) in
  let modl = W.Wasmtime.new_module engine ~wasm in
  W.Wasi.configure store ~stdin:(Bytes "hello to stderr") ~stderr:(Capture err);
  let linker = W.Wasmtime.Linker.create engine in
  W.Wasmtime.Linker.define_wasi linker;
  W.Wasmtime.Linker.module_ linker store ~name:"" modl;
  let wasi_func = W.Wasmtime.Linker.get_default linker store ~name:"" in
  W.Wasmtime.func_call0 store wasi_func [];
  check "Bytes stdin + Capture stderr"
    ~expected:"hello to stderr" ~actual:(W.Wasi.capture_contents err)
