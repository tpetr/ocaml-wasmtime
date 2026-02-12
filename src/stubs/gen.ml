let () =
  let fmt file = Format.formatter_of_out_channel (open_out_bin file) in
  let fmt_c = fmt "wasmtime_stubs.c" in
  let fmt_ml = fmt "wasmtime_generated.ml" in
  Format.fprintf fmt_c "#include <wasm.h>\n";
  Format.fprintf fmt_c "#include <wasi.h>\n";
  Format.fprintf fmt_c "#include <wasmtime.h>\n";
  (* Forward declarations for wasi_capture.c *)
  Format.fprintf fmt_c "typedef struct wasi_capture_buf_t wasi_capture_buf_t;\n";
  Format.fprintf fmt_c "wasi_capture_buf_t *wasi_capture_buf_new(void);\n";
  Format.fprintf fmt_c "void wasi_capture_set_stdout(wasi_config_t *, wasi_capture_buf_t *);\n";
  Format.fprintf fmt_c "void wasi_capture_set_stderr(wasi_config_t *, wasi_capture_buf_t *);\n";
  Format.fprintf fmt_c "unsigned char *wasi_capture_buf_data(wasi_capture_buf_t *);\n";
  Format.fprintf fmt_c "size_t wasi_capture_buf_len(wasi_capture_buf_t *);\n";
  Format.fprintf fmt_c "void wasi_capture_buf_free(wasi_capture_buf_t *);\n";
  Cstubs.write_c fmt_c ~prefix:"caml_" (module Bindings.C);
  Cstubs.write_ml fmt_ml ~prefix:"caml_" (module Bindings.C);
  flush_all ()
