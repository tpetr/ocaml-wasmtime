module C = Configurator.V1

let empty_flags = { C.Pkg_config.cflags = []; libs = [] }

let wasmtime_flags () =
  let config ~lib_dir =
    let cflags = ["-isystem"; Filename.concat lib_dir "include"] in
    let libs = [Filename.concat (Filename.concat lib_dir "lib") "libwasmtime.a"] in
    { C.Pkg_config.cflags; libs }
  in
  match Sys.getenv_opt "LIBWASMTIME" with
  | Some lib_dir -> config ~lib_dir
  | None ->
    (* Try _wasmtime/ in project root *)
    let local_dir = Filename.concat (Sys.getcwd ()) "_wasmtime" in
    if Sys.file_exists local_dir then config ~lib_dir:local_dir
    else
      (match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
       | Some prefix ->
         let lib_dir = Filename.concat (Filename.concat prefix "lib") "libwasmtime" in
         if Sys.file_exists lib_dir then config ~lib_dir
         else empty_flags
       | None -> empty_flags)

let system_libs () =
  let os =
    let ic = Unix.open_process_in "uname -s" in
    let os = input_line ic in (* nosemgrep *)
    let os = String.trim os in
    ignore (Unix.close_process_in ic);
    String.lowercase_ascii os
  in
  match os with
  | "linux" -> ["-lpthread"; "-ldl"; "-lm"]
  | "darwin" -> ["-lpthread"; "-lm"]
  | _ -> ["-lpthread"; "-lm"]

let () =
  C.main ~name:"wasmtime-config" (fun _c ->
    let wasmtime_flags =
      try wasmtime_flags () with
      | _ -> empty_flags
    in
    let sys_libs = try system_libs () with _ -> ["-lpthread"; "-lm"] in
    C.Flags.write_sexp "c_flags.sexp" wasmtime_flags.cflags;
    C.Flags.write_sexp "c_library_flags.sexp" (wasmtime_flags.libs @ sys_libs))
