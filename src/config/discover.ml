module C = Configurator.V1

let empty_flags = { C.Pkg_config.cflags = []; libs = [] }

let resolve_path p =
  if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p

let wasmtime_dir = ref ""

let wasmtime_flags () =
  let config ~lib_dir =
    let lib_dir = resolve_path lib_dir in
    let cflags = ["-isystem"; Filename.concat lib_dir "include"] in
    let lib_dir_path = Filename.concat lib_dir "lib" in
    let lib_file =
      (* Try Windows lib names first, then Unix *)
      let candidates = [
        "wasmtime.dll.lib"; "wasmtime.lib"; "libwasmtime.a"
      ] in
      match List.find_opt (fun f -> Sys.file_exists (Filename.concat lib_dir_path f)) candidates with
      | Some f -> Filename.concat lib_dir_path f
      | None -> Filename.concat lib_dir_path "libwasmtime.a"
    in
    let libs = [lib_file] in
    { C.Pkg_config.cflags; libs }
  in
  (* Priority: LIBWASMTIME env > --wasmtime-dir flag > OPAM_SWITCH_PREFIX *)
  match Sys.getenv_opt "LIBWASMTIME" with
  | Some lib_dir -> config ~lib_dir
  | None ->
    if !wasmtime_dir <> "" then config ~lib_dir:!wasmtime_dir
    else
      (match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
       | Some prefix ->
         let lib_dir = Filename.concat (Filename.concat prefix "lib") "libwasmtime" in
         if Sys.file_exists lib_dir then config ~lib_dir
         else empty_flags
       | None -> empty_flags)

let system_libs () =
  match Sys.os_type with
  | "Win32" | "Cygwin" -> []
  | _ ->
    let is_linux =
      try
        let ic = Unix.open_process_in "uname -s" in
        let os = String.trim (input_line ic) in (* nosemgrep *)
        ignore (Unix.close_process_in ic);
        String.lowercase_ascii os = "linux"
      with _ -> false
    in
    if is_linux then ["-ldl"; "-lm"] else ["-lm"]

let () =
  C.main ~name:"wasmtime-config"
    ~args:[("--wasmtime-dir", Arg.Set_string wasmtime_dir,
            "DIR Path to wasmtime C API directory")]
    (fun _c ->
    let wasmtime_flags =
      try wasmtime_flags () with
      | _ -> empty_flags
    in
    let sys_libs = try system_libs () with _ -> ["-lm"] in
    C.Flags.write_sexp "c_flags.sexp" wasmtime_flags.cflags;
    C.Flags.write_sexp "c_library_flags.sexp" (wasmtime_flags.libs @ sys_libs))
