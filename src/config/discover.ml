module C = Configurator.V1

let empty_flags = { C.Pkg_config.cflags = []; libs = [] }

let resolve_path p =
  if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p

let wasmtime_dir = ref ""
let used_wasmtime_dir_fallback = ref false

(* Find wasmtime installed via Homebrew (macOS) *)
let homebrew_prefix () =
  let check_prefix p =
    if Sys.file_exists (Filename.concat (Filename.concat p "lib") "libwasmtime.a")
    then Some p else None
  in
  match Sys.getenv_opt "HOMEBREW_PREFIX" with
  | Some prefix -> check_prefix prefix
  | None ->
    (* Default Homebrew paths: arm64 and x86_64 *)
    List.find_map check_prefix ["/opt/homebrew"; "/usr/local"]

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
  (* Priority: LIBWASMTIME env > Homebrew > OPAM_SWITCH_PREFIX > --wasmtime-dir flag *)
  match Sys.getenv_opt "LIBWASMTIME" with
  | Some lib_dir -> config ~lib_dir
  | None ->
    let from_brew = match homebrew_prefix () with
      | Some prefix -> Some (config ~lib_dir:prefix)
      | None -> None
    in
    let from_prefix = match from_brew with
      | Some _ -> from_brew
      | None ->
        (match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
         | Some prefix ->
           let lib_dir = Filename.concat (Filename.concat prefix "lib") "libwasmtime" in
           if Sys.file_exists lib_dir then Some (config ~lib_dir)
           else None
         | None -> None)
    in
    (match from_prefix with
     | Some flags -> flags
     | None ->
       if !wasmtime_dir <> "" then begin
         used_wasmtime_dir_fallback := true;
         config ~lib_dir:!wasmtime_dir
       end
       else empty_flags)

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
    (* When building as an opam package and falling through to --wasmtime-dir,
       the resolved path points into the ephemeral opam build directory which
       won't exist after installation. Override c_library_flags to use the
       install path ($OPAM_SWITCH_PREFIX/lib/libwasmtime/) so that downstream
       packages can find libwasmtime at its installed location. *)
    let libs =
      if !used_wasmtime_dir_fallback
         && Sys.getenv_opt "OPAM_PACKAGE_NAME" <> None then
        match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
        | Some prefix ->
          let install_lib_dir =
            List.fold_left Filename.concat prefix ["lib"; "libwasmtime"; "lib"]
          in
          let lib_name = match Sys.os_type with
            | "Win32" | "Cygwin" -> "wasmtime.dll.lib"
            | _ -> "libwasmtime.a"
          in
          [Filename.concat install_lib_dir lib_name]
        | None -> wasmtime_flags.libs
      else
        wasmtime_flags.libs
    in
    C.Flags.write_sexp "c_flags.sexp" wasmtime_flags.cflags;
    C.Flags.write_sexp "c_library_flags.sexp" (libs @ sys_libs))
