(* Cross-platform setup script for wasmtime C API.
   Run: ocaml scripts/setup.ml

   When run via opam, the platform-appropriate archive is pre-downloaded
   by extra-source. When run locally, falls back to downloading via curl. *)

let version = "41.0.3"
let dest = "_wasmtime"

let () =
  if Sys.file_exists dest && Sys.file_exists (Filename.concat (Filename.concat dest "lib") "libwasmtime.a") then (
    Printf.printf "wasmtime already available\n";
    exit 0)

let run cmd =
  Printf.printf "+ %s\n%!" cmd;
  let rc = Sys.command cmd in
  if rc <> 0 then (
    Printf.eprintf "Command failed (exit %d): %s\n" rc cmd;
    exit 1)

(* Find a pre-downloaded archive matching wasmtime-c-api-*.{tar.xz,zip} *)
let find_archive () =
  let files = Sys.readdir "." in
  let found = ref None in
  Array.iter (fun f ->
    if !found = None then
      let prefix = "wasmtime-c-api-" in
      if String.length f > String.length prefix
         && String.sub f 0 (String.length prefix) = prefix
         && (Filename.check_suffix f ".tar.xz" || Filename.check_suffix f ".zip")
      then found := Some f
  ) files;
  !found

let extract_tar_xz file =
  run (Printf.sprintf "mkdir -p %s" dest);
  run (Printf.sprintf "tar xJf %s --strip-components=1 -C %s" file dest)

let extract_zip file =
  run (Printf.sprintf "mkdir -p %s" dest);
  (* Windows 10+ tar handles zip; use a temp dir since zip has no --strip-components *)
  let tmp = "_wasmtime_tmp" in
  run (Printf.sprintf "tar -xf %s -C ." file);
  (* The zip extracts to a directory like wasmtime-v41.0.3-x86_64-mingw-c-api/ *)
  let files = Sys.readdir "." in
  let inner = ref "" in
  Array.iter (fun f ->
    if Sys.is_directory f
       && String.length f > 9
       && String.sub f 0 9 = "wasmtime-"
       && f <> dest
       && f <> tmp
    then inner := f
  ) files;
  if !inner = "" then (
    Printf.eprintf "Could not find extracted directory from %s\n" file;
    exit 1);
  (* Rename extracted dir to dest *)
  (if Sys.file_exists dest then
     if Sys.win32 then run (Printf.sprintf "rmdir /s /q %s" dest)
     else run (Printf.sprintf "rm -rf %s" dest));
  if Sys.win32 then run (Printf.sprintf "move %s %s" !inner dest)
  else run (Printf.sprintf "mv %s %s" !inner dest)

let download () =
  let os, arch =
    if Sys.win32 then ("mingw", "x86_64")
    else
      let read_cmd cmd =
        let tmp = Filename.temp_file "wasmtime_setup" ".txt" in
        let rc = Sys.command (Printf.sprintf "%s > %s" cmd tmp) in
        if rc <> 0 then (Printf.eprintf "Command failed: %s\n" cmd; exit 1);
        let ic = open_in_bin tmp in
        let s = String.trim (input_line ic) in (* nosemgrep *)
        close_in ic;
        Sys.remove tmp;
        s
      in
      let os = match String.lowercase_ascii (read_cmd "uname -s") with
        | "darwin" -> "macos"
        | s -> s
      in
      let arch = match read_cmd "uname -m" with
        | "arm64" -> "aarch64"
        | a -> a
      in
      (os, arch)
  in
  let ext = if Sys.win32 then "zip" else "tar.xz" in
  let file = Printf.sprintf "wasmtime-v%s-%s-%s-c-api.%s" version arch os ext in
  let url = Printf.sprintf
    "https://github.com/bytecodealliance/wasmtime/releases/download/v%s/%s"
    version file in
  if Sys.win32 then (
    run (Printf.sprintf "curl -sLO %s" url);
    extract_zip file
  ) else (
    run (Printf.sprintf "mkdir -p %s" dest);
    run (Printf.sprintf "curl -sL %s | tar xJ --strip-components=1 -C %s" url dest)
  );
  Printf.printf "Done: %s/lib/libwasmtime.a\n" dest

let () =
  match find_archive () with
  | Some file when Filename.check_suffix file ".zip" ->
    Printf.printf "Extracting %s...\n%!" file;
    extract_zip file;
    Printf.printf "Done\n"
  | Some file ->
    Printf.printf "Extracting %s...\n%!" file;
    extract_tar_xz file;
    Printf.printf "Done\n"
  | None ->
    Printf.printf "No archive found, downloading wasmtime v%s...\n%!" version;
    download ()
