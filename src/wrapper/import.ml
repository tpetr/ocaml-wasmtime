module W = Bindings.C (Wasmtime_generated)

exception Trap of { message : string }

let zero = Sys.opaque_identity (int_of_string "0")
let rec keep_alive o = if zero <> 0 then keep_alive (Sys.opaque_identity o)
