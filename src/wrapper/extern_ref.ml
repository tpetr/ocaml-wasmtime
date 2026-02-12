(* In v41, externref is a GC-managed struct within a store context.
   We keep a simple string wrapper for now. The actual externref creation
   happens in wrappers.ml when we have access to the context. *)
type t = string

let of_string str = str

module Private = struct
  let to_string = Fun.id
  let of_string = Fun.id
end
