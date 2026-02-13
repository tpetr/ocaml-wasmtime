(* A low-level but hopefully type safe version of the API. *)

exception Trap of { message : string }

module Engine : sig
  type t

  val create
    :  ?debug_info:bool
    -> ?max_wasm_stack:int
    -> ?reference_types:bool
    -> ?simd:bool
    -> ?bulk_memory:bool
    -> ?multi_value:bool
    -> ?memory_reservation:int
    -> ?memory_guard_size:int
    -> ?memory_reservation_for_growth:int
    -> unit
    -> t
end

module Store : sig
  type t

  val create : Engine.t -> t
end

module Byte_vec : sig
  type t

  val create : len:int -> t
  val of_string : string -> t
  val to_string : t -> string
  val length : t -> int
end

module Module : sig
  type t

  type extern_kind = Func | Global | Table | Memory

  type export = { name : string; kind : extern_kind }

  val exports : t -> export list
end

module Func : sig
  type t

  val of_func_0_0 : Store.t -> (unit -> unit) -> t

  val of_func
    :  args:'a Val.Kind.tuple
    -> results:'b Val.Kind.tuple
    -> Store.t
    -> ('a -> 'b)
    -> t

  val of_func_list
    :  args:Val.Kind.packed list
    -> results:Val.Kind.packed list
    -> Store.t
    -> (Val.t list -> Val.t list)
    -> t
end

module Memory : sig
  type t

  val size_in_pages : Store.t -> t -> int
  val size_in_bytes : Store.t -> t -> int
  val grow : Store.t -> t -> int -> bool

  (** [to_string] makes a copy of the memory data. *)
  val to_string : Store.t -> t -> pos:int -> len:int -> string
  val get : Store.t -> t -> pos:int -> char
  val set : Store.t -> t -> pos:int -> char -> unit
end

module Extern : sig
  type t

  val func_as : Func.t -> t
  val as_func : t -> Func.t
  val as_memory : t -> Memory.t
end

module Instance : sig
  type t

  val exports : Store.t -> t -> Extern.t list
end

module Wasi : sig
  type stdin = Inherit | Bytes of string | File of string

  type capture
  val create_capture : unit -> capture
  val capture_contents : capture -> string

  type stdout = Inherit | File of string | Capture of capture
  type stderr = Inherit | File of string | Capture of capture

  val configure
    :  ?inherit_argv:bool
    -> ?inherit_env:bool
    -> ?stdin:stdin
    -> ?stdout:stdout
    -> ?stderr:stderr
    -> ?preopen_dirs:(string * string) list
    -> Store.t
    -> unit
end

module Wasmtime : sig
  module Linker : sig
    type t

    val create : Engine.t -> t
    val define_wasi : t -> unit
    val define_instance : t -> Store.t -> name:string -> Instance.t -> unit
    val instantiate : t -> Store.t -> Module.t -> Instance.t
    val module_ : t -> Store.t -> name:string -> Module.t -> unit
    val get_default : t -> Store.t -> name:string -> Func.t
  end

  val wat_to_wasm : wat:Byte_vec.t -> Byte_vec.t
  val new_module : Engine.t -> wasm:Byte_vec.t -> Module.t
  val new_instance : ?imports:Extern.t list -> Store.t -> Module.t -> Instance.t
  val func_call0 : Store.t -> Func.t -> Val.t list -> unit
  val func_call1 : Store.t -> Func.t -> Val.t list -> Val.t
  val func_call2 : Store.t -> Func.t -> Val.t list -> Val.t * Val.t

  val func_call
    :  args:'a Val.Kind.tuple
    -> results:'b Val.Kind.tuple
    -> Store.t
    -> Func.t
    -> 'a
    -> 'b

  val func_call_list : Store.t -> Func.t -> Val.t list -> n_outputs:int -> Val.t list
end
