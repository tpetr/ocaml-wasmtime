type t

val of_string : string -> t

module Private : sig
  val to_string : t -> string
  val of_string : string -> t
end
