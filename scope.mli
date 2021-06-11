
exception Variable_not_found of string

type t = (string, Value.t) Hashtbl.t list

val init : unit -> t

val def : t -> string -> Value.t -> unit
val push : t -> string -> Value.t -> t

val find : t -> string -> Value.t
