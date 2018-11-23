include Core.Comparable.S
include Core.Sexpable.S with type t := t

val next : unit -> t
val to_int : t -> int
