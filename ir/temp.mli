include Core.Comparable.S
include Core.Sexpable.S with type t := t
include Core.Hashable.S with type t := t

val next : Tc.typ -> unit -> t
val to_int : t -> int
val to_type : t -> Tc.typ
