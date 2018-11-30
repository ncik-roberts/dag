open Core
module Inverter (C : Comparable.S) : sig
  val invert : C.t list C.Map.t -> C.Set.t C.Map.t
end

module type Comparable_sexpable = sig
  include Comparable.S
  include Sexpable.S with type t := t
end

val merge_list_exn : ('k, 'v, 'cmp) Map.t list -> ('k, 'v, 'cmp) Map.t

module Many_fn : sig
  type ('a, 'b) t =
    | Fun of ('a -> ('a, 'b) t)
    | Result of 'b
    [@@deriving sexp]

  val result_exn : ?msg:string -> ('a, 'b) t -> 'b
  val app_exn : ?msg:string -> ('a, 'b) t -> 'a -> ('a, 'b) t
  val app_many_exn : ('a, 'b) t -> 'a list -> ('a, 'b) t
  val lift : ('a -> 'b) -> ('a, 'b) t
  val compose : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
end
