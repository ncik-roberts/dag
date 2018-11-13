open Core
module Inverter (C : Comparable.S) : sig
  val invert : C.t list C.Map.t -> C.Set.t C.Map.t
end

module type Comparable_sexpable = sig
  include Comparable.S
  include Sexpable.S with type t := t
end
