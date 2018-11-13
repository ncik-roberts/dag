open Core
module Inverter (C : Comparable.S) : sig
  val invert : C.t list C.Map.t -> C.Set.t C.Map.t
end
