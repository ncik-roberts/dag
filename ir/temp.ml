open Core

module T = struct
  type t = {
    unique_id : int;
    typ : Tc.typ; [@hash.ignore]
  } [@@deriving sexp, hash]

  let compare : t -> t -> int = fun x1 x2 ->
    Int.compare x1.unique_id x2.unique_id
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let to_int x = T.(x.unique_id)
let to_type x = T.(x.typ)
let counter = ref 0
let next typ () = incr counter; T.{ unique_id = !counter; typ = typ; }
