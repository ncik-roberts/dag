open Core
module Inverter (C : Comparable.S) = struct
  let invert (m : C.t list C.Map.t) : C.Set.t C.Map.t =
    C.Map.fold m
      ~init:C.Map.empty
      ~f:(fun ~key ~data acc ->
        List.fold_left data ~init:acc
          ~f:(fun acc vertex ->
            C.Map.update acc vertex ~f:(function
              | None -> C.Set.singleton key
              | Some set -> C.Set.add set key)))
end

module type Comparable_sexpable = sig
  include Comparable.S
  include Sexpable.S with type t := t
end

let merge_list_exn : ('k, 'v, 'cmp) Map.t list -> ('k, 'v, 'cmp) Map.t = function
  | [] -> failwith "Can't call with empty list."
  | x :: xs ->
      List.fold_left xs ~init:x
        ~f:(Map.merge_skewed ~combine:(fun ~key -> failwith "Duplicate key."))
