open Core

module F (T1 : Utils.Comparable_sexpable) (T2 : Utils.Comparable_sexpable) = struct
  module S1 = T1.Set
  module S2 = T2.Set
  let set ~convert : S1.t -> S2.t =
    S1.fold ~init:S2.empty ~f:(fun acc x -> S2.add acc (convert x))
  let list ~convert : T1.t list -> T2.t list = List.map ~f:convert
  let option ~convert : T1.t option -> T2.t option = Option.map ~f:convert
end

module To_temp = F (Dag.Vertex) (Temp)

module M = struct
  type dag = Dag.dag * (Dag.Vertex.t -> Temp.t) * (Temp.t -> Dag.Vertex.t)
  module Vertex = Temp
  let vertices_in_block (dag, into, out) ~parallel_block_vertex =
    Dag.vertices_in_block dag ~parallel_block_vertex:(out parallel_block_vertex)
      |> To_temp.set ~convert:into
  let inputs (dag, into, out) = Dag.inputs dag |> To_temp.list ~convert:into
  let unroll (dag, into, out) vtx = Dag.unroll dag (out vtx) |> To_temp.option ~convert:into
  let enclosing_parallel_blocks (dag, into, out) vtx =
    Dag.enclosing_parallel_blocks dag (out vtx) |> To_temp.list ~convert:into
  let view (dag, into, out) vtx = Dag.view dag (out vtx)
  let successors (dag, into, out) vtx =
    Dag.successors dag (out vtx) |> To_temp.set ~convert:into
  let predecessors (dag, into, out) vtx =
    Dag.predecessors dag (out vtx) |> To_temp.list ~convert:into
  let return_vertex (dag, into, out) = Dag.return_vertex dag |> into
  let vertices (dag, into, out) = Dag.vertices dag |> To_temp.set ~convert:into
  let type_of (dag, into, out) vtx = Dag.type_of dag (out vtx)
end

let into_dag = Fn.id

include M
