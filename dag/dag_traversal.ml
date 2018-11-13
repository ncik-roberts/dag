open Core

module Vertex = Dag.Vertex

(** Topological sort of dag. *)
type traversal = Vertex.t list

type context = {
  (* Visited but unevaluated. *)
  frontier : Vertex.Set.t;

 (* Keys: currently evaluating; Data: vertices to finish evaluating first. *)
  evaluation_dependencies : Vertex.Set.t Vertex.Map.t;

  (* Visited and evaluated. *)
  evaluated : Vertex.Set.t;
}

let evaluating (ctx : context) : Vertex.Set.t =
  Vertex.Set.of_map_keys ctx.evaluation_dependencies

(* Returns set of newly-evaluated vertices as well as new dependency map. *)
let find_newly_evaluated (vertex : Vertex.t) (ctx : context)
  : Vertex.Set.t * Vertex.Set.t Vertex.Map.t =
  Map.fold ctx.evaluation_dependencies
    ~init:(Vertex.Set.empty, ctx.evaluation_dependencies)
    ~f:(fun ~key ~data (acc_set, acc_map as acc) ->
      if Set.mem data vertex then
        let removed = Set.remove data vertex in
        if Set.is_empty removed
          then (Set.add acc_set vertex, Map.remove acc_map vertex)
          else (acc_set, Map.set acc_map ~key ~data:removed)
      else acc)

let any_traversal (dag : Dag.dag) : traversal =
  let rec loop ?(acc=[]) (ctx : context) : traversal =
    match Vertex.Set.choose ctx.frontier with
    | None -> acc
    | Some vertex ->
        if Map.mem ctx.evaluation_dependencies vertex
          then failwith "Invariant destroyed: evaluating vertex added to frontier.";
        if Set.mem ctx.evaluated vertex
          then failwith "Invariant destroyed: evaluated vertex added to frontier.";
        let frontier = Vertex.Set.remove ctx.frontier vertex in
        let predecessors = Vertex.Set.of_list (Dag.predecessors dag vertex) in
        let non_evaluated_predecessors = Vertex.Set.diff ctx.evaluated predecessors in
        let non_evaluating_predecessors = Vertex.Set.diff (evaluating ctx) non_evaluated_predecessors in
        let (newly_evaluated, dependencies) = find_newly_evaluated vertex ctx in
        (* Add all predecessors of current vertex to frontier. *)
        let ctx' = {
          frontier = Vertex.Set.union frontier non_evaluating_predecessors;
          evaluation_dependencies =
            Vertex.Map.add_exn dependencies ~key:vertex ~data:non_evaluated_predecessors;
          evaluated = Vertex.Set.union ctx.evaluated newly_evaluated;
        } in
        loop ctx' ~acc:(vertex :: acc)
  in
  loop {
    frontier = Vertex.Set.singleton (Dag.return_vertex dag);
    evaluation_dependencies = Vertex.Map.empty;
    evaluated = Vertex.Set.empty;
  }

let all_traversals (dag : Dag.dag) : traversal list = raise (Failure "hey")
