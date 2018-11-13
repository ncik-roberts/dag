open Core

module Vertex = Dag.Vertex

(** Topological sort of dag. *)
type traversal = Vertex.t list

(* A set that, when evaluated, should yield the data. *)
type to_evaluate = {
  pre_evaluation : Vertex.Set.t;
  during_evaluation : Vertex.Set.t;
}

type evaluation_state =
  | Evaluating_dependencies of to_evaluate
  | Evaluating_expression of Vertex.Set.t

type context = {
  (* Visited but unevaluated. *)
  frontier : Vertex.Set.t;

  (* Keys: currently evaluating;
   * Data: what needs to be evaluated to start evaluating the vertex
   *   and what needs to be evaluated to complete evaluating the vertex.
   *)
  evaluation_dependencies : evaluation_state Vertex.Map.t;

  (* Visited and evaluated. *)
  evaluated : Vertex.Set.t;
}

let evaluating (ctx : context) : Vertex.Set.t =
  Vertex.Set.of_map_keys ctx.evaluation_dependencies

let step_evaluation (vertex : Vertex.t) (ctx : context) : context =
  Map.fold ctx.evaluation_dependencies
    ~init:{
      frontier = Vertex.Set.empty;
      evaluated = Vertex.Set.empty;
      evaluation_dependencies = ctx.evaluation_dependencies;
    }
    ~f:(fun ~key ~data ctx -> match data with
      | Evaluating_dependencies to_eval ->
          if Set.mem to_eval.pre_evaluation vertex then
            let data' = match Set.remove to_eval.pre_evaluation vertex with
              | s when Set.is_empty s -> Evaluating_expression to_eval.during_evaluation
              | s -> Evaluating_dependencies { to_eval with pre_evaluation = s }
            in
            { ctx with evaluation_dependencies =
                Map.set ctx.evaluation_dependencies ~key ~data:data'
            }
          else ctx
      | Evaluating_expression evaluating ->
          if Set.mem evaluating vertex then
            let f = match Set.remove evaluating vertex with
              | s when Set.is_empty s -> Fn.flip Map.remove key
              | s -> Map.set ~key ~data:(Evaluating_expression s)
            in
            { ctx with evaluation_dependencies = f ctx.evaluation_dependencies }
          else ctx)

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
        let non_evaluated_predecessors = Vertex.Set.diff predecessors ctx.evaluated in
        let non_evaluating_predecessors = Vertex.Set.diff non_evaluated_predecessors (evaluating ctx) in
        let new_ctx = step_evaluation vertex ctx in
        (* Add all predecessors of current vertex to frontier. *)
        let ctx' = {
          frontier = Vertex.Set.union_list [frontier; non_evaluating_predecessors; new_ctx.frontier;];
          evaluation_dependencies = begin
            (* Singleton or empty. *)
            let set_of_option = Option.value_map ~f:Vertex.Set.singleton ~default:Vertex.Set.empty in
            let data = Evaluating_dependencies {
              pre_evaluation = non_evaluated_predecessors;
              during_evaluation = set_of_option (Dag.unroll dag vertex);
            } in
            Vertex.Map.add_exn new_ctx.evaluation_dependencies ~key:vertex ~data
          end;
          evaluated = Vertex.Set.union ctx.evaluated new_ctx.evaluated;
        } in
        loop ctx' ~acc:(vertex :: acc)
  in
  loop {
    frontier = Vertex.Set.singleton (Dag.return_vertex dag);
    evaluation_dependencies = Vertex.Map.empty;
    evaluated = Vertex.Set.empty;
  }

let all_traversals (dag : Dag.dag) : traversal list = raise (Failure "hey")
