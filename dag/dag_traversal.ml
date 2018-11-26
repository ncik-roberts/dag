open Core

module Vertex = Dag.Vertex
module Vertex_view = Dag.Vertex_view

(** Topological sort of dag. *)
type traversal_tree =
  | Block of Vertex.t * traversal
  | Just of Vertex.t
  [@@deriving sexp]

and traversal = traversal_tree list [@@deriving sexp]

let rec traversal_to_list = List.concat_map ~f:(function
  | Just v -> [v]
  | Block (x, ys) -> x :: traversal_to_list ys)

(**
 * When reading the documentation for this context, keep in mind that
 * our algorithm for finding a traversal begins at the return
 * statement and proceeds backwards. Thus, our traversal identifies
 * things in reverse order of evaluation.
 *)
type context = {
  (* What we must have already evaluated before this point.
   * That is, a set of the vertices on which the `evaluated` set
   * depends directly.
   *)
  direct_predecessors : Vertex.Set.t;

  (* Vertices already placed into the evaluation order.
   * We add one new vertex to this each iteration of the loop.
   *)
  evaluated : Vertex.Set.t;

  curr_bound_parallel_vertex : Vertex.t option;
} [@@deriving sexp]

let transitive_predecessor_closure (dag : Dag.dag) : Vertex.Set.t Vertex.Map.t =
  let init = Vertex.Map.of_alist_exn
    (Dag.vertices dag |> Set.to_list |> List.map ~f:(fun vtx ->
      (vtx, Vertex.Set.of_list (Dag.predecessors dag vtx))))
  in
  let rec loop acc =
    let any_changed = ref false in
    let acc' = Map.map acc ~f:(fun x ->
      Set.fold x ~init:x ~f:(fun acc elt ->
        let acc' = List.fold_left (Dag.predecessors dag elt) ~init:acc ~f:Set.add in
        if Set.length acc <> Set.length acc' then any_changed := true;
        acc'))
    in
    if !any_changed then loop acc' else acc
  in loop init

let traversals_with_filter (dag : Dag.dag)
    ~(filter : Vertex.t list -> Vertex.t list) : traversal list =

  let predecessors = transitive_predecessor_closure dag in

  let isn't_value (v : Vertex.t) : bool = match Dag.view dag v with
    | Vertex_view.Input _ -> false
    | Vertex_view.Literal _ -> false
    | _ -> true
  in

  (* How do I evaluate a vertex? *)
  let rec loop_of_vertex ?(curr=None) (vertex : Vertex.t) : (traversal * Vertex.Set.t) list =
    loop ~acc:[] {
      curr_bound_parallel_vertex = curr;
      direct_predecessors = Vertex.Set.singleton vertex;
      evaluated = Vertex.Set.empty;
    }

  (* Arbitrarily find a way to evaluate starting from a context. *)
  and loop ~(acc : traversal) (ctx : context) : (traversal * Vertex.Set.t) list =
    let candidates = Set.filter ctx.direct_predecessors ~f:(fun v ->
      isn't_value v && Set.is_subset (Dag.successors dag v) ~of_:ctx.evaluated
    ) in
    let loop_with (vertex : Vertex.t) (subtraversals : (traversal * Vertex.Set.t) list) : (traversal * Vertex.Set.t) list =
      let predecessors_minus_vertex = Set.remove ctx.direct_predecessors vertex in
      let direct_predecessors =
        Vertex.Set.of_list (Dag.predecessors dag vertex)
          |> Set.union predecessors_minus_vertex
      in
      List.concat_map subtraversals ~f:(fun (subtraversal, remaining) ->
        let ctx' = {
          curr_bound_parallel_vertex = ctx.curr_bound_parallel_vertex;
          direct_predecessors = Set.union direct_predecessors remaining;
          evaluated =
            Set.union
              ctx.evaluated
              (Vertex.Set.of_list (vertex :: traversal_to_list subtraversal));
        } in loop ctx' ~acc:(
          let elem = match Dag.view dag vertex with
            | Vertex_view.Parallel_block _ -> Block (vertex, subtraversal)
            | _ -> Just vertex
          in elem :: acc))
    in
    match filter (Set.to_list candidates) with
    | [] -> [(acc, Vertex.Set.empty)]
    | candidate_list ->
        List.concat_map candidate_list ~f:(fun vertex ->
          let subtraversals =
            Option.value_map (Dag.unroll dag vertex) ~default:[([], Vertex.Set.empty)]
              ~f:(loop_of_vertex ~curr:(
                begin
                  match Dag.view dag vertex with
                  | Vertex_view.Parallel_block (bd_vtx, _) -> Some bd_vtx
                  | _ -> None
                end))
          in
          let results = loop_with vertex subtraversals in
          match ctx.curr_bound_parallel_vertex with
          | Some pvtx when not (Set.mem (Map.find_exn predecessors vertex) pvtx) ->
              let results2 = loop ~acc { ctx with direct_predecessors = Set.remove ctx.direct_predecessors vertex } in
              results @ (List.map results2 ~f:(Tuple2.map_snd ~f:(Fn.flip Vertex.Set.add vertex)))
          | _ -> results)
  in
  List.filter_map (loop_of_vertex (Dag.return_vertex dag)) ~f:(fun (result, set) ->
    Option.some_if (Set.is_empty set) result)

let any_traversal (dag : Dag.dag) : traversal =
  traversals_with_filter dag ~filter:(function
    | [] -> []
    | x :: _ -> [x])
  |> function
  | [] -> failwith "No traversal found?"
  | [x] -> x
  | _ -> failwith "Too many traversals found?"

let all_traversals = traversals_with_filter ~filter:Fn.id
