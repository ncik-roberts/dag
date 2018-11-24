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
} [@@deriving sexp]

let traversals_with_filter (dag : Dag.dag)
    ~(filter : Vertex.t list -> Vertex.t list) : traversal list =

  let isn't_input (v : Vertex.t) : bool = match Dag.view dag v with
    | Vertex_view.Input _ -> false
    | _ -> true
  in

  (* How do I evaluate a vertex? *)
  let rec loop_of_vertex (vertex : Vertex.t) : traversal list =
    loop ~acc:[] {
      direct_predecessors = Vertex.Set.singleton vertex;
      evaluated = Vertex.Set.empty;
    }

  (* Arbitrarily find a way to evaluate starting from a context. *)
  and loop ~(acc : traversal) (ctx : context) : traversal list =
    let candidates = Set.filter ctx.direct_predecessors ~f:(fun v ->
      isn't_input v && Set.is_subset (Dag.successors dag v) ~of_:ctx.evaluated
    ) in
    let loop_with (vertex : Vertex.t) (subtraversals : traversal list) : traversal list =
      let predecessors_minus_vertex = Set.remove ctx.direct_predecessors vertex in
      let direct_predecessors =
        Vertex.Set.of_list (Dag.predecessors dag vertex)
          |> Set.union predecessors_minus_vertex
      in
      List.concat_map subtraversals ~f:(fun subtraversal ->
        let ctx' = {
          direct_predecessors;
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
    | [] -> [acc]
    | candidate_list ->
        List.concat_map candidate_list ~f:(fun vertex ->
          let subtraversal =
            Option.value_map (Dag.unroll dag vertex) ~default:[[]] ~f:loop_of_vertex
          in
          loop_with vertex subtraversal)
  in
  loop_of_vertex (Dag.return_vertex dag)

let any_traversal (dag : Dag.dag) : traversal =
  traversals_with_filter dag ~filter:(function
    | [] -> []
    | x :: _ -> [x])
  |> function
  | [] -> failwith "No traversal found?"
  | [x] -> x
  | _ -> failwith "Too many traversals found?"

let all_traversals = traversals_with_filter ~filter:Fn.id
