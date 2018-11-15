open Core

module Vertex = Dag.Vertex

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

  (* Only vertices to consider evaluate.
   * None indicates the universe.
   *)
  subgraph : Vertex.Set.t option;

  (* Vertices already placed into the evaluation order.
   * We add one new vertex to this each iteration of the loop.
   *)
  evaluated : Vertex.Set.t;
} [@@deriving sexp]

let any_traversal (dag : Dag.dag) : traversal =

  (* How do I evaluate a vertex? *)
  let rec loop_of_vertex ?(subgraph : Vertex.Set.t option = None) (vertex : Vertex.t) : traversal =
    loop ~acc:[] {
      direct_predecessors = Vertex.Set.singleton vertex;
      subgraph;
      evaluated = Vertex.Set.empty;
    }

  (* Arbitrarily find a way to evaluate starting from a context. *)
  and loop ~(acc : traversal) (ctx : context) : traversal =
    let candidate = Set.find ctx.direct_predecessors ~f:(fun v ->
      Set.is_subset (Dag.successors dag v) ~of_:ctx.evaluated
    ) in
    let loop_with (vertex : Vertex.t) (subtraversal : traversal) : traversal =
      let predecessors_minus_vertex = Set.remove ctx.direct_predecessors vertex in
      let intersect_with_subgraph =
        Option.value_map ctx.subgraph ~f:Set.inter ~default:Fn.id
      in
      let ctx' = { ctx with
        direct_predecessors =
          Vertex.Set.of_list (Dag.predecessors dag vertex)
            |> intersect_with_subgraph
            |> Set.union predecessors_minus_vertex;
        evaluated = Set.union ctx.evaluated (Vertex.Set.of_list (vertex :: traversal_to_list subtraversal));
      } in loop ctx' ~acc:(
        let elem = match Dag.view dag vertex with
          | Dag.Vertex_view.Parallel_block _ -> Block (vertex, subtraversal)
          | _ -> Just vertex
        in elem :: acc)
    in
    match candidate with
    | None when Vertex.Set.is_empty ctx.direct_predecessors -> acc
    | None ->
        prerr_endline (Sexp.to_string_hum (sexp_of_context ctx));
        prerr_endline (Sexp.to_string_hum (Vertex.Set.sexp_of_t ctx.direct_predecessors));
        failwith "Cyclic dependency detected."
    | Some vertex ->
        let subtraversal =
          Option.value_map (Dag.unroll dag vertex) ~default:[]
            ~f:(fun return_vertex ->
              let in_block = Dag.vertices_in_block dag ~parallel_block_vertex:vertex in
              loop_of_vertex return_vertex ~subgraph:(Some in_block))
        in
        loop_with vertex subtraversal
  in
  loop_of_vertex (Dag.return_vertex dag)

let all_traversals (dag : Dag.dag) : traversal list = raise (Failure "hey")
