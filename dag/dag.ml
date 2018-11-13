open Core

module Vertex = Int32

module Vertex_view = struct
  type literal =
    | Int32 of int32
    | Bare_binop of Ast.binop
    [@@deriving sexp]

  type t =
    | Parallel_block of Vertex.t (* Return of block. *)
    | Function of Ast.call_name
    | Binop of Ast.binop
    | Unop of Ast.unop
    | Literal of literal
    | Parallel_binding of Ast.ident
    | Input of Ast.ident
    | Return
    [@@deriving sexp]
end

module Vertex_info = struct
  type t = {
    successors : Vertex.Set.t;
    predecessors : Vertex.t list;
    view : Vertex_view.t;
  } [@@deriving sexp]

  let collect_into_map
    ~(predecessors : Vertex.t list Vertex.Map.t)
    ~(successors : Vertex.Set.t Vertex.Map.t)
    ~(views : Vertex_view.t Vertex.Map.t) : t Vertex.Map.t =
    let preds_succs =
      Map.fold2 predecessors successors
        ~init:Vertex.Map.empty
        ~f:(fun ~key ~data acc -> match data with
          | `Both (p, s) -> Map.add_exn acc ~key ~data:(p, s)
          | `Left p -> Map.add_exn acc ~key ~data:(p, Vertex.Set.empty)
          | `Right s -> Map.add_exn acc ~key ~data:([], s))
    in
    Map.fold2 views preds_succs
      ~init:Vertex.Map.empty
      ~f:(fun ~key ~data acc -> match data with
        | `Both (v, (p, s)) ->
            Map.add_exn acc ~key ~data:{ view = v; predecessors = p; successors = s; }
        | `Left v ->
            Map.add_exn acc ~key ~data:{ view = v; predecessors = []; successors = Vertex.Set.empty; }
        | `Right (_p, _s) -> failwithf "Unknown vertex `%ld`." key ())
end

(** Directed acyclic graph *)
type dag = {
  vertex_infos : Vertex_info.t Vertex.Map.t;
  return_vertex : Vertex.t;
  inputs : Vertex.t list;
} [@@deriving sexp]

let return_vertex dag = dag.return_vertex
let vertex_info dag = Map.find_exn dag.vertex_infos
let predecessors dag key = (vertex_info dag key).Vertex_info.predecessors
let view dag key = (vertex_info dag key).Vertex_info.view
let successors dag key = (vertex_info dag key).Vertex_info.successors
let inputs dag = dag.inputs

type 'a counter = unit -> 'a
let make_counter ~(seed:'a) ~(next:'a -> 'a) : 'a counter =
  let ctr = ref seed in
  fun () -> Ref.replace ctr next; !ctr

type dag_fun = {
  dag_name : string;
  dag_graph : dag;
} [@@deriving sexp]

module Invert_vertex = Utils.Inverter (Vertex)

(** Local variable context. *)
module Context = String.Map

(** Result of translation. *)
module Result = struct
  type t = {
    views : Vertex_view.t Vertex.Map.t;
    predecessors : Vertex.t list Vertex.Map.t;
    vertex : Vertex.t;
  } [@@deriving sexp]

  (** Return a result for the given vertex, additionally adding singleton
   * bindings for in the maps if provided as optional arguments. *)
  let empty_of
    : ?view:Vertex_view.t option
    -> ?predecessors:Vertex.t list option
    -> Vertex.t -> t =
    let to_singleton vertex =
      Option.value_map ~f:(Vertex.Map.singleton vertex) ~default:Vertex.Map.empty in
    fun ?(view=None) ?(predecessors=None) (vertex : Vertex.t) ->
      let views = to_singleton vertex view in
      let predecessors = to_singleton vertex predecessors in
      { views; predecessors; vertex; }

  (**
   * Bias indicates whether to take vertex from left or right argument.
   * Otherwise, merges; failing if there is a duplicate key in the merged
   * maps.
   *)
  let union_with_bias : bias:[ `Left | `Right ] -> t -> t -> t =
    let merge_exn = Map.merge_skewed ~combine:(fun ~key -> failwith "Duplicate key.") in
    fun ~bias result1 result2 -> {
      views = merge_exn result1.views result2.views;
      predecessors = merge_exn result1.predecessors result2.predecessors;
      vertex = match bias with
        | `Left -> result1.vertex
        | `Right -> result2.vertex;
    }
end

(** Dag of ast *)
type t = dag_fun list [@@deriving sexp]
let of_ast : Ast.t -> t =
  (* All the below "loop" functions make use of the following mutable state:
   *   - next_vertex for determining fresh vertex numberings.
   * next_vertex will always return a globally unique vertex, even across
   * invocations of `of_ast`.
   *)
  let next_vertex = make_counter ~seed:0l ~next:Int32.succ in

  (* Returns id of expression vertex. *)
  let rec loop_expr (ctx : Vertex.t Context.t) (expr : Ast.expr) : Result.t =
    (* vertex: the id of the expression vertex.
     * result: if None, that means the expression is a local var, so it is represented solely
     *           by edges in the graph.
     *         if Some _, that means the expression is not a local var, so we must overtly
     *           represent it as a view in the hash table.
     *)
    let vertex_info = match expr with
      | Ast.Fun_call fun_call ->
          let vertex = next_vertex () in
          let view = Vertex_view.Function fun_call.call_name in
          let results = List.map fun_call.call_args ~f:(loop_arg ctx) in
          `New_vertex (vertex, view, results, `No_additional_predecessors)
      | Ast.Parallel stmts ->
          let vertex = next_vertex () in
          let { Result.vertex = return_vertex; _; } as result = loop_stmts ctx stmts in
          let view = Vertex_view.Parallel_block return_vertex in

          (* For a parallel block, the additional predecessors are the vertices used
           * in the statements that are already in the context.
           *
           * Intuitively, these are what must be computed before the parallel block can
           * be run.
           *)
          let additional_predecessors =
            let ctx_keys = Vertex.Set.of_list (Map.data ctx) in
            Map.fold Result.(result.predecessors)
              ~init:Vertex.Set.empty
              ~f:(fun ~key:_ ~data -> Set.union (Set.inter (Vertex.Set.of_list data) ctx_keys))
          in
          `New_vertex (vertex, view, [result], `With_additional_predecessors additional_predecessors)
      | Ast.Const i ->
          let vertex = next_vertex () in
          let view = Vertex_view.(Literal (Int32 i)) in
          `New_vertex (vertex, view, [], `No_additional_predecessors)
      | Ast.Binop binop ->
          let vertex = next_vertex () in
          let view = Vertex_view.Binop binop.binary_operator in
          let result1 = loop_expr ctx binop.binary_operand1 in
          let result2 = loop_expr ctx binop.binary_operand2 in
          `New_vertex (vertex, view, [result1; result2], `No_additional_predecessors)
      | Ast.Unop unop ->
          let vertex = next_vertex () in
          let view = Vertex_view.Unop unop.unary_operator in
          let result = loop_expr ctx unop.unary_operand in
          `New_vertex (vertex, view, [result], `No_additional_predecessors)
      | Ast.Variable v ->
          let vertex = Map.find_exn ctx v in
          `Reused_vertex vertex
    in

    (* Take action based on vertex status. *)
    match vertex_info with
    | `Reused_vertex vertex -> Result.empty_of vertex
    | `New_vertex (vertex, view, results, predecessor_status) ->
        (* Fold results together. *)
        let vertices = List.map results ~f:(fun r -> Result.(r.vertex)) in
        let predecessors =
          begin
            match predecessor_status with
            | `No_additional_predecessors -> vertices
            | `With_additional_predecessors ps -> vertices @ Set.to_list ps
          end |> function
          (* Short circuit: add no binding if the predecessors is empty. *)
          | [] -> None
          | xs -> Some xs
        in
        let init = Result.empty_of vertex ~predecessors ~view:(Some view) in
        List.fold_left results ~init ~f:(Result.union_with_bias ~bias:`Left)

  and loop_arg (ctx : Vertex.t Context.t) (arg : Ast.arg) : Result.t = match arg with
    | Ast.Bare_binop binop ->
        let vertex = next_vertex () in
        let view = Vertex_view.(Literal (Bare_binop binop)) in
        Result.empty_of vertex ~view:(Some view)
    | Ast.Expr expr -> loop_expr ctx expr

  (* The vertex field of the result is the return expression vertex. *)
  and loop_stmts (ctx : Vertex.t Context.t) (stmts : Ast.stmt list) : Result.t =
    (* Returns the vertex of the expression involved in the binding or return. *)
    let rec loop_stmt (ctx : Vertex.t Context.t) (stmt : Ast.stmt) : Result.t =
      match stmt with
      | Ast.Bind bind_stmt ->
          let expr_result = loop_expr ctx bind_stmt.bind_expr in

          (* Add intermediary vertex expressing parallel bind. *)
          let bind_vertex = next_vertex () in
          let view = Vertex_view.Parallel_binding bind_stmt.bind_ident in

          let result =
            let open Result in
            empty_of bind_vertex ~view:(Some view) ~predecessors:(Some [expr_result.vertex])
          in

          (* Left bias so that bind_vertex is taken. *)
          Result.union_with_bias ~bias:`Left result expr_result

      | Ast.Let let_stmt -> loop_expr ctx let_stmt.let_expr
      | Ast.Return expr -> loop_expr ctx expr
    in
    let (_ctx, return_result) =
      (* Right-bias fold so that return_result belongs to the last statement. *)
      List.fold_left stmts
        ~init:(ctx, None) ~f:(fun (ctx, prev_result) stmt ->
          (* Retain contex from previous result, except replacing the vertex with
           * the current vertex (hence right bias). *)
          let union_with_previous =
            Option.value_map prev_result
              ~default:Fn.id ~f:(Result.union_with_bias ~bias:`Right)
          in
          let result = union_with_previous (loop_stmt ctx stmt) in

          (* Update ctx with binding *)
          let ctx' = match stmt with
            | Ast.Let { let_ident = ident }
            | Ast.Bind { bind_ident = ident } ->
                Context.add_exn ctx ~key:ident ~data:Result.(result.vertex)
            | Ast.Return _ -> ctx
          in

          (ctx', Some result))
    in

    Option.value_exn return_result
  in

  let of_fun_defn (f : Ast.fun_defn) : dag_fun =
    let input_names = Ast.(List.map f.fun_params ~f:(fun p -> p.param_ident)) in
    let inputs = List.map input_names ~f:(fun _ -> next_vertex ()) in
    let input_pairs = List.zip_exn input_names inputs in

    (* Initial local variable context *)
    let init_ctx = String.Map.of_alist_exn input_pairs in

    let result = loop_stmts init_ctx Ast.(f.fun_body) in
    { dag_name = Ast.(f.fun_name);
      dag_graph =
        let Result.{ predecessors; views; vertex = return_vertex; } = result in
        (* Add input views. *)
        let views =
          List.fold_left input_pairs ~init:views
            ~f:(fun acc (name, vtx) -> Map.add_exn acc ~key:vtx ~data:(Vertex_view.Input name))
        in
        let successors = Invert_vertex.invert predecessors in
        let vertex_infos = Vertex_info.collect_into_map ~successors ~predecessors ~views in
        { vertex_infos; return_vertex; inputs; }
    }
  in
  List.map ~f:of_fun_defn
