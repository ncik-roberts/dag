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

(** Directed acyclic graph *)
type dag = {
  successors : Vertex.Set.t Vertex.Map.t;
  predecessors : Vertex.t list Vertex.Map.t;
  return_vertex : Vertex.t;
  inputs : Vertex.t list;
  views : Vertex_view.t Vertex.Map.t
} [@@deriving sexp]

let return_vertex dag = dag.return_vertex
let predecessors dag =
  Fn.compose (Option.value ~default:[]) (Map.find dag.predecessors)
let view dag = Map.find_exn dag.views
let successors dag =
  Fn.compose (Option.value ~default:Vertex.Set.empty) (Map.find dag.successors)
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
  }

  let to_singleton vertex =
    Option.value_map ~f:(Vertex.Map.singleton vertex) ~default:Vertex.Map.empty

  let empty_of ?(view=None) ?(predecessors=None) (vertex : Vertex.t) =
    let views = to_singleton vertex view in
    let predecessors = to_singleton vertex predecessors in
    { views; predecessors; vertex; }

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
let of_fun_defn (f : Ast.fun_defn) : dag_fun =
  (* All the below "loop" functions make use of the following mutable state:
   *   - next_vertex for determining fresh vertex numberings.
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
          `New_vertex (vertex, view, results)
      | Ast.Parallel stmts ->
          let vertex = next_vertex () in
          let { Result.vertex = return_vertex; _; } as result = loop_stmts ctx stmts in
          let view = Vertex_view.Parallel_block return_vertex in
          `New_vertex (vertex, view, [result])
      | Ast.Const i ->
          let vertex = next_vertex () in
          let view = Vertex_view.(Literal (Int32 i)) in
          `New_vertex (vertex, view, [])
      | Ast.Binop binop ->
          let vertex = next_vertex () in
          let view = Vertex_view.Binop binop.binary_operator in
          let result1 = loop_expr ctx binop.binary_operand1 in
          let result2 = loop_expr ctx binop.binary_operand2 in
          `New_vertex (vertex, view, [result1; result2])
      | Ast.Unop unop ->
          let vertex = next_vertex () in
          let view = Vertex_view.Unop unop.unary_operator in
          let result = loop_expr ctx unop.unary_operand in
          `New_vertex (vertex, view, [result])
      | Ast.Variable v ->
          let vertex = Map.find_exn ctx v in
          `Reused_vertex vertex
    in

    (* Take action based on vertex status. *)
    match vertex_info with
    | `Reused_vertex vertex -> Result.empty_of vertex
    | `New_vertex (vertex, view, results) ->
        (* Fold results together. *)
        let vertices = List.map results ~f:(fun r -> Result.(r.vertex)) in
        let init = Result.empty_of vertex ~predecessors:(Some vertices) ~view:(Some view) in
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
        ~init:(ctx, None) ~f:(fun (ctx, _) stmt ->
          let result = loop_stmt ctx stmt in

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

  let input_names = Ast.(List.map f.fun_params ~f:(fun p -> p.param_ident)) in
  let inputs = List.map input_names ~f:(fun _ -> next_vertex ()) in

  (* Initial local variable context *)
  let init_ctx = String.Map.of_alist_exn (List.zip_exn input_names inputs) in

  let result = loop_stmts init_ctx Ast.(f.fun_body) in
  { dag_name = Ast.(f.fun_name);
    dag_graph =
      let Result.{ predecessors; views; vertex = return_vertex; } = result in
      let successors = Invert_vertex.invert predecessors in
      { predecessors; views; successors; return_vertex; inputs; }
  }

type t = dag_fun list [@@deriving sexp]
let of_ast = List.map ~f:of_fun_defn
