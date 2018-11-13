open Core

module Vertex = Int32

module Vertex_view = struct
  type literal =
    | Int32 of int32
    | Bare_binop of Ast.binop
    [@@deriving sexp]

  type t =
    | Parallel_block of Vertex.t
    | Function of Ast.call_name
    | Binop of Ast.binop
    | Unop of Ast.unop
    | Literal of literal
    | Return
    [@@deriving sexp]
end

(** Directed acyclic graph *)
type dag = {
  successors : Vertex.Set.t Vertex.Map.t;
  predecessors : (Vertex.t list) Vertex.Map.t;
  return_vertex : Vertex.t;
  inputs : Vertex.t list;
  views : Vertex_view.t Vertex.Map.t
} [@@deriving sexp]

let return_vertex dag = dag.return_vertex
let predecessors dag = Vertex.Map.find_exn dag.predecessors
let view dag = Map.find_exn dag.views
let successors dag = Map.find_exn dag.successors
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

(** Dag of ast *)
let of_fun_defn (fun_defn : Ast.fun_defn) : dag_fun =
  (* All the below "loop" functions make use of the following mutable state:
   *   - counter for determining fresh vertex numberings, and
   *   - lookup_local_var for determining the vertices corresponding to local variables.
   *   - predecessors for populating the predecessors field of the return value.
   *)
  let next_vertex = make_counter ~seed:0l ~next:Int32.succ in
  let predecessors : (Vertex.t list) Vertex.Table.t = Vertex.Table.create () in
  let views : Vertex_view.t Vertex.Table.t = Vertex.Table.create () in

  let input_pairs = List.map Ast.(fun_defn.fun_params) ~f:Ast.(fun param -> (param.param_ident, next_vertex ())) in
  let local_vars = String.Table.of_alist_exn input_pairs in

  (* Returns id of expression vertex. *)
  let rec loop_expr (expr : Ast.expr) : Vertex.t =
    (* vertex: the id of the expression vertex.
     * result: if None, that means the expression is a local var, so it is represented solely
     *           by edges in the graph.
     *         if Some _, that means the expression is not a local var, so we must overtly
     *           represent it as a view in the hash table.
     *)
    let (vertex : Vertex.t), (result : (Vertex_view.t * Vertex.t list) option) = match expr with
      | Ast.Fun_call fun_call ->
          let vertex = next_vertex () in
          let view = Vertex_view.Function fun_call.call_name in
          let predecessor = List.map fun_call.call_args ~f:loop_arg in
          (vertex, Some (view, predecessor))
      | Ast.Parallel stmts ->
          let vertex = next_vertex () in
          let return_vtx = loop_stmts stmts in
          let view = Vertex_view.Parallel_block return_vtx in
          (vertex, Some (view, []))
      | Ast.Const i ->
          let vertex = next_vertex () in
          Vertex_view.(vertex, Some (Literal (Int32 i), []))
      | Ast.Binop binop ->
          let vertex = next_vertex () in
          let view = Vertex_view.Binop binop.binary_operator in
          let pred1 = loop_expr binop.binary_operand1 in
          let pred2 = loop_expr binop.binary_operand2 in
          (vertex, Some (view, [ pred1; pred2; ]))
      | Ast.Unop unop ->
          let vertex = next_vertex () in
          let view = Vertex_view.Unop unop.unary_operator in
          let pred = loop_expr unop.unary_operand in
          (vertex, Some (view, [ pred ]))
      | Ast.Variable v ->
          let vertex = String.Table.find_exn local_vars v in
          (vertex, None)
    in
    Option.iter result ~f:(fun (view, predecessor) ->
      Vertex.Table.add_exn views ~key:vertex ~data:view;
      Vertex.Table.add_exn predecessors ~key:vertex ~data:predecessor);
    vertex

  and loop_arg (arg : Ast.arg) : Vertex.t = match arg with
    | Ast.Bare_binop binop ->
        let vertex = next_vertex () in
        let view = Vertex_view.(Literal (Bare_binop binop)) in
        Vertex.Table.add_exn views ~key:vertex ~data:view;
        vertex
    | Ast.Expr expr -> loop_expr expr

  (* Returns id of return vertex. *)
  and loop_stmts (stmts : Ast.stmt list) : Vertex.t =
    (* Returns the vertex of the expression involved in the binding or return. *)
    let rec loop_stmt (stmt : Ast.stmt) : Vertex.t = match stmt with
      | Ast.Bind { bind_ident = ident; bind_expr = expr; _; }
      | Ast.Let { let_ident = ident; let_expr = expr; _; } ->
          let expr_vertex = loop_expr expr in
          String.Table.add_exn local_vars ~key:ident ~data:expr_vertex;
          expr_vertex
      | Ast.Return expr -> loop_expr expr
    in
    let return_vertex = List.fold_left stmts ~init:None ~f:(fun _x1 x2 -> Some (loop_stmt x2)) in
    Option.value_exn return_vertex
  in

  let return_vertex = loop_stmts Ast.(fun_defn.fun_body) in
  { dag_name = Ast.(fun_defn.fun_name);
    dag_graph =
      let predecessors = Vertex.Map.of_hashtbl_exn predecessors in
      let views = Vertex.Map.of_hashtbl_exn views in
      let successors = Invert_vertex.invert predecessors in
      let inputs = List.map ~f:snd input_pairs in
      { predecessors; views; successors; return_vertex; inputs; }
  }

type t = dag_fun list [@@deriving sexp]
let of_ast = List.map ~f:of_fun_defn
