open Core

module Vertex = Int32

module Vertex_view = struct
  type literal =
    | Int32 of int32
    | Float of float
    | Bool of bool
    | Bare_binop of Ast.binop
    | Bare_unop of Ast.unop
    [@@deriving sexp]

  type t =
    | Parallel_block of Vertex.t * Vertex.t (* (Parallel binding, return of block) *)
    | Function of Ast.call_name
    | Binop of Ast.binop
    | Unop of Ast.unop
    | Access of Ast.ident
    | Index
    | Struct_Init of Tc.typ * Ast.ident list
    | Literal of literal
    | Input of Ast.ident
    [@@deriving sexp]
end

module Vertex_info = struct
  type t = {
    successors : Vertex.Set.t;
    predecessors : Vertex.t list;
    view : Vertex_view.t;
    enclosing_parallel_blocks : Vertex.t list;
    vertices_in_block : Vertex.Set.t option; (* Populated only if the view is a parallel block vertex. *)
    typ : Tc.typ;
  } [@@deriving sexp]

  let collect_into_map
    ~(types : Tc.typ Vertex.Map.t)
    ~(predecessors : Vertex.t list Vertex.Map.t)
    ~(successors : Vertex.Set.t Vertex.Map.t)
    ~(views : Vertex_view.t Vertex.Map.t)
    ~(enclosing_parallel_blocks : Vertex.t list Vertex.Map.t)
    ~(vertices_in_block : Vertex.Set.t Vertex.Map.t) : t Vertex.Map.t =

    let for_key (key : Vertex.t) : t =
      let find map ~default ~f = Option.value_map (Map.find map key) ~f ~default in
      let p = find predecessors ~default:[] ~f:Fn.id in
      let s = find successors ~default:Vertex.Set.empty ~f:Fn.id in
      let v = match Map.find views key with
        | Some v -> v
        | None -> failwithf "Unknown vertex `%ld`." key ()
      in
      let e = find enclosing_parallel_blocks ~default:[] ~f:Fn.id in
      let t = match Map.find types key with
        | Some t -> t
        | None -> failwithf "(%s): Unknown type of vertex `%ld`." (Vertex_view.sexp_of_t v |> Sexp.to_string_hum) key ()
      in
      let vtx = find vertices_in_block ~f:Option.some ~default:None in
      { predecessors = p;
        successors = s;
        view = v;
        enclosing_parallel_blocks = e;
        vertices_in_block = vtx;
        typ = t;
      }
    in

    let keys =
      let ks = Vertex.Set.of_map_keys in
      Vertex.Set.union_list
        [ ks predecessors;
          ks successors;
          ks views;
          ks enclosing_parallel_blocks;
          ks vertices_in_block;
        ]
    in

    Vertex.Set.to_map keys ~f:for_key

end

(** Directed acyclic graph *)
type dag = {
  vertex_infos : Vertex_info.t Vertex.Map.t;
  return_vertex : Vertex.t;
  inputs : Vertex.t list;
} [@@deriving sexp]

(* Reproduced from mli file *)
module type Daglike = sig
  module Vertex : Utils.Comparable_sexpable
  type dag
  val return_vertex : dag -> Vertex.t
  val predecessors : dag -> Vertex.t -> Vertex.t list
  val successors : dag -> Vertex.t -> Vertex.Set.t
  val inputs : dag -> Vertex.t list
  val view : dag -> Vertex.t -> Vertex_view.t
  val vertices : dag -> Vertex.Set.t
  val enclosing_parallel_blocks : dag -> Vertex.t -> Vertex.t list
  val vertices_in_block : dag -> parallel_block_vertex:Vertex.t -> Vertex.Set.t
  val unroll : dag -> Vertex.t -> Vertex.t option
  val type_of : dag -> Vertex.t -> Tc.typ
end

let return_vertex dag = dag.return_vertex
let vertex_info dag = Map.find_exn dag.vertex_infos
let type_of dag key = (vertex_info dag key).Vertex_info.typ
let predecessors dag key = (vertex_info dag key).Vertex_info.predecessors
let view dag key = (vertex_info dag key).Vertex_info.view
let successors dag key = (vertex_info dag key).Vertex_info.successors
let inputs dag = dag.inputs
let vertices dag = Vertex.Set.of_map_keys dag.vertex_infos
let enclosing_parallel_blocks dag key = (vertex_info dag key).Vertex_info.enclosing_parallel_blocks
let vertices_in_block dag ~parallel_block_vertex:key =
  match (vertex_info dag key).Vertex_info.vertices_in_block with
  | None -> Vertex.Set.empty
  | Some vs -> vs

let unroll dag key =
  let open Vertex_view in
  (* Only recursive production is Parallel_block *)
  match view dag key with
  | Parallel_block (_, t) -> Some t
  | Function _ | Binop _ | Unop _ | Literal _
  | Input _  | Index | Struct_Init _ | Access _ -> None

type 'a counter = unit -> 'a
let make_counter ~(seed:'a) ~(next:'a -> 'a) =
  let ctr = ref seed in
  fun () -> Ref.replace ctr next; !ctr

type dag_fun = {
  dag_name : string;
  dag_graph : dag;
} [@@deriving sexp]

module Invert_vertex = Utils.Inverter (Vertex)

(** Local variable context. *)
module Context = struct
  type t = {
    local_vars : Vertex.t String.Map.t;
    enclosing_parallel_blocks : Vertex.t list;
  }
end

(** Result of translation. *)
module Result = struct
  type t = {
    views : Vertex_view.t Vertex.Map.t;
    predecessors : Vertex.t list Vertex.Map.t;
    enclosing_parallel_blocks : Vertex.t list Vertex.Map.t;
    vertex : Vertex.t;
    types : Tc.typ Vertex.Map.t;
  } [@@deriving sexp]

  (** Return a result for the given vertex, additionally adding singleton
   * bindings for in the maps if provided as optional arguments. *)
  let empty_of
    : ?view:Vertex_view.t option
    -> ?predecessors:Vertex.t list option
    -> ?enclosing_parallel_blocks:Vertex.t list
    -> Vertex.t
    -> Tc.typ
    -> t =
    let to_singleton vertex =
      Option.value_map ~f:(Vertex.Map.singleton vertex) ~default:Vertex.Map.empty in
    fun ?(view=None) ?(predecessors=None) ?(enclosing_parallel_blocks=[]) (vertex : Vertex.t) (typ : Tc.typ) ->
      let views = to_singleton vertex view in
      let predecessors = to_singleton vertex predecessors in
      let enclosing_parallel_blocks = Vertex.Map.singleton vertex enclosing_parallel_blocks in
      let types = Vertex.Map.singleton vertex typ in
      { views; predecessors; vertex; enclosing_parallel_blocks; types; }

  (**
   * Bias indicates whether to take vertex from left or right argument.
   * Otherwise, merges; failing if there is a duplicate key in the merged
   * maps.
   *)
  let union_with_bias : bias:[ `Left | `Right ] -> t -> t -> t =
    let merge_exn = Map.merge_skewed ~combine:(fun ~key -> failwith "Duplicate key.") in
    let merge_checking_equality = Map.merge_skewed ~combine:(fun ~key v1 v2 ->
      if v1 <> v2 then failwith "Duplicate, not equal type." else v1) in
    fun ~bias result1 result2 -> {
      views = merge_exn result1.views result2.views;
      predecessors = merge_exn result1.predecessors result2.predecessors;
      types = merge_checking_equality result1.types result2.types;
      (* These are redundant but that's ok. Just take either result. *)
      enclosing_parallel_blocks =
        Map.merge_skewed ~combine:(fun ~key -> Fn.const)
          result1.enclosing_parallel_blocks
          result2.enclosing_parallel_blocks;
      vertex = match bias with
        | `Left -> result1.vertex
        | `Right -> result2.vertex;
    }
end

(* All the below "loop" functions make use of the following mutable state:
 *   - next_vertex for determining fresh vertex numberings.
 * next_vertex will always return a globally unique vertex, even across
 * invocations of `of_ast`.
 *)
let next_vertex = make_counter ~seed:0l ~next:Int32.succ

(** Dag of ast *)
type t = dag_fun list [@@deriving sexp]
let of_ast : Tc.typ Ast.fun_defn list -> t =

  (* Returns id of expression vertex. *)
  let rec loop_expr (ctx : Context.t) ((typ, expr) : Tc.typ Ast.expr) : Result.t =
    let vertex_info = match expr with
      | Ast.Fun_call fun_call ->
          let vertex = next_vertex () in
          let view = Vertex_view.Function fun_call.call_name in
          let results = List.map fun_call.call_args ~f:(loop_arg ctx) in
          `New_vertex (vertex, view, results, `No_additional_results)
      | Ast.Parallel parallel ->
          let vertex = next_vertex () in
          let vertex_binding = next_vertex () in
          let result_expr = loop_expr ctx parallel.parallel_arg in
          let result_binding = Result.empty_of vertex_binding (fst parallel.parallel_type)
            ~enclosing_parallel_blocks:Context.(ctx.enclosing_parallel_blocks)
            ~view:(Some (Vertex_view.Input parallel.parallel_ident))
          in
          let ctx' = Context.{
            enclosing_parallel_blocks = vertex :: ctx.enclosing_parallel_blocks;
            local_vars = Map.add_exn Context.(ctx.local_vars)
              ~key:parallel.parallel_ident ~data:vertex_binding;
          } in
          let { Result.vertex = return_vertex; _; } as result = loop_stmts ctx' parallel.parallel_body in
          let view = Vertex_view.Parallel_block (vertex_binding, return_vertex) in
          `New_vertex (vertex, view, [result_expr], `With_additional_results [result; result_binding])
      | Ast.Const i ->
          let vertex = next_vertex () in
          let view = Vertex_view.(Literal (Int32 i)) in
          `New_vertex (vertex, view, [], `No_additional_results)
      | Ast.Float f ->
          let vertex = next_vertex () in
          let view = Vertex_view.(Literal (Float f)) in
          `New_vertex (vertex, view, [], `No_additional_results)
      | Ast.Bool b ->
          let vertex = next_vertex () in
          let view = Vertex_view.(Literal (Bool b)) in
          `New_vertex (vertex, view, [], `No_additional_results)
      | Ast.Binop binop ->
          let vertex = next_vertex () in
          let view = Vertex_view.Binop binop.binary_operator in
          let result1 = loop_expr ctx binop.binary_operand1 in
          let result2 = loop_expr ctx binop.binary_operand2 in
          `New_vertex (vertex, view, [result1; result2], `No_additional_results)
      | Ast.Unop unop ->
          let vertex = next_vertex () in
          let view = Vertex_view.Unop unop.unary_operator in
          let result = loop_expr ctx unop.unary_operand in
          `New_vertex (vertex, view, [result], `No_additional_results)
      | Ast.Index i ->
          let vertex = next_vertex () in
          let view = Vertex_view.Index in
          let src = loop_expr ctx i.index_source in
          let expr = loop_expr ctx i.index_expr in
          `New_vertex(vertex, view, [src; expr], `No_additional_results)
      | Ast.Access (s, f) ->
          let vertex = next_vertex () in
          let view = Vertex_view.Access f in
          let src = loop_expr ctx s in
          `New_vertex (vertex, view, [src], `No_additional_results)
      | Ast.Struct_Init s ->
          let vertex = next_vertex () in
          let fields = List.map ~f:(fun f -> Ast.(f.field_name)) s.struct_fields in
          let view = Vertex_view.Struct_Init (typ, fields) in
          let results = List.map ~f:(fun f -> loop_expr ctx Ast.(f.field_expr)) s.struct_fields in
          `New_vertex (vertex, view, results, `No_additional_results)
      | Ast.Variable v ->
          let vertex = Map.find_exn Context.(ctx.local_vars) v in
          `Reused_vertex vertex
    in

    (* Take action based on vertex status. *)
    match vertex_info with
    | `Reused_vertex vertex -> Result.empty_of vertex typ
    | `New_vertex (vertex, view, results, result_status) ->
        (* Fold results together. *)
        let predecessors = List.map results ~f:(fun r -> Result.(r.vertex))
          |> function
            (* Short circuit: add no binding if the predecessors is empty. *)
            | [] -> None
            | xs -> Some xs
        in
        let results_to_add = results @ match result_status with
          | `No_additional_results -> []
          | `With_additional_results rs -> rs
        in
        let init = Result.empty_of vertex typ
          ~enclosing_parallel_blocks:Context.(ctx.enclosing_parallel_blocks)
          ~predecessors ~view:(Some view)
        in
        List.fold_left results_to_add ~init ~f:(Result.union_with_bias ~bias:`Left)

  and loop_arg (ctx : Context.t) (arg : Tc.typ Ast.arg) : Result.t = match arg with
    | Ast.Bare_binop (typ, binop) ->
        let vertex = next_vertex () in
        let view = Vertex_view.(Literal (Bare_binop binop)) in
        Result.empty_of vertex typ ~view:(Some view)
          ~enclosing_parallel_blocks:Context.(ctx.enclosing_parallel_blocks)
    | Ast.Bare_unop (typ, unop) ->
        let vertex = next_vertex () in
        let view = Vertex_view.(Literal (Bare_unop unop)) in
        Result.empty_of vertex typ ~view:(Some view)
          ~enclosing_parallel_blocks:Context.(ctx.enclosing_parallel_blocks)
    | Ast.Expr expr -> loop_expr ctx expr

  (* The vertex field of the result is the return expression vertex. *)
  and loop_stmts (ctx : Context.t) (stmts : Tc.typ Ast.stmt list) : Result.t =
    (* Returns the vertex of the expression involved in the binding or return. *)
    let rec loop_stmt (ctx : Context.t) (stmt : Tc.typ Ast.stmt) : Result.t =
      match stmt with
      | Ast.Let lstmt -> loop_expr ctx lstmt.let_expr
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
            | Ast.Let { let_ident = ident } ->
                Context.{ ctx with
                  local_vars = Map.add_exn ctx.local_vars ~key:ident ~data:Result.(result.vertex)
                }
            | Ast.Return _ -> ctx
          in

          (ctx', Some result))
    in

    Option.value_exn return_result
  in

  let of_fun_defn (f : Tc.typ Ast.fun_defn) : dag_fun =
    let input_names = Ast.(List.map f.fun_params ~f:(fun p -> p.param_ident)) in
    let inputs = List.map input_names ~f:(fun _ -> next_vertex ()) in
    let input_pairs = List.zip_exn input_names inputs in

    (* Initial local variable context *)
    let init_ctx = Context.{
      local_vars = String.Map.of_alist_exn input_pairs;
      enclosing_parallel_blocks = [];
    } in

    let result = loop_stmts init_ctx Ast.(f.fun_body) in
    { dag_name = Ast.(f.fun_name);
      dag_graph =
        let Result.{ predecessors; views; vertex = return_vertex; enclosing_parallel_blocks; types; } = result in
        (* Add input views. *)
        let views =
          List.fold_left input_pairs ~init:views
            ~f:(fun acc (name, vtx) -> Map.add_exn acc ~key:vtx ~data:(Vertex_view.Input name))
        in
        let types =
          List.fold2_exn inputs Ast.(f.fun_params) ~init:types
            ~f:(fun acc vtx param ->
              let (typ, _) = Ast.(param.param_type) in
              (* No add_exn because some param types have already been added. *)
              Map.set acc ~key:vtx ~data:typ)
        in
        let successors = Invert_vertex.invert predecessors in
        let vertices_in_block = Invert_vertex.invert enclosing_parallel_blocks in
        let vertex_infos = Vertex_info.collect_into_map
          (* Look at all this data I collected: *)
          (* Hope you like it. :) *)
          ~successors
          ~predecessors
          ~views
          ~enclosing_parallel_blocks
          ~vertices_in_block
          ~types
        in
        { vertex_infos; return_vertex; inputs; }
    }
  in
  List.map ~f:of_fun_defn

let renumber_with (dag : dag) ?(remove=Fn.const false) (new_number : Vertex.t -> Vertex.t) : dag =
  let vertex_info_list = Map.to_alist dag.vertex_infos in
  let vertex_info_list' = List.filter_map vertex_info_list ~f:(fun (k, v) ->
    if remove k then None else
    let v' = Vertex_info.{
      typ = v.typ;
      successors = Vertex.Set.map ~f:new_number v.successors;
      predecessors = List.map ~f:new_number v.predecessors;
      view = begin
        let open Vertex_view in
        match v.view with
        | Parallel_block (vtx1, vtx2) -> Parallel_block (new_number vtx1, new_number vtx2)
        | x -> x
      end;
      enclosing_parallel_blocks = List.map ~f:new_number v.enclosing_parallel_blocks;
      vertices_in_block = Option.map ~f:(Vertex.Set.map ~f:new_number) v.vertices_in_block;
    } in Some (new_number k, v'))
  in
  {
    inputs = List.map ~f:new_number dag.inputs;
    return_vertex = new_number dag.return_vertex;
    vertex_infos = Vertex.Map.of_alist_exn vertex_info_list';
  }

let renumber_map (dag : dag) (map : Vertex.t Vertex.Map.t) : dag =
  renumber_with dag (fun k -> Option.value ~default:k (Map.find map k))
    ~remove:(Map.mem map)

let renumber (dag : dag) : dag =
  let new_number =
    let table = Vertex.Table.create () in
    Hashtbl.find_or_add table ~default:next_vertex
  in renumber_with dag new_number

let substitute (source : dag) (vertex : Vertex.t) (target : dag) : dag =
  let source = renumber source in
  let enclosing_blocks = enclosing_parallel_blocks target vertex in

  (* As we fold through the contents of source, we do two things:
   *   - Update the enclosing parallel blocks for all members of source.
   *   - Update the vertices_in_block value for target.
   * This doesn't wire together inputs/ouputs yet; we do that in the next step.
   *)
  let (source, target) =
    Map.fold source.vertex_infos ~init:(source, target) ~f:(fun ~key ~data (source, target) ->
      let source_vertex_infos = Map.update source.vertex_infos key ~f:(function
        | None -> failwith "Impossible: vertex not found in source."
        | Some vertex_info -> Vertex_info.{ vertex_info with
            enclosing_parallel_blocks =
              vertex_info.enclosing_parallel_blocks @ enclosing_blocks
          })
      in
      let target_vertex_infos =
        List.fold_left enclosing_blocks ~init:target.vertex_infos ~f:(fun acc vtx ->
          Map.update acc vtx ~f:(function
            | None -> failwith "Unknown enclosing parallel block."
            | Some vertex_info -> Vertex_info.{ vertex_info with
                vertices_in_block =
                  Option.map ~f:(Fn.flip Set.add key) vertex_info.vertices_in_block
              }))
      in
      ({ source with vertex_infos = source_vertex_infos },
       { target with vertex_infos = target_vertex_infos }))
  in

  (**
   * Now we wire together the predecessors of the vertex into the inputs of source.
   *)
  let input_pred_pairs = List.zip_exn source.inputs (predecessors target vertex) in
  (* First subgoal: update successors in target. *)
  let target = List.fold_left input_pred_pairs ~init:target
    ~f:(fun target (input, pred) ->
      let target_vertex_infos = Map.update target.vertex_infos pred ~f:(function
        | None -> failwith "Impossible: not found predecessor."
        | Some vertex_info -> Vertex_info.{ vertex_info with
            successors = Set.remove vertex_info.successors vertex
              |> Set.union (successors source input);
          })
      in Vertex_info.{ target with vertex_infos = target_vertex_infos })
  in
  (* Next subgoal: globally rename input vertices in source to the predecessors. *)
  let source = renumber_map source (Vertex.Map.of_alist_exn input_pred_pairs) in

  (* Now we wire together the return vertex of source to the successors of the function call in target
   * by renaming the function call to the return vertex. *)
  let target = renumber_map target (Vertex.Map.singleton vertex source.return_vertex) in

  (* Now for the death blow: add everything from source into target. *)
  { target with vertex_infos = Map.merge_skewed target.vertex_infos source.vertex_infos
      ~combine:(fun ~key -> failwithf "Duplicate key `%ld` in source and target. :(" key ()) }

let inline (dag_fun : dag_fun) (ast : t) : dag_fun =
  let dag = dag_fun.dag_graph in
  let ast_map = String.Map.of_alist_exn (List.map ast ~f:(fun d -> (d.dag_name, d.dag_graph))) in
  let rec loop (visited : Vertex.Set.t) (candidates : Vertex.Set.t) (dag : dag) : dag =
    match Vertex.Set.choose candidates with
    | None -> dag
    | Some vertex ->
        let visited' = Set.add visited vertex in
        let open Vertex_view in
        match view dag vertex with
        | Function (Ast.Fun_ident name) ->
            let f = Map.find_exn ast_map name in
            let dag' = substitute f vertex dag in
            let candidates' = Set.diff (Set.of_map_keys dag'.vertex_infos) visited' in
            loop visited' candidates' dag'
        | _ ->
          let candidates' = Vertex.Set.remove candidates vertex in
          loop visited' candidates' dag
  in
  { dag_fun with dag_graph = loop Vertex.Set.empty (Vertex.Set.of_map_keys dag.vertex_infos) dag }
