open Core

module Vertex = Dag.Vertex
module Vertex_view = Dag.Vertex_view

type context = Temp.t Vertex.Map.t

let tuple1_of_list_exn : 'a list -> 'a = function
  | [x] -> x
  | _ -> failwith "Not singleton list."

let tuple2_of_list_exn : 'a list -> 'a * 'a = function
  | [x; y] -> (x, y)
  | _ -> failwith "Not doubleton list."

let uncons (xs : 'a list) : ('a * 'a list) option = match xs with
  | [] -> None
  | x :: xs -> Some (x, xs)

let run (dag : Dag.dag) (traversal : Dag_traversal.traversal) : Ir.t =

  (* Look up, being smart about literals. *)
  let lookup_exn (ctx : context) (key : Vertex.t) : Ir.operand =
    match Dag.view dag key with
    | Vertex_view.Literal (Vertex_view.Int32 i) -> Ir.Const i
    | _ ->
    match Map.find ctx key with
    | Some x -> Ir.Temp x
    | None -> failwithf "Missing key: `%s`" (Sexp.to_string_hum (Vertex.sexp_of_t key)) ()
  in

  let convert_op (arg : Vertex.t) : Ir.Operator.t =
    let open Vertex_view in
    match Dag.view dag arg with
    | Literal (Bare_binop b) -> Ir.Operator.Binop b
    | Literal (Bare_unop u) -> Ir.Operator.Unop u
    | Literal (Int32 _) | Parallel_block _
    | Function _ | Binop _ | Unop _ | Input _ -> failwith "Not operator."
  in

  let convert_call_name (call_name : Ast.call_name) (args : Vertex.t list)
    : Ir.fun_call * Vertex.t list =
    (* Figure out the datatype constructor *)
    let ctor = match call_name with
      | Ast.Reduce -> `Unary (fun op -> Ir.Reduce op)
      | Ast.Map -> `Unary (fun op -> Ir.Map op)
      | Ast.Zip_with -> `Unary (fun op -> Ir.Zip_with op)
      | Ast.Dim n -> `Nullary (Ir.Dim n)
      | Ast.Transpose -> `Nullary Ir.Transpose
      | Ast.Fun_ident _ -> failwith "You didn't inline before trying to translate."
    in
    match ctor with
    | `Unary f ->
      begin
        match uncons args with
        | None -> failwith "Invariant broken."
        | Some (hd_arg, tl_args) ->
            let op = convert_op hd_arg in
            (f op, tl_args)
      end
    | `Nullary const -> (const, args)
  in

  (* Convert a single statement *)
  let rec convert_tree (ctx : context) (t : Dag_traversal.traversal_tree) : context * Ir.stmt =
    match t with
    | Dag_traversal.Just v ->
      begin
        let dest_stmt_opt = match Dag.view dag v with
          | Vertex_view.Binop binop ->
              let (pred1, pred2) = Dag.predecessors dag v |> tuple2_of_list_exn in
              let (arg1, arg2) = (lookup_exn ctx pred1, lookup_exn ctx pred2) in
              let dest = Temp.next () in
              let stmt = Ir.Binop (dest, binop, arg1, arg2) in
              Some (dest, stmt)
          | Vertex_view.Unop unop ->
              let pred = Dag.predecessors dag v |> tuple1_of_list_exn in
              let arg = lookup_exn ctx pred in
              let dest = Temp.next () in
              let stmt = Ir.Unop (dest, unop, arg) in
              Some (dest, stmt)
          | Vertex_view.Function call_name ->
              let preds = Dag.predecessors dag v in
              let dest = Temp.next () in
              let (fun_call, preds) = convert_call_name call_name preds in
              let args = List.map preds ~f:(lookup_exn ctx) in
              let stmt = Ir.Fun_call (dest, fun_call, args) in
              Some (dest, stmt)
          | Vertex_view.Literal _ -> None
          | Vertex_view.Input input ->
              failwithf "Encountered unexpected parameter: `%s`" input ()
          | Vertex_view.Parallel_block _ ->
              failwith "Encountered parallel block when simple vertex was expected."
        in
        Option.value_map dest_stmt_opt ~default:(ctx, Ir.Nop)
          ~f:(fun (dest, stmt) ->
            let ctx' = Map.add_exn ctx ~key:v ~data:dest in
            (ctx', stmt))
      end
    | Dag_traversal.Block _ -> failwith "Hey"

  (* Convert group of statements *)
  and convert (ctx : context) (t : Dag_traversal.traversal) : Ir.stmt list =
    List.folding_map traversal ~init:ctx ~f:convert_tree
  in

  let inputs = Dag.inputs dag in
  let params = List.map ~f:(fun _ -> Temp.next ()) inputs in
  let init_ctx = Vertex.Map.of_alist_exn (List.zip_exn inputs params) in
  Ir.{
    params = params;
    body = convert init_ctx traversal;
  }

