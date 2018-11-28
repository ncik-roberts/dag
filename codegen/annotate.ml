open Core

module A_air = Annotated_air
module Param = A_air.Param
module Expr = A_air.Expr

(* returns dim * f *)
let rec build_length_function : Tc.typ -> int * (Expr.t -> Expr.t list -> Expr.t) * (int -> Temp.t) = function
  | Tc.Array typ ->
      let (len, index, f) = build_length_function typ in
      let t = Temp.next Tc.Int () in
      (len + 1,
          (fun t -> function
            | e :: es -> index (Expr.Index (t, e)) es
            | [] -> failwith "Invalid"),
          function 0 -> t | n -> f (n-1))
  | _ -> (0, (fun t -> function
    | [] -> t
    | _ -> failwith "Invalid."),
    fun _ -> failwith "Invalid dimension.")

let dim : Tc.typ -> int = Fn.compose Tuple3.get1 build_length_function

type context = A_air.result
type kernel_ctx = {
  used : Temp.Set.t;
  defined : Temp.Set.t;
  additional_buffers : A_air.buffer_info Temp.Map.t;
}

let kernel_ctx_to_kernel_info : kernel_ctx -> A_air.kernel_info = fun kc ->
  A_air.{
    free_variables = Set.diff kc.used kc.defined;
    additional_buffers = kc.additional_buffers;
  }

let annotate_array_view
  (ctx : context)
  (t, av : Temp.t * Air.array_view) : A_air.buffer_info =
  let open A_air in
  let rec loop : Air.array_view -> buffer_info = function
    | (_, Air.Array t) -> Map.find_exn ctx.buffer_infos t
    | (typ, Air.Transpose av) ->
        let bi = loop av in
        assert (typ = bi.typ);
        { bi with
            index = (function
              | expr1 :: expr2 :: exprs ->
                  bi.index (expr2 :: expr1 :: exprs)
              | _ -> failwith "Not enough :(");
        }
    | (typ, Air.Reverse av) ->
        let bi = loop av in
        assert (typ = bi.typ);
        let n_minus_1 = (* n - 1 *)
          Expr.Call (Ir.Operator.Binop Ast.Minus,
            [ Length_expr.to_expr (bi.length 0); Expr.Const 1l; ]) in
        { bi with
            index = (function
              | expr :: exprs ->
                  bi.index
                    (Expr.Call (Ir.Operator.Binop Ast.Minus, [ n_minus_1; expr; ]) :: exprs)
              | _ -> failwith "Not enough :(");
        }
    | (typ, Air.Zip_with (op, avs)) ->
        let length, bis = match avs with
          | hd :: tl ->
              let bi = loop hd in
              let bi_len = bi.length 0 in
              let bis = List.map tl ~f:(fun av ->
                let bi = loop av in
                if not (Length_expr.equals bi_len (bi.length 0))
                  then failwith "Unequal lengths in zipwith.";
                bi)
              in (bi.length, bi :: bis)
          | _ -> failwith "Empty zipwith!"
        in
        let dim = dim typ in
        assert (dim = 1); (* Don't know how to handle other cases yet. *)
        { dim;
          length;
          typ;
          index = (fun expr -> Expr.Call (op, List.map ~f:(fun bi -> bi.index expr) bis));
        }
  in
  loop av

let rec annotate_par_stmts (ctx : context) (stmts : Air.par_stmt list) : context =
  List.fold_left stmts ~init:ctx ~f:annotate_par_stmt

and annotate_par_stmt (ctx : context) (stmt : Air.par_stmt) : context =
  match stmt with
  | Air.Parallel (dest, id, tavs, body) ->
      let buffer_infos = List.map tavs ~f:(annotate_array_view ctx) in
      let (kernel_ctx, ctx) = annotate_seq_stmt ctx body in
      A_air.{ ctx with
          buffer_infos = List.fold2_exn tavs buffer_infos ~init:ctx.buffer_infos
            ~f:(fun m (key, _) data -> Map.add_exn m ~key ~data);
          kernel_infos = Map.add_exn ctx.kernel_infos ~key:id ~data:(kernel_ctx_to_kernel_info kernel_ctx);
      }

and empty_kernel_ctx : kernel_ctx =
  { used = Temp.Set.empty;
    defined = Temp.Set.empty;
    additional_buffers = Temp.Map.empty;
  }

and annotate_seq_stmt
  (ctx : context)
  ?(kernel_info=empty_kernel_ctx)
  (stmt : Air.seq_stmt) : kernel_ctx * context =
  match stmt with
  | _ -> failwith ":("

let annotate (air : Air.t) : context =
  let (buffer_infos, params) =
    List.fold_map Air.(air.params) ~init:Temp.Map.empty ~f:(fun ctx p ->
      let typ = Temp.to_type p in
      let (dim, index, f) = build_length_function typ in
      match dim with
      | 0 -> (ctx, Param.Not_array p)
      | _ ->
          let ctx' = Map.add_exn ctx ~key:p
            ~data:A_air.{
              length = Fn.compose (fun t -> Length_expr.Temp t) f;
              index = index (Expr.Temp p);
              dim;
              typ;
            }
          in (ctx', Param.Array (p, List.init dim ~f))
    )
    in
    let ctx = A_air.{ params; buffer_infos; kernel_infos = Id.Map.empty; } in
    let ctx' = annotate_par_stmt ctx Air.(air.body) in
    ctx'
