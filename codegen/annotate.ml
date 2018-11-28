open Core

module A_air = Annotated_air
module Param = A_air.Param
module Expr = A_air.Expr
module Length_expr = A_air.Length_expr

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

type context = {
  result : A_air.result;
  aliases : Temp.t Temp.Map.t;
}

type kernel_context = {
  used : Temp.Set.t;
  defined : Temp.Set.t;
  additional_buffers : Temp.Set.t;
}

let kernel_ctx_to_kernel_info : kernel_context -> A_air.kernel_info = fun kc ->
  A_air.{
    free_variables = Set.diff kc.used kc.defined;
    additional_buffers = kc.additional_buffers;
  }

let lookup_exn (ctx : context) : Temp.t -> A_air.buffer_info =
  let rec canonicalize (t : Temp.t) : Temp.t =
    Option.value_map (Map.find ctx.aliases t) ~default:t ~f:canonicalize
  in
  Fn.compose (Map.find_exn A_air.(ctx.result.buffer_infos)) canonicalize

let annotate_array_view
  (ctx : context)
  (av : Air.array_view)
  ~(variety : A_air.buffer_info_sum) : A_air.buffer_info =
  let open A_air in
  let rec loop : Air.array_view -> buffer_info = function
    | (_, Air.Array t) -> lookup_exn ctx t
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
            variety;
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
          variety;
          index = (fun expr -> Expr.Call (op, List.map ~f:(fun bi -> bi.index expr) bis));
        }
  in
  loop av

let rec used_of_length_expr : Length_expr.t -> Temp.Set.t =
  function
    | Length_expr.Temp t -> Temp.Set.singleton t
    | Length_expr.Mult (e1, e2) -> Set.union (used_of_length_expr e1) (used_of_length_expr e2)

let used_of_operand (ctx : context) : Air.operand -> Temp.Set.t =
  function
    | Air.Const _ -> Temp.Set.empty
    | Air.Temp t -> Temp.Set.singleton t
    | Air.Dim (n, av) ->
        let bi = annotate_array_view ctx av ~variety:A_air.Run_array_view in
        used_of_length_expr (A_air.(bi.length) n)

let defined_of_dest : Ir.dest -> Temp.Set.t =
  function
    | Ir.Return _ -> Temp.Set.empty
    | Ir.Dest t -> Temp.Set.singleton t

let rec annotate_par_stmts (ctx : context) (stmts : Air.par_stmt list) : context =
  List.fold_left stmts ~init:ctx ~f:annotate_par_stmt

and annotate_par_stmt (ctx : context) (stmt : Air.par_stmt) : context =
  match stmt with
  | Air.Parallel (dest, id, tavs, body) ->
      let buffer_infos = List.map tavs ~f:(fun (_, av) -> annotate_array_view ctx av ~variety:A_air.Bound_parallel) in
      let (kernel_ctx, ctx) = annotate_seq_stmt ctx body in
      { ctx with
          result =
            let result = ctx.result in
            A_air.{ result with
              buffer_infos = List.fold2_exn tavs buffer_infos ~init:result.buffer_infos
                ~f:(fun m (key, _) data -> Map.add_exn m ~key ~data);
              kernel_infos = Map.add_exn result.kernel_infos ~key:id ~data:(kernel_ctx_to_kernel_info kernel_ctx);
            };
      }
  | Air.Seq stmt -> snd (annotate_seq_stmt ctx stmt)
  | Air.Par_stmt par_stmt -> annotate_par_stmt_stmt ctx par_stmt

and empty_kernel_ctx : kernel_context =
  { used = Temp.Set.empty;
    defined = Temp.Set.empty;
    additional_buffers = Temp.Set.empty;
  }

and annotate_par_stmt_stmt (ctx : context) (par : Air.par_stmt Air.stmt) : context =
  snd
    (annotate_stmt empty_kernel_ctx ctx
      (fun ?(kernel_ctx = empty_kernel_ctx) ctx x -> (kernel_ctx, annotate_par_stmt ctx x))
      par)

and annotate_seq_stmt_stmt (kernel_ctx : kernel_context) (ctx : context) (seq : Air.seq_stmt Air.stmt)
  : kernel_context * context =
    annotate_stmt kernel_ctx ctx annotate_seq_stmt seq

and annotate_stmt
  : type a.
      kernel_context
        -> context
        -> (?kernel_ctx : kernel_context -> context -> a -> kernel_context * context)
        -> a Air.stmt
        -> kernel_context * context = fun kernel_ctx ctx recur stmt ->
  match stmt with
  | Air.Run (Ir.Return t, av) ->
      let buffer_info = annotate_array_view ctx av ~variety:A_air.Run_array_view in
      (kernel_ctx,
       { ctx with result = A_air.
           { ctx.result with
               buffer_infos =
                 Map.add_exn ctx.result.buffer_infos ~key:t ~data:buffer_info;
           }
       })
  | Air.Run (Ir.Dest t, av) ->
      let defined = Temp.Set.singleton t in
      let buffer_info = annotate_array_view ctx av ~variety:A_air.Run_array_view in
      ({ kernel_ctx with
           defined = Set.union defined kernel_ctx.defined;
           additional_buffers = Set.add kernel_ctx.additional_buffers t;
       },
       { ctx with result = A_air.
           { ctx.result with
               buffer_infos =
                 Map.add_exn ctx.result.buffer_infos ~key:t ~data:buffer_info;
           }
       })
  | Air.Nop -> (kernel_ctx, ctx)
  | Air.Reduce (dest, _, operand, (t, av)) ->
      let defined = defined_of_dest dest in
      let used = used_of_operand ctx operand in
      let buffer_info = annotate_array_view ctx av ~variety:A_air.Bound_parallel in
      ({ defined = Set.union defined kernel_ctx.defined;
         used = Set.union used kernel_ctx.used;
         additional_buffers = Set.add kernel_ctx.additional_buffers t;
       },
       { ctx with result = A_air.
           { ctx.result with
               buffer_infos =
                 Map.add_exn ctx.result.buffer_infos ~key:t ~data:buffer_info;
           }
       })
  | Air.Block stmts ->
      List.fold_left stmts ~init:(kernel_ctx, ctx)
        ~f:(fun (k, c) -> recur ~kernel_ctx:k c)
  | Air.For (dest, (t, av), body) ->
      let defined = defined_of_dest dest in
      let buffer_info = annotate_array_view ctx av ~variety:A_air.Bound_parallel in
      let (kernel_ctx', ctx) = recur ctx body in
      ({ defined = Set.union defined (Set.union kernel_ctx.defined kernel_ctx'.defined);
         used = Set.union kernel_ctx.used kernel_ctx'.used;
         additional_buffers = Set.union kernel_ctx.additional_buffers kernel_ctx'.additional_buffers;
       },
       { ctx with result = A_air.
           { ctx.result with
               buffer_infos =
                 Map.add_exn ctx.result.buffer_infos ~key:t ~data:buffer_info; };
       })

and annotate_seq_stmt
  ?(kernel_ctx=empty_kernel_ctx)
  (ctx : context)
  (stmt : Air.seq_stmt) : kernel_context * context =
  let (kernel_ctx', ctx) = match stmt with
    | Air.Binop (dest, _, src1, src2) ->
        let used = Set.union (used_of_operand ctx src1) (used_of_operand ctx src2) in
        let defined = defined_of_dest dest in
        ({ used; defined; additional_buffers = Temp.Set.empty; }, ctx)
    | Air.Assign (dest, src) | Air.Unop (dest, _, src) ->
        let used = used_of_operand ctx src in
        let defined = defined_of_dest dest in
        ({ used; defined; additional_buffers = Temp.Set.empty }, ctx)
    | Air.Seq_stmt seq_stmt -> annotate_seq_stmt_stmt kernel_ctx ctx seq_stmt
  in

  (* Update kernel ctx based on use/def *)
  let kernel_ctx =
    { used = Set.union kernel_ctx.used kernel_ctx'.used;
      defined = Set.union kernel_ctx.defined kernel_ctx'.defined;
      additional_buffers = Set.union kernel_ctx.additional_buffers kernel_ctx.additional_buffers;
    }
  in

  (* Copy over aliases as necessary. *)
  let ctx = match stmt with
    | Air.Assign (Ir.Dest t_dst, Air.Temp t_src) ->
        { ctx with aliases = Map.add_exn ctx.aliases ~key:t_dst ~data:t_src }
    | _ -> ctx
  in

  (kernel_ctx, ctx)

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
              variety = Run_array_view;
            }
          in (ctx', Param.Array (p, List.init dim ~f))
    )
    in
    let ctx = {
      result = A_air.{ params; buffer_infos; kernel_infos = Id.Map.empty; };
      aliases = Temp.Map.empty;
    } in
    let ctx' = annotate_par_stmt ctx Air.(air.body) in
    ctx'
