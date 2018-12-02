open Core

module A_air = Annotated_air
module Param = A_air.Param
module Expr = A_air.Expr
module Length_expr = A_air.Length_expr
module Many_fn = Utils.Many_fn

(* returns dim * f *)
let rec build_length_function : Tc.typ -> int * (Expr.t -> (Expr.t, Expr.t) Many_fn.t) * Temp.t list = function
  | Tc.Array typ ->
      let (len, index, f) = build_length_function typ in
      let t = Temp.next Tc.Int () in
      (len + 1, (fun t -> Many_fn.Fun (fun e -> index (Expr.Index (t, e)))), t :: f)
  | _ -> (0, (fun t -> Many_fn.Result t), [])

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
  fun t ->
    let t' = canonicalize t in
    match Map.find A_air.(ctx.result.buffer_infos) t' with
    | None -> failwithf "Not found: %s (canonically %s)\n%s"
        (Sexp.to_string_hum (Temp.sexp_of_t t))
        (Sexp.to_string_hum (Temp.sexp_of_t t'))
        (Sexp.to_string_hum (Temp.Map.sexp_of_t A_air.sexp_of_buffer_info A_air.(ctx.result.buffer_infos)))
        ()
    | Some bi -> bi

let annotate_array_view
  (ctx : context)
  (av : Air.array_view)
  : A_air.buffer_info =
  let open A_air in
  let rec loop : Air.array_view -> buffer_info = function
    | (_, Air.Array t) -> lookup_exn ctx t
    | (_, Air.Array_index (t1, t2)) ->
        let bi = lookup_exn ctx t1 in
        { dim = bi.dim - 1;
          length = List.tl_exn bi.length;
          index = Many_fn.app_exn bi.index (Expr.Temp t2);
          typ = (match bi.typ with Tc.Array t -> t | _ -> failwith "No.");
        }

    | (typ, Air.Transpose av) ->
        let bi = loop av in
        assert (typ = bi.typ);
        { bi with
            index =
              let open Many_fn in
              Fun (fun expr1 -> Fun (fun expr2 ->
                app_many_exn bi.index [expr1; expr2;]));
        }
    | (typ, Air.Reverse av) ->
        let bi = loop av in
        assert (typ = bi.typ);
        let n_minus_1 = (* n - 1 *)
          Expr.Call (Ir.Operator.Binop Ast.Minus,
            [ Length_expr.to_expr (List.hd_exn bi.length); Expr.Const 1l; ]) in
        { bi with
            index =
              let open Many_fn in
              Fun (fun expr ->
                app_exn bi.index
                  (Expr.Call (Ir.Operator.Binop Ast.Minus, [ n_minus_1; expr; ]))
                  ~msg:"App_exn reverse")
        }
    | (typ, Air.Tabulate (b,e,s)) ->
        let bi = loop av in 
        assert (typ = bi.typ);
        bi (* Todo: What should go here? *)

    | (typ, Air.Zip_with (op, avs)) ->
        let length, bis = match avs with
          | hd :: tl ->
              let bi = loop hd in
              let bis = List.map tl ~f:loop in
              (bi.length, bi :: bis)
          | _ -> failwith "Empty zipwith!"
        in
        let dim = dim typ in
        assert (dim = 1); (* Don't know how to handle other cases yet. *)
        { dim;
          length;
          typ;
          index =
            let open Many_fn in
            Fun (fun expr ->
              let e =
                let f bi = result_exn ~msg:"result_exn zip_with"
                  (app_exn ~msg:"app_exn zip_with" bi.index expr)
                in Expr.Call (op, List.map bis ~f)
              in
              Result e);
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
    | Air.Index (t1, t2) -> Temp.Set.of_list [t1; t2;]
    | Air.Dim (n, av) ->
        let bi = annotate_array_view ctx av in
        used_of_length_expr (List.nth_exn A_air.(bi.length) n)

let used_of_primitive (ctx : context) (p : Air.primitive) : Temp.Set.t = 
  let of_op = used_of_operand ctx in
  match p with
    | Air.Min (a,b) | Air.Max (a,b) -> Set.union (of_op a) (of_op b)
    | Air.F2I (a) | Air.I2F (a) -> of_op a

let defined_of_dest : Ir.dest -> Temp.Set.t =
  function
    | Ir.Return _ -> Temp.Set.empty
    | Ir.Dest t -> Temp.Set.singleton t

let rec annotate_par_stmts (ctx : context) (stmts : Air.par_stmt list) : context =
  List.fold_left stmts ~init:ctx ~f:annotate_par_stmt

and annotate_par_stmt (ctx : context) (stmt : Air.par_stmt) : context =
  match stmt with
  | Air.Parallel (dest, id, tavs, body) ->
      let buffer_infos = List.map tavs ~f:(fun (_, _, av) ->
        annotate_array_view ctx av) in
      let ctx =
        { ctx with
            result =
              let result = ctx.result in
              A_air.{ result with
                buffer_infos = List.fold2_exn tavs buffer_infos ~init:result.buffer_infos
                  ~f:(fun m (key, _, _) data -> Map.add_exn m ~key ~data);
              };
        }
      in
      let (kernel_ctx, ctx) = annotate_seq_stmt ctx body in
      { ctx with result = A_air.
          { ctx.result with
              kernel_infos = Map.add_exn ctx.result.kernel_infos ~key:id ~data:(kernel_ctx_to_kernel_info kernel_ctx);
          }
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
      let buffer_info = annotate_array_view ctx av in
      (kernel_ctx,
       { ctx with result = A_air.
           { ctx.result with
               buffer_infos =
                 Map.add_exn ctx.result.buffer_infos ~key:t ~data:buffer_info;
           }
       })
  | Air.Run (Ir.Dest t, av) ->
      let defined = Temp.Set.singleton t in
      let buffer_info = annotate_array_view ctx av in
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
      let buffer_info = annotate_array_view ctx av in
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
  | Air.For (dest, (t, t_idx, av), body) ->
      let defined = defined_of_dest dest
        |> Fn.flip Set.add t
        |> Fn.flip Set.add t_idx
      in
      let buffer_info = annotate_array_view ctx av in
      let ctx = { ctx with result = A_air.
        { ctx.result with
            buffer_infos =
              Map.add_exn ctx.result.buffer_infos ~key:t ~data:buffer_info; };
      } in
      let (kernel_ctx', ctx) = recur ctx body in
      ({ defined = Set.union defined (Set.union kernel_ctx.defined kernel_ctx'.defined);
         used = Set.union kernel_ctx.used kernel_ctx'.used;
         additional_buffers = Set.union kernel_ctx.additional_buffers kernel_ctx'.additional_buffers;
       }, ctx)

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
    | Air.Primitive (dest,src) ->
        let used = used_of_primitive ctx src in
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

let param_of_temp : Temp.t -> A_air.Param.t * A_air.buffer_info option =
  fun p ->
    let typ = Temp.to_type p in
    let (dim, index, ts) = build_length_function typ in
    match dim with
    | 0 -> (Param.Not_array p, None)
    | _ -> (Param.Array (p, ts), Some A_air.{
        length = List.map ~f:(fun t -> Length_expr.Temp t) ts;
        index = index (Expr.Temp p);
        dim;
        typ;
      })

let annotate (air : Air.t) : A_air.result =
  let (buffer_infos, params) =
    List.fold_map Air.(air.params) ~init:Temp.Map.empty ~f:(fun ctx p ->
      match param_of_temp p with
      | (p', None) -> (ctx, p')
      | (p', Some bi) -> (Map.add_exn ctx ~key:p ~data:bi, p'))
    in

    (* Whether or not there is an out-buffer as a param depends on whether
     * the return type is an array. *)
    let t = Temp.next Air.(air.return_type) () in
    let result = match param_of_temp t with
      | (p, Some bi) -> A_air.{
          params = p :: params;
          buffer_infos = Map.add_exn buffer_infos ~key:t ~data:bi;
          kernel_infos = Id.Map.empty;
          out_param = Some t;
        }

      (* No need to have a separate parameter at all if the function returns directly. *)
      | _ -> A_air.{
        params;
        buffer_infos;
        kernel_infos = Id.Map.empty;
        out_param = None;
      }
    in
    let ctx = { result; aliases = Temp.Map.empty; } in
    let ctx' = annotate_par_stmt ctx Air.(air.body) in
    ctx'.result
