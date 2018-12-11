open Core

module A_air = Annotated_air
module Param = A_air.Param
module Expr = A_air.Expr
module Length_expr = A_air.Length_expr
module Many_fn = Utils.Many_fn

(* returns dim * f *)
let rec build_length_function : Tc.typ -> int * (Expr.t -> Expr.t -> Expr.t) * Temp.t list = function
  | Tc.Array typ ->
      let (len, _, f) = build_length_function typ in
      let t = Temp.next Tc.Int () in
      (len + 1, (fun t e -> Expr.Index (t, e)), t :: f)
  | _ -> (0, (fun _ -> failwith "No."), [])

let dim : Tc.typ -> int = Fn.compose Tuple3.get1 build_length_function

type context = {
  result : A_air.result;
  aliases : Temp.t Temp.Map.t;
}

type kernel_context = {
  used : Temp.Set.t;
  defined : Temp.Set.t;
  additional_buffers : Temp.Set.t;
  return_buffer_info : A_air.buffer_info option;
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

  let app index t = Many_fn.app index t ~default:(fun arr -> Expr.Index (arr, t)) in

  let app_many index ts = List.fold_left ts ~init:index ~f:app
  in

  let rec loop : Air.array_view -> buffer_info = function
    | (_, Air.Array t) ->
        let bi = lookup_exn ctx t in
        { dim = bi.dim;
          length = bi.length;
          filtered_lengths = bi.filtered_lengths;
          index = Many_fn.Result (Expr.Temp t);
          typ = bi.typ;
        }
    | (_, Air.Array_index (t1, t2)) ->
        let bi = lookup_exn ctx t1 in
        { dim = bi.dim - 1;
          length = List.tl_exn bi.length;
          filtered_lengths = List.tl_exn bi.filtered_lengths;
          index = app bi.index (Expr.Temp t2);
          typ = (match bi.typ with Tc.Array t -> t | _ -> failwith "No.");
        }

    | (typ, Air.Transpose av) ->
        let bi = loop av in
        (if List.exists ~f:(Option.is_some) bi.filtered_lengths
           then failwith "Can't transpose filtered array.");
        assert (typ = bi.typ);
        { bi with
            index =
              let open Many_fn in
              Fun (fun expr1 -> Fun (fun expr2 ->
                app_many bi.index [expr2; expr1;]));
        }
    | (typ, Air.Reverse av) ->
        let bi = loop av in
        (if List.exists ~f:(Option.is_some) bi.filtered_lengths
           then failwith "Can't transpose reversed array.");
        assert (typ = bi.typ);
        let n_minus_1 = (* n - 1 *)
          Expr.Call (Ir.Operator.Binop Ast.Minus,
            [ Length_expr.to_expr (List.hd_exn bi.length); Expr.Const 1l; ]) in
        { bi with
            index =
              Many_fn.Fun (fun expr ->
                app bi.index (Expr.Call (Ir.Operator.Binop Ast.Minus, [ n_minus_1; expr; ])))
        }
    | (typ, Air.Tabulate (b, e, s)) ->
        { typ = Tc.(Array Int);
          dim = 1;
          length = List.return Length_expr.(Div (Minus (Temp e, Temp b), Temp s));
          filtered_lengths = [None];
          index = Many_fn.Fun (fun expr -> Many_fn.Result begin
            Expr.Call (Ir.Operator.Binop Ast.Plus, [
              Expr.Temp b;
              Expr.Call (Ir.Operator.Binop Ast.Times, [
                Expr.Temp s;
                expr;
            ])])
          end);
        }

    | (typ, Air.Zip_with (op, avs)) ->
        let hd, bis = match avs with
          | hd :: tl ->
              let bi = loop hd in
              let bis = List.map tl ~f:loop in
              (bi, bi :: bis)
          | _ -> failwith "Empty zipwith!"
        in
        let dim = dim typ in
        assert (dim = 1); (* Don't know how to handle other cases yet. *)
        { dim;
          length = hd.length;
          filtered_lengths = [ List.hd_exn hd.filtered_lengths ];
          typ;
          index =
            Many_fn.Fun (fun expr ->
              let e =
                let f bi = Many_fn.result_exn ~msg:"result_exn zip_with" (app bi.index expr)
                in Expr.Call (op, List.map bis ~f)
              in
              Many_fn.Result e);
        }
  in
  loop av

let rec used_of_length_expr : Length_expr.t -> Temp.Set.t =
  function
    | Length_expr.Temp t -> Temp.Set.singleton t
    | Length_expr.Mult (e1, e2) -> Set.union (used_of_length_expr e1) (used_of_length_expr e2)
    | Length_expr.Minus (e1, e2) -> Set.union (used_of_length_expr e1) (used_of_length_expr e2)
    | Length_expr.Div (e1, e2) -> Set.union (used_of_length_expr e1) (used_of_length_expr e2)

let rec used_of_array_view (ctx : context) (av : Air.array_view) : Temp.Set.t = match snd av with
    | Air.Zip_with (_, avs) -> List.fold_left avs ~init:Temp.Set.empty ~f:(fun s av -> Set.union s (used_of_array_view ctx av))
    | Air.Array t -> Temp.Set.singleton t
    | Air.Array_index _ -> Temp.Set.empty (* don't ask *)
    | Air.Reverse av -> used_of_array_view ctx av
    | Air.Transpose av -> used_of_array_view ctx av
    | Air.Tabulate (a, b, c) -> Temp.Set.of_list [a;b;c;]

let used_of_operand (ctx : context) : Air.operand -> Temp.Set.t =
  function
    | Air.Const _ | Air.Float _ | Air.Bool _ -> Temp.Set.empty
    | Air.Temp t -> Temp.Set.singleton t
    | Air.IndexOp (t1, t2) -> Temp.Set.of_list [t1; t2;]
    | Air.Dim (n, av) ->
        let bi = annotate_array_view ctx av in
        used_of_length_expr (List.nth_exn A_air.(bi.length) n)

let used_of_primitive (ctx : context) (p : Air.primitive) : Temp.Set.t =
  let of_op = used_of_operand ctx in
  match p with
    | Air.Min (a, b) | Air.Max (a, b) -> Set.union (of_op a) (of_op b)
    | Air.F2I a | Air.I2F a | Air.Log2 a -> of_op a

let used_of_struct_fields (ctx : context) (fields) =
  let of_op = used_of_operand ctx in
  List.fold fields ~init:Temp.Set.empty
  ~f:(fun set (_,fld) -> Set.union set (of_op fld))

let defined_of_dest : Ir.dest -> Temp.Set.t =
  function
    | Ir.Return _ -> Temp.Set.empty
    | Ir.Dest t -> Temp.Set.singleton t

let mk_buffer_info_out_of_temp t kernel_ctx buffer_infos =
  (* DON'T ASK *)
  match kernel_ctx.return_buffer_info with
  | Some buffer_info -> A_air.{
      typ = Temp.to_type t;
      dim = 1 + buffer_info.dim;
      length = List.map buffer_infos ~f:(fun bi -> List.hd_exn bi.length) @ buffer_info.length;
      filtered_lengths = begin
        List.map ~f:(fun bi -> List.hd_exn bi.filtered_lengths) buffer_infos
          @ buffer_info.filtered_lengths
      end;
      index = begin
        let (_, fn, _) = build_length_function (Temp.to_type t) in
        Many_fn.Fun (fun e -> Many_fn.Result (fn (Expr.Temp t) e))
      end;
    }
  | None -> A_air.{
      typ = Temp.to_type t;
      dim = List.length buffer_infos;
      length = List.map buffer_infos ~f:(fun bi -> List.hd_exn bi.length);
      filtered_lengths = List.map ~f:(fun bi -> List.hd_exn bi.filtered_lengths) buffer_infos;
      index = begin
        let (_, fn, _) = build_length_function (Temp.to_type t) in
        Many_fn.Fun (fun e -> Many_fn.Result (fn (Expr.Temp t) e))
      end;
    }

let update_backing_temps
  (backing_temps : Temp.Set.t Temp.Map.t)
  (t : Temp.t)
  (av : Air.array_view) : Temp.Set.t Temp.Map.t =
  let rec loop (_, av) = match av with
    | Air.Array_index (t1, _) -> Map.find backing_temps t1 |> Option.value ~default:Temp.Set.empty
    | Air.Transpose av -> loop av
    | Air.Array t -> Temp.Set.singleton t
    | Air.Reverse av -> loop av
    | Air.Zip_with (_, avs) -> Temp.Set.union_list (List.map ~f:loop avs)
    | Air.Tabulate (_, _, _) -> Temp.Set.empty
  in
  Map.add_exn backing_temps ~key:t ~data:(loop av)

let rec annotate_par_stmts (ctx : context) (stmts : Air.par_stmt list) : kernel_context * context =
  List.fold_left stmts ~init:(empty_kernel_ctx, ctx) ~f:(fun (_, acc) x -> annotate_par_stmt acc x)

and annotate_par_stmt (ctx : context) (stmt : Air.par_stmt) : kernel_context * context =
  match stmt with
  | Air.Parallel (dest, id, tavs, body) ->
      let (ctx, buffer_infos) = List.fold_map tavs ~init:ctx ~f:(fun ctx (key, _, av) ->
        let bi = annotate_array_view ctx av in
        let ctx =
          { ctx with
              result = begin
                let result = ctx.result in
                A_air.{ result with
                  buffer_infos = Map.add_exn result.buffer_infos ~key ~data:bi
                }
              end;
          }
        in (ctx, bi)
      ) in
      let (kernel_ctx, ctx) = annotate_seq_stmt ctx body in
      let kernel_ctx = { kernel_ctx with defined = List.fold_left tavs ~init:kernel_ctx.defined ~f:(fun acc (t1, t2, _) ->
        acc |> Fn.flip Set.add t1
            |> Fn.flip Set.add t2) }
      in
      (empty_kernel_ctx, { ctx with result = A_air.
          { ctx.result with
              kernel_infos = Map.add_exn ctx.result.kernel_infos ~key:id ~data:(kernel_ctx_to_kernel_info kernel_ctx);
              buffer_infos = ctx.result.buffer_infos |>
                (* only update buffer infos as necessary *)
                (match dest with
                 | Ir.Return _ -> Fn.id
                 | Ir.Dest t -> Map.add_exn ~key:t ~data:(mk_buffer_info_out_of_temp t kernel_ctx buffer_infos));
          };
      })
  | Air.Seq stmt -> annotate_seq_stmt ctx stmt
  | Air.Par_stmt par_stmt -> annotate_par_stmt_stmt ctx par_stmt

and empty_kernel_ctx : kernel_context =
  { used = Temp.Set.empty;
    defined = Temp.Set.empty;
    additional_buffers = Temp.Set.empty;
    return_buffer_info = None;
  }

and annotate_par_stmt_stmt (ctx : context) (par : Air.par_stmt Air.stmt) : kernel_context * context =
   annotate_stmt empty_kernel_ctx ctx
      (fun ?(kernel_ctx = empty_kernel_ctx) ctx x -> annotate_par_stmt ctx x)
      par

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
      let used = used_of_array_view ctx av in
      let buffer_info = annotate_array_view ctx av in
      ({ kernel_ctx with used; return_buffer_info = Some buffer_info },
       { ctx with result = A_air.
           { ctx.result with
               buffer_infos =
                 Map.add_exn ctx.result.buffer_infos ~key:t ~data:buffer_info;
           }
       })
  | Air.Run (Ir.Dest t, av) ->
      let used = used_of_array_view ctx av in
      let defined = Temp.Set.singleton t in
      let buffer_info = annotate_array_view ctx av in
      ({ used = Set.union used kernel_ctx.used;
         defined = Set.union defined kernel_ctx.defined;
         additional_buffers = Set.add kernel_ctx.additional_buffers t;
         return_buffer_info = None;
       },
       { ctx with result = A_air.
           { ctx.result with
               buffer_infos =
                 Map.add_exn ctx.result.buffer_infos ~key:t ~data:buffer_info;
           }
       })
  | Air.Nop -> (kernel_ctx, ctx)
  | Air.Filter_with (dest, (t1, av1), (t2, av2)) ->
      let defined = defined_of_dest dest in
      let buffer_info1 = annotate_array_view ctx av1 in
      let buffer_info2 = annotate_array_view ctx av2 in
      let my_buffer_info =
        let typ = A_air.(buffer_info1.typ) in
        let (_, index, _) = build_length_function typ in
        let temp = Temp.next Tc.Int () in (* placeholder type; won't reach CUDA *)
        A_air.{ buffer_info1 with
          index = Many_fn.Fun (fun e -> Many_fn.Result (index (Expr.Temp (Ir.temp_of_dest dest)) e));
          filtered_lengths = Some temp :: List.tl_exn buffer_info1.filtered_lengths;
        }
      in
      ({ used = kernel_ctx.used;
         defined = Set.union defined kernel_ctx.defined;
         additional_buffers =
           (match dest with
            | Ir.Return _ -> Fn.id
            | Ir.Dest t -> Fn.flip Set.add t)
           kernel_ctx.additional_buffers;
         return_buffer_info = begin
           match dest with
           | Ir.Return _ -> Some my_buffer_info
           | Ir.Dest _ -> None
        end;
       },
       { ctx with result = A_air.
           { ctx.result with
               buffer_infos =
                 Map.add_exn ctx.result.buffer_infos ~key:t1 ~data:buffer_info1
                   |> Map.add_exn ~key:t2 ~data:buffer_info2
                   |> match dest with
                      | Ir.Return _ -> Fn.id
                      | Ir.Dest t -> Map.add_exn ~key:t ~data:my_buffer_info;
           }
       })
  | Air.Scan (dest, _, operand, (t, av)) ->
      let defined = defined_of_dest dest in
      let used = used_of_operand ctx operand in
      let buffer_info_av = annotate_array_view ctx av in
      let my_buffer_info =
        let typ = Ir.type_of_dest dest in
        let (_, index, _) = build_length_function (Ir.type_of_dest dest) in
        A_air.{ buffer_info_av with typ; index = Many_fn.Fun (fun e -> Many_fn.Result (index (Expr.Temp (Ir.temp_of_dest dest)) e)); }
      in
      ({ defined = Set.union defined kernel_ctx.defined;
         used = Set.union used kernel_ctx.used;
         (* Conditionally add returned thing to addtl_buffers *)
         additional_buffers =
           (match dest with
            | Ir.Return _ -> Fn.id
            | Ir.Dest t -> Fn.flip Set.add t)
           kernel_ctx.additional_buffers;
         return_buffer_info = Some my_buffer_info;
       },
       { ctx with result = A_air.
           { ctx.result with
               buffer_infos =
                 Map.add_exn ctx.result.buffer_infos ~key:t ~data:buffer_info_av
                   |> match dest with
                      | Ir.Return _ -> Fn.id
                      | Ir.Dest t -> Map.add_exn ~key:t ~data:my_buffer_info;
           }
       })
  | Air.Reduce (dest, _, operand, (t, av)) ->
      let defined = defined_of_dest dest in
      let used = Set.union (used_of_operand ctx operand) (used_of_array_view ctx av) in
      let buffer_info = annotate_array_view ctx av in
      ({ defined = Set.union defined kernel_ctx.defined;
         used = Set.union used kernel_ctx.used;
         additional_buffers = kernel_ctx.additional_buffers;
         return_buffer_info = None;
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
              Map.add_exn ctx.result.buffer_infos ~key:t ~data:buffer_info;
            backing_temps =
              update_backing_temps ctx.result.backing_temps t av;
        };
      } in
      let (kernel_ctx', ctx) = recur ~kernel_ctx:kernel_ctx ctx body in
      let ctx' =
        match dest with
        | Ir.Return _ -> ctx
        | Ir.Dest t ->
            { ctx with result = A_air.
                { ctx.result with buffer_infos =
                  Map.add_exn ctx.result.buffer_infos ~key:t
                    ~data:(mk_buffer_info_out_of_temp t kernel_ctx' [buffer_info]) }; }
      in
      ({ defined = Set.union defined (Set.union kernel_ctx.defined kernel_ctx'.defined);
         used = Set.union (Set.union kernel_ctx.used kernel_ctx'.used) (used_of_array_view ctx av);
         additional_buffers = Set.union kernel_ctx.additional_buffers kernel_ctx'.additional_buffers;
         return_buffer_info = None;
       }, ctx')

and annotate_seq_stmt
 ?(kernel_ctx=empty_kernel_ctx)
  (ctx : context)
  (stmt : Air.seq_stmt) : kernel_context * context =
  let (kernel_ctx', ctx) = match stmt with
    | Air.Binop (dest, _, src1, src2) | Air.Index (dest,src1,src2) ->
        let used = Set.union (used_of_operand ctx src1) (used_of_operand ctx src2) in
        let defined = defined_of_dest dest in
        ({ used; defined; additional_buffers = Temp.Set.empty;
           return_buffer_info = None;
         }, ctx)
    | Air.Assign (dest, src) ->
        let used = used_of_operand ctx src in
        let defined = defined_of_dest dest in
        ({ used; defined;
           additional_buffers = Temp.Set.empty;
           return_buffer_info = (match src with
             | Air.Temp t -> Map.find ctx.result.A_air.buffer_infos t
             | _ -> None);
         }, ctx)
    | Air.Unop (dest, _, src) | Air.Access (dest, src, (_ : Ast.ident)) ->
        let used = used_of_operand ctx src in
        let defined = defined_of_dest dest in
        ({ used; defined; additional_buffers = Temp.Set.empty; return_buffer_info = None; }, ctx)
    | Air.Primitive (dest,src) ->
        let used = used_of_primitive ctx src in
        let defined = defined_of_dest dest in
        ({ used; defined; additional_buffers = Temp.Set.empty; return_buffer_info = None; }, ctx)
    | Air.Struct_Init (dest,_,flx) ->
       let used = used_of_struct_fields ctx flx in
       let defined = defined_of_dest dest in
       ({ used; defined; additional_buffers = Temp.Set.empty; return_buffer_info = None; }, ctx)
    | Air.Seq_stmt seq_stmt -> annotate_seq_stmt_stmt kernel_ctx ctx seq_stmt
  in

  (* Update kernel ctx based on use/def *)
  let kernel_ctx =
    { used = Set.union kernel_ctx.used kernel_ctx'.used;
      defined = Set.union kernel_ctx.defined kernel_ctx'.defined;
      additional_buffers = Set.union kernel_ctx.additional_buffers kernel_ctx'.additional_buffers;
      return_buffer_info = kernel_ctx'.return_buffer_info;
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
        index = Many_fn.Fun (fun e -> Many_fn.Result (index (Expr.Temp p) e));
        filtered_lengths = List.init dim ~f:(fun _ -> None);
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
          backing_temps = Temp.Map.empty;
        }

      (* No need to have a separate parameter at all if the function returns directly. *)
      | _ -> A_air.{
        params;
        backing_temps = Temp.Map.empty;
        buffer_infos;
        kernel_infos = Id.Map.empty;
        out_param = None;
      }
    in
    let ctx = { result; aliases = Temp.Map.empty; } in
    let ctx' = annotate_par_stmt ctx Air.(air.body) in
    (snd ctx').result
