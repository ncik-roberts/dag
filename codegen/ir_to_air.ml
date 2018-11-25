open Core

module Dest = Ir.Dest.T

type context = {
  (* Keep track of which temps (with at most one successor) correspond
   * to array views.
   *)
  array_views : Air.array_view Temp.Map.t;
}

let nop : Air.par_stmt = Air.Par_stmt Air.Nop

let convert_operand : Ir.operand -> Air.operand =
  function
    | Ir.Const i -> Air.Const i
    | Ir.Temp t -> Air.Temp t

(* Check that a temp isn't in the context.
 * Used for checking invariants.
 *)
let not_in (ctx : context) (op : Ir.operand) : bool =
  match op with
  | Ir.Const _ -> true
  | Ir.Temp t -> Fn.non (Map.mem ctx.array_views) t

let make_array_view
  (fun_call : Ir.fun_call)
  (srcs : [ `Array_view of Air.array_view | `Operand of Air.operand ] list) : Air.array_view =
    match fun_call, srcs with
    | Ir.Zip_with f, [ `Array_view view1; `Array_view view2; ] -> Air.Zip_with (f, [ view1; view2; ])
    | Ir.Transpose, [ `Array_view view ] -> Air.Transpose view
    | Ir.Map f, [ `Array_view view ] -> Air.Zip_with (f, [ view ])
    | Ir.Reduce _, _ -> failwith "Reduce is not an array view."
    | Ir.Dim _, _ -> failwith "Dim is not an array view."
    | Ir.Zip_with _, _ -> failwith "Invalid zipwith."
    | Ir.Transpose, _ -> failwith "Invalid transpose."
    | Ir.Map _, _ -> failwith "Invalid map."

let canonicalize (ctx : context) : Ir.operand -> 'a = function
  | Ir.Const i -> `Operand (Air.Const i)
  | Ir.Temp t ->
      let array_view_opt = Map.find ctx.array_views t in
      Option.value_map array_view_opt
        ~default:(`Array_view (Air.Array t))
        ~f:(fun array_view -> `Array_view array_view) (* Eta-expansion necessary *)

let rec to_seq_stmt : Air.par_stmt -> Air.seq_stmt =
  function
    | Air.Parallel (_, [], _) -> failwith "Empty parallel loop."
    | Air.Parallel (dst, [(t, av)], stmt) -> Air.Seq_stmt (Air.For (dst, (t, av), stmt))
    | Air.Parallel (dst, (t, av) :: tavs, stmt) ->
        Air.Seq_stmt (Air.For (dst, (t, av), to_seq_stmt (Air.Parallel (Dest.Return, tavs, stmt))))
    | Air.Par_stmt stmt -> Air.Seq_stmt (to_seq_stmt' stmt)
    | Air.Seq stmt -> stmt

and to_seq_stmt' : Air.par_stmt Air.stmt -> Air.seq_stmt Air.stmt =
  function
    | Air.Reduce (a, b, c, d) -> Air.Reduce (a, b, c, d)
    | Air.Block xs -> Air.Block (List.map ~f:to_seq_stmt xs)
    | Air.For (a, b, c) -> Air.For (a, b, to_seq_stmt c)
    | Air.Run (a, b) -> Air.Run (a, b)
    | Air.Nop -> Air.Nop

(* A program contains many parallel statements. Let's change some of
 * them to sequential statements. *)
let rec expand (air : Air.par_stmt) : Air.par_stmt list = match air with
  | Air.Seq _ | Air.Parallel _ | Air.Par_stmt Air.Nop  -> List.return air
  | Air.Par_stmt (Air.Reduce _ | Air.Run _ as r) -> Air.[
      Par_stmt r;
      Seq (Seq_stmt (to_seq_stmt' r));
    ]
  | Air.Par_stmt (Air.For (dst, (t, av), stmt)) ->
      List.map (expand stmt) ~f:Air.(fun stmt' -> Par_stmt (For (dst, (t, av), stmt')))
  | Air.Par_stmt (Air.Block stmts) ->
      let rec loop acc = function
        | [] ->
            assert (List.is_empty acc);
            List.return []
        | [stmt] -> List.map (expand stmt) ~f:(fun stmt' -> List.rev (stmt' :: acc))
        | stmt :: stmts -> List.concat_map (expand stmt) ~f:(fun stmt' -> loop (stmt' :: acc) stmts)
      in List.map (loop [] stmts) ~f:Air.(fun stmts -> Par_stmt (Block stmts))

let rec make_parallel
  (d1 : Ir.dest) (* dest *)
  (av1 : Air.array_view) (* array view *)
  (t1 : Temp.t) (* temp to which each elem of array view is bound *)
  (stmts : Air.par_stmt list) : Air.par_stmt list =
  List.concat_map stmts ~f:(fun stmt -> match stmt with
    | Air.Parallel (d2, avs, body) -> Air.[
        (* One option: fold together parallel blocks. *)
        Parallel (d1, (t1, av1) :: avs, body);

        (* Another option: unparallelize outer loop. *)
        Par_stmt (For (d1, (t1, av1), stmt));
      ]
    | Air.Seq seq_stmt -> Air.[
        Parallel (d1, [(t1, av1)], seq_stmt);
        Par_stmt (For (d1, (t1, av1), stmt));
      ]
    | Air.Par_stmt (Air.Run (d2, av2)) -> Air.[
        begin
          let t2 = Temp.next () in
          Parallel (d1, [(t1, av1); (t2, av2);], Assign (d2, Temp t2))
        end;
        Parallel (d1, [(t1, av1)], Seq_stmt (Run (d2, av2)));
        Par_stmt (For (d1, (t1, av1), stmt));
      ]
    | Air.Par_stmt par_stmt -> Air.[
        Parallel (d1, [(t1, av1)], Seq_stmt (to_seq_stmt' par_stmt));
        Par_stmt (For (d1, (t1, av1), stmt));
      ]
  )

let all (ir : Ir.t) (dag : Temp_dag.dag) : Air.t list =

  (* Returned list is the various ways of parallelizing this statement. *)
  let rec loop (ctx : context) (stmt : Ir.stmt) : (context * Air.par_stmt) list = match stmt with
    | Ir.Nop -> [(ctx, nop)]
    | Ir.Assign (dest, src) ->
        let result_opt = match (dest, src) with
          | (Dest.Dest dest, Ir.Temp src) ->
              let array_view_opt = Map.find ctx.array_views src in
              Option.map array_view_opt ~f:(fun array_view ->
                let ctx' = { array_views = Map.add_exn ctx.array_views ~key:dest ~data:array_view } in
                [(ctx', nop)])
          | (Dest.Return, Ir.Temp src) ->
              let array_view_opt = Map.find ctx.array_views src in
              Option.map array_view_opt ~f:(fun array_view ->
                [(ctx, Air.Par_stmt (Air.Run (Dest.Return, array_view)))])
          | _ -> None
        in
        Option.value result_opt
          ~default:Air.([(ctx, Seq (Assign (dest, convert_operand src)))])
  | Ir.Binop (dest, binop, src1, src2) ->
      (* Right now, no binops that operate on array views. *)
      assert (not_in ctx src1);
      assert (not_in ctx src2);
      Air.[(ctx, Seq (Binop (dest, binop, convert_operand src1, convert_operand src2)))]
  | Ir.Unop (dest, unop, src) ->
      assert (not_in ctx src);
      Air.[(ctx, Seq (Unop (dest, unop, convert_operand src)))]
  | Ir.Fun_call (dest, Ir.Reduce op, srcs) ->
    begin
      match List.map ~f:(canonicalize ctx) srcs with
      | [ `Operand o; `Array_view v; ] ->
          [(ctx, Air.(Par_stmt (Reduce (dest, op, o, v))))]
      | _ -> failwith "Invalid reduce."
    end
  | Ir.Fun_call (dest, Ir.Dim n, srcs) ->
    begin
      match List.map ~f:(canonicalize ctx) srcs with
      | [ `Array_view v; ] ->
          [(ctx, Air.(Seq (Assign (dest, Dim (n, v)))))]
      | _ -> failwith "Invalid reduce."
    end
  | Ir.Fun_call (dest, fun_call, srcs) ->
      (* Canonical srcs (i.e. if it is an array view) *)
      let csrcs = List.map srcs ~f:(canonicalize ctx) in

      List.concat [
        (* Option 1: defer execution of array view. *)
        begin
          match dest with
          | Dest.Return -> []
          | Dest.Dest dest ->
              let array_view = make_array_view fun_call csrcs in
              let ctx' = { array_views = Map.add_exn ctx.array_views ~key:dest ~data:array_view } in
              List.return (ctx', nop)
        end;

        (* Option 2: run array view right away. *)
        begin
          let array_view = make_array_view fun_call csrcs in
          List.return (ctx, Air.(Par_stmt (Run (dest, array_view))))
        end;
      ]
  | Ir.Parallel (dest, src, t_src, stmts) ->
      let array_view = match canonicalize ctx src with
        | `Array_view view -> view
        | `Operand _ -> failwith "Invalid argument to a parallel block."
      in
      let stmts' =
        let ctx' = { array_views = Map.add_exn ctx.array_views ~key:t_src ~data:(Air.Array t_src) } in
        loop_stmts ctx' stmts
      in
      let alternatives = make_parallel dest array_view t_src stmts' in
      List.map ~f:(Tuple2.create ctx) alternatives

  (* List is all possible options *)
  and loop_stmts (ctx : context) (stmts : Ir.stmt list) : Air.par_stmt list =
    let stmts = List.filter stmts ~f:(function Ir.Nop -> false | _ -> true) in
    match stmts with
    | [stmt] -> List.map ~f:snd (loop ctx stmt)
    | _ ->
        let rec f (ctx : context) (stmts : Ir.stmt list) (acc : Air.par_stmt list) : Air.par_stmt list =
          match stmts with
          | [] -> [Air.(Par_stmt (Block (List.rev acc)))]
          | s :: ss ->
              let ctx_stmt_pairs = loop ctx s in
              List.concat_map ctx_stmt_pairs ~f:(fun (ctx, stmt) -> f ctx ss (stmt :: acc))
        in
        f ctx stmts []
  in

  let init_ctx = { array_views =
    Temp.Map.of_alist_exn (List.map Ir.(ir.params) ~f:(fun t -> (t, Air.Array t)))
  } in

  let alternatives = loop_stmts init_ctx Ir.(ir.body) in

  (* Take all possible combinations of sequential/parallel statements *)
  let alternatives = List.concat_map ~f:expand alternatives in

  List.map alternatives ~f:(fun alt ->
    Air.{ params = Ir.(ir.params);
          body = alt;
        })

(* This is... awful. Generate all options and then take the head. *)
let any (ir : Ir.t) (dag : Temp_dag.dag) : Air.t = List.hd_exn (all ir dag)
