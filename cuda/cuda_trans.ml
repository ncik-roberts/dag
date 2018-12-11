open Core

module Ano = Annotated_air
module Length_expr = Annotated_air.Length_expr
module Expr = Annotated_air.Expr
module Op = Ir.Operator
module CU = Cuda_ir
module Many_fn = Utils.Many_fn

let rec fix f x = f (fix f) x
let dedup line = function
  | `Ok -> ()
  | `Duplicate -> failwithf "(%d): dup :(" line ()

let failwith n = failwith ("AIR -> CUDA : " ^ n)

type context = {
  result : Ano.result;

  lvalue : Expr.t option; (* this is an lvalue and its lengths *)
  lvalue_lengths : Length_expr.t list option;

  out_param : Temp.t option;

  (* Temps indicating the dimensions of parameters. *)
  dims_of_params : Temp.t list;

  backed_temps : Temp.Hash_set.t;

  (* Map from temps to kind of allocation necessary *)
  (* If it's not in here, it's just on the host. *)
  allocation_method :
    [ `Just_device (* additional_buffers *)
    | `Host_and_device of Temp.t (* temp is the device temp *)
    | `Host_and_device_no_malloc_on_host of Temp.t (* temp is the device temp *)
    | `Unallocated
    ] Temp.Table.t;
}

(* Unique identifier for functions. s*)
let fn_next : unit -> string =
  let fn_name_ctr = ref 0 in
  fun () ->
    incr fn_name_ctr;
    sprintf "fn_%d" !fn_name_ctr

(* Translate a temp into a variable.*)
let temp_name (t : Temp.t) : string =
  sprintf "t_%d" (Temp.to_int t)

(* Put that variable into a CUDA one. *)
let temp_to_var (t : Temp.t) : CU.cuda_expr = CU.Var (temp_name t)

let next_var () : CU.cuda_expr = temp_to_var (Temp.next Tc.Int ())

(* These flatten out any nesting. We're presuming the nd_array struct takes care of it. *)
let rec typ_name (typ : Tc.typ) : string =
  match typ with
  | Tc.Array t -> typ_name t ^ "[]"
  | Tc.Int -> "int"
  | _ -> failwith "Not supported."

let rec trans_type (typ : Tc.typ) : CU.cuda_type =
  match typ with
  | Tc.Int -> CU.Integer
  | Tc.Float -> CU.Float
  | Tc.Struct i -> CU.Struct i
  | Tc.Array t -> CU.Pointer (remove_arrays t)
  | Tc.Pointer t -> CU.Pointer (trans_type t)
  | Tc.Bool -> CU.Boolean
  | _ -> failwith "Don't currently support other types"

(* Remove outermost arrays. *)
and remove_arrays =
  function Tc.Array typ -> remove_arrays typ | typ -> trans_type typ


(* Translate an AIR parameter list into a list of CUDA parameters.
 *
 * Notice that `temp_name t` allows us to uniquely determine the name of a temp at
 * any point. Therefore, we don't have to maintain a separate context mapping temps
 * to cuda variable names. What a relief!
 *
 * Recall, graciously, that the parameter for the output buffer has already been
 * added to the parameter list in the context, so it too is translated by the
 * concat_map.
 *)
let trans_params (params : Ano.Param.t list) : (CU.cuda_type * CU.cuda_ident) list =
   List.concat_map params ~f:(fun t ->
     match t with
     (* An array parameter is accompanied by `n` dimension parameters, where n is the
      * dimensionality of the array. *)
     | Ano.Param.Array (t, dims) ->
         let array_param = (trans_type (Temp.to_type t), temp_name t) in
         let dim_params = List.map dims ~f:(fun t -> (CU.Integer, temp_name t)) in
         array_param :: dim_params


     (* A non-array parameter, sadly, is not. *)
     | Ano.Param.Not_array t -> List.return (trans_type (Temp.to_type t), temp_name t))

let rec lengths ctx (_, av) = match av with
  | Air.Array t -> get_lengths ctx t
  | Air.Array_index (t, _) -> List.drop (get_lengths ctx t) 1
  | Air.Zip_with (_, []) -> failwith "lol"
  | Air.Zip_with (_, av1 :: _) -> lengths ctx av1
  | Air.Reverse av -> lengths ctx av
  | Air.Tabulate (b, e, s) ->
      [CU.Binop (CU.DIV, CU.Binop (CU.SUB, temp_to_var e, temp_to_var b), temp_to_var s)]
  | Air.Transpose av -> match lengths ctx av with
      | x1 :: x2 :: xs -> x2 :: x1 :: xs
      | _ -> failwith "That can't be right."

(* Translate AIR operands into their cuda equivalents. s*)
and trans_op ctx = function
  | Air.IndexOp (t1, t2) -> begin
      match Map.find Ano.(ctx.result.buffer_infos) t1 with
      | None -> Many_fn.Result (trans_expr ctx (Expr.Index (Expr.Temp t1, Expr.Temp t2)))
      | Some bi -> begin
          match Ano.(bi.index) with
          | Many_fn.Result t -> Many_fn.Result (trans_expr ctx (Expr.Index (t, Expr.Temp t2)))
          | Many_fn.Fun f -> Many_fn.compose (trans_expr ctx) (f (Expr.Temp t2))
        end
    end
  | Air.Const c -> Many_fn.Result (CU.IConst (Int64.of_int32_exn c))
  | Air.Float f -> Many_fn.Result (CU.FConst f)
  | Air.Bool b -> Many_fn.Result (CU.BConst b)
  | Air.Temp t -> Many_fn.Result (CU.Var (temp_name t))
  | Air.Dim (n, view) -> Many_fn.Result (List.nth_exn (lengths ctx view) n)

and trans_op_exn ctx = Fn.compose (Many_fn.result_exn ~msg:":(") (trans_op ctx)

and trans_prim ctx m = match m with
  | Air.Max (a, b)
  | Air.Min (a, b) -> begin
      let name = match m with Air.Max _ -> "max" | _ -> "min" in
      let operands = List.map ~f:(trans_op_exn ctx) [a; b;] in
      match Air.type_of_operand a with
      | Tc.Int -> CU.FnCall (sprintf "_dag_i%s" name, operands)
      | Tc.Float -> CU.FnCall (sprintf "_dag_f%s" name, operands)
      | _ -> failwith "I don't know how to take a max."
    end
  | Air.Log2 a -> begin
      match Air.type_of_operand a with
      | Tc.Int -> CU.FnCall ("_dag_ilog2", [trans_op_exn ctx a])
      | Tc.Float -> CU.FnCall ("log2f", [trans_op_exn ctx a])
      | _ -> failwith "I don't know what a log 2 is."
    end
  | Air.I2F a -> CU.Cast(CU.Float, trans_op_exn ctx a)
  | Air.F2I a -> CU.Cast(CU.Integer, trans_op_exn ctx a)

and trans_binop = function
  | Ast.Plus -> CU.ADD
  | Ast.Minus -> CU.SUB
  | Ast.Times -> CU.MUL
  | Ast.Div -> CU.DIV
  | Ast.Mod -> CU.MOD
  | Ast.Lshift -> CU.SHL
  | Ast.Rshift -> CU.SHR
  | Ast.And -> CU.AND
  | Ast.Or -> CU.OR
  | Ast.BitAnd -> CU.BITAND
  | Ast.BitOr -> CU.BITOR
  | Ast.BitXor -> CU.BITXOR
  | Ast.Less -> CU.LT
  | Ast.LessEq -> CU.LTE
  | Ast.Greater -> CU.GT
  | Ast.GreaterEq -> CU.GTE

and trans_unop = function
  | Ast.Negate -> CU.NEG
  | Ast.Logical_not -> CU.NOT

(* Creates the loop header (initial value, max value, and increment stride)
 * -> for (var = init;var < limit; var += stride) *)
and trans_incr_loop_hdr ~loop_var ~lo ~hi ~stride =
 (CU.DeclareAssign (CU.Integer, loop_var, lo),
   CU.Cmpop (CU.LT, CU.Var loop_var, hi), CU.AssignOp (CU.ADD, CU.Var loop_var, stride))

(* Actually perform the reduction! *)
and make_reduce op args =
  match (op, args) with
    | Op.Binop bin, [a;b] -> CU.AssignOp (trans_binop bin, a, b)
    | Op.Binop _, _ -> failwith "Binary operators have two arguments."
    | Op.Unop _, _ -> failwith "Cannot reduce with a unary operator."

and trans_len_expr : Length_expr.t -> CU.cuda_expr = function
  | Length_expr.Temp t -> temp_to_var t
  | Length_expr.Mult (e1, e2) -> CU.Binop (CU.MUL, trans_len_expr e1, trans_len_expr e2)
  | Length_expr.Minus (e1, e2) -> CU.Binop (CU.SUB, trans_len_expr e1, trans_len_expr e2)
  | Length_expr.Div (e1, e2) -> CU.Binop (CU.DIV, trans_len_expr e1, trans_len_expr e2)

and build_index_expr lens init =
  List.fold_left lens ~init ~f:(fun acc x -> CU.Binop (CU.MUL, acc, x))

(* Find a buffer info's length. *)
and get_lengths (ctx : context) (t : Temp.t) : CU.cuda_expr list =
  match Map.find Ano.(ctx.result.buffer_infos) t with
  | Some inf -> List.map ~f:trans_len_expr Ano.(inf.length)
  | None -> List.map ~f:trans_len_expr (Option.value_exn ctx.lvalue_lengths)

and trans_index_expr
  (ctx : context)
  (arr : Expr.t)
  (i : Expr.t) : CU.cuda_expr * CU.cuda_expr * CU.cuda_expr list =
  match arr with
  | Expr.Index (arr', j) ->
    begin
      match trans_index_expr ctx arr' j with
      | (result_arr, index_expr, _ :: lens) ->
          let index_expr' = CU.Binop (CU.ADD, index_expr, build_index_expr lens (trans_expr ctx i)) in
          (result_arr, index_expr', lens)
      | _ -> failwith "No more lengths :("
    end
  | Expr.Temp t ->
      let lengths = List.tl_exn (get_lengths ctx t) in
      (temp_to_var t, build_index_expr lengths (trans_expr ctx i), lengths)
  | _ -> failwith "No."

and trans_expr : context -> Expr.t -> CU.cuda_expr = fun ctx -> function
  | Expr.Temp t -> temp_to_var t
  | Expr.Const i -> CU.IConst (Int64.of_int32 i)
  | Expr.Index (arr, i) -> begin
      match trans_index_expr ctx arr i with
      | (arr, i, []) -> CU.Index (arr, i)
      | (arr, i, _) -> CU.Address (CU.Index (arr, i))
    end
  | Expr.Call (op, xs) as e ->
  let open Ir.Operator in
  match op, xs with
  | Binop b, [ e1; e2; ] -> CU.Binop (trans_binop b, trans_expr ctx e1, trans_expr ctx e2)
  | Unop u, [ e1; ] -> CU.Unop (trans_unop u, trans_expr ctx e1)
  | _ -> failwithf "Invalid: %s." (Sexp.to_string_hum (Expr.sexp_of_t e)) ()

let dest_to_expr_lvalue (ctx : context) (dest : Ir.dest) : Expr.t =
  match dest with
  | Ir.Return _ -> Option.value_exn ctx.lvalue
  | Ir.Dest t -> Expr.Temp t

let dest_to_lvalue (ctx : context) (dest : Ir.dest) : CU.cuda_expr option =
  match dest with
  | Ir.Return _ -> Option.map ~f:(trans_expr ctx) ctx.lvalue
  | Ir.Dest t -> Some (temp_to_var t)

let dest_to_lvalue_exn ctx dest = Option.value_exn (dest_to_lvalue ctx dest)

(* on_return is the lvalue to use upon encountering a return stmt. *)
let dest_to_stmt (ctx : context) (dest : Ir.dest) (rhs : CU.cuda_expr) : CU.cuda_stmt =
  match dest, ctx.lvalue with
  | Ir.Return _, None -> CU.Return rhs
  | Ir.Return _, Some lhs -> CU.Assign (trans_expr ctx lhs, rhs)
  | Ir.Dest t, _ -> CU.DeclareAssign (trans_type (Temp.to_type t), temp_name t, rhs)

let app index t = Many_fn.app index t ~default:(fun arr -> Expr.Index (arr, t))
let rec app_expr ctx f e = Many_fn.compose (trans_expr ctx) (app f e)

let get_length ctx t = List.hd_exn (get_lengths ctx t)

let get_index (ctx : context) (t : Temp.t) : Temp.t -> (Expr.t, CU.cuda_expr) Many_fn.t =
  match Map.find Ano.(ctx.result.buffer_infos) t with
  | Some inf ->
      (* Lookup function *)
      fun t -> app_expr ctx Ano.(inf.index) (Expr.Temp t)
  | None -> failwithf "Couldn't find buffer index for `%d`" (Temp.to_int t) ()

(* Create a loop that iterates over the buffer t. *)
let create_loop ~counter:loop_temp (hi : CU.cuda_expr) =
  trans_incr_loop_hdr
    ~loop_var:(temp_name loop_temp)
    ~lo:(CU.IConst 0L)
    ~hi
    ~stride:(CU.IConst 1L)

(* Used to perform operations with length expressions. *)
let multiply_cuda_exprs (exprs : CU.cuda_expr list) =
  let rec reduce_expr exprs =
    match exprs with
    | [] -> failwith "There is absolutely no way that this is what you wanted to do."
    | [expr] -> expr
    | ex :: xs -> (CU.Binop (CU.MUL, ex, reduce_expr xs))
  in
  reduce_expr exprs

(* Translate a statement irrespective of parallel/sequential semantics *)
let rec trans_a_stmt : type a. (context -> a -> CU.cuda_stmt list) -> context -> a Air.stmt -> CU.cuda_stmt list =
  fun continuation ctx -> function
    | Air.For (dest, (bound_temp, t_idx, view), stmt) ->
        (* Get the destination buffer which the for loop is being assigned into. *)
        let dest_array = dest_to_expr_lvalue ctx dest in

        (* Iterate over the bound temp (which Annotate stores as having all the
         * information of the view array). *)
        let hdr = create_loop ~counter:t_idx (get_length ctx bound_temp) in
        Temp.Table.add ctx.allocation_method ~key:bound_temp ~data:`Unallocated
          |> dedup __LINE__;

        let body =
          (* The lvalue for the for body is an index into the destination array. *)
          (* Translate the body under the new lvalue. *)
          let ctx' = { ctx with lvalue = Some (Expr.Index (dest_array, Expr.Temp t_idx)) } in
          continuation ctx' stmt in

       let to_memcpy =
         let open Option.Monad_infix in
         if not (Hash_set.mem ctx.backed_temps bound_temp) then []
         else
           let backing_temps = Map.find_exn ctx.result.Ano.backing_temps bound_temp
             |> Set.to_list
           in
           List.map backing_temps ~f:(fun src ->
             match Temp.Table.find ctx.allocation_method src with
             | Some `Host_and_device dest -> (src, dest)
             | _ -> failwith "No way.")
       in

       let hd = List.map to_memcpy ~f:(fun (src, dest) ->
         let typ = Temp.to_type src in
         let tfr = CU.(Host, Device) in
         let lengths =
           try get_lengths ctx dest
           with _ -> get_lengths ctx src
         in
         let len = CU.Binop (CU.MUL, multiply_cuda_exprs lengths, CU.Size_of (remove_arrays typ)) in
         CU.Transfer (temp_to_var dest, temp_to_var src, len, tfr))
       in


       hd @ [ CU.Loop (hdr, body); ]

    | Air.Block s -> List.concat_map ~f:(continuation ctx) s
    | Air.Nop -> []
    | Air.Reduce _ | Air.Run _ | Air.Scan _ | Air.Filter_with _ ->
        failwith
          "Unable to perform reduce/run without specifically associated parallel or sequential semantics."

(* Translate a sequential statement *)
and trans_seq_stmt (ctx : context) (stmt : Air.seq_stmt) : CU.cuda_stmt list =
  let (<--) d src = match dest_to_lvalue ctx d with
    | Some (CU.Var dest) ->
        CU.DeclareAssign (trans_type (Ir.type_of_dest d), dest, src)
    | Some dest -> CU.Assign (dest, src)
    | None -> CU.Return src
  in

  match stmt with
  | Air.Binop (d, op, s1, s2) ->
      [ d <-- CU.Binop (trans_binop op, trans_op_exn ctx s1, trans_op_exn ctx s2) ]
  | Air.Index (d, src, i) -> [ d <-- CU.Index (trans_op_exn ctx src, trans_op_exn ctx i) ]
  | Air.Access (d,src,f) ->
      [ d <-- CU.Field (trans_op_exn ctx src, f)]
  | Air.Unop (d, op, s) ->
      [ d <-- CU.Unop (trans_unop op, trans_op_exn ctx s) ]
  | Air.Assign (d, s) ->
      [ d <-- trans_op_exn ctx s ]
  | Air.Primitive (d, s) ->
      [ d <-- trans_prim ctx s]
  | Air.Struct_Init (d, t, flx) ->
      let (dest, hd, tl) = match (dest_to_lvalue ctx d, d) with
        | Some lvalue, Ir.Return _ -> (lvalue, [], [])
        | _, Ir.Dest t ->
            let typ = Ir.type_of_dest d in
            (temp_to_var t, [ CU.Declare (trans_type typ, temp_name t) ], [])
        | None, Ir.Return _ ->
            let typ = Ir.type_of_dest d in
            let t = Temp.next typ () in
            (temp_to_var t, [ CU.Declare (trans_type typ, temp_name t) ], [ CU.Return (temp_to_var t) ])
      in
      hd @ [ CU.InitStruct (dest, List.map flx ~f:(fun (n,o) -> (n,trans_op_exn ctx o)))] @ tl
  | Air.Seq_stmt seq_stmt ->

  (* Run/reduce given sequential semantics. *)
  match seq_stmt with
  (* Don't need to bind view: we already figured out how to index into
   * it in the annotation phase. Based on the value of view, the
   * call to get_index will return the right indexing function. *)
  | Air.Run (dest, _) ->
      (match dest with
       | Ir.Return t ->
           Temp.Table.add ctx.allocation_method ~key:t ~data:`Unallocated |> dedup __LINE__
       | _ -> ());
      let dest_temp = Ir.temp_of_dest dest in
      let index_fn = get_index ctx dest_temp in
      let loop_temp = Temp.next Tc.Int () in
      let lengths = get_lengths ctx dest_temp in
      let hdr = create_loop ~counter:loop_temp (List.hd_exn lengths) in
      let body = trans_array_view ctx (dest, loop_temp, index_fn, List.tl_exn lengths) in
      [ CU.Loop (hdr, [ body ]) ]

  (* For reduce, we associate the buffer_info for the view with t. *)
  | Air.Reduce (dest, op, init, (t, _)) ->
      let lvalue = Temp.next (Ir.type_of_dest dest) () in
      let index_fn = get_index ctx t in
      let loop_temp = Temp.next Tc.Int () in
      let hdr = create_loop ~counter:loop_temp (get_length ctx t) in
      Temp.Table.add ctx.allocation_method ~key:t ~data:`Unallocated
        |> dedup __LINE__;
      let assign_stmt = make_reduce op
        [ temp_to_var lvalue;
          Many_fn.result_exn ~msg:"Reduce result_exn." (index_fn loop_temp);
        ]
      in
      [ CU.DeclareAssign (trans_type (Temp.to_type lvalue), temp_name lvalue, trans_op_exn ctx init);
        CU.Loop (hdr, [assign_stmt]);
        dest_to_stmt ctx dest (temp_to_var lvalue);
      ]

  | Air.Filter_with (dest, (t1, _), (t2, _)) ->
      List.iter [t1; t2;] ~f:(fun key ->
        Temp.Table.add ctx.allocation_method ~key ~data:`Unallocated
          |> dedup __LINE__);
      let index1_fn = get_index ctx t1 in
      let index2_fn = get_index ctx t2 in
      let loop_temp = Temp.next Tc.Int () in
      let acc = Temp.next Tc.Int () in
      let hdr = create_loop ~counter:loop_temp (get_length ctx t1) in
      let check_stmt =
        CU.Condition (Many_fn.result_exn ~msg:"cuda_trans1" (index2_fn loop_temp),
          [ CU.Assign (trans_expr ctx (Expr.Index (dest_to_expr_lvalue ctx dest, Expr.Temp acc)),
              Many_fn.result_exn ~msg:"cuda_trans2" (index1_fn loop_temp));
            CU.AssignOp (CU.ADD, temp_to_var acc, CU.IConst 1L);
          ],
          [ (* nothing to do in else case *) ])
      in
      [ CU.DeclareAssign (CU.Integer, temp_name acc, CU.IConst 0L);
        CU.Loop (hdr, [ check_stmt ]);
      ]

  | Air.Scan (dest, op, init, (t, _)) ->
      let ty = match Ir.type_of_dest dest with
        | Tc.Array ty -> ty
        | _ -> failwith "Not right."
      in
      let acc = Temp.next ty () in
      let index_fn = get_index ctx t in
      let loop_temp = Temp.next Tc.Int () in
      let hdr = create_loop ~counter:loop_temp (get_length ctx t) in
      Temp.Table.add ctx.allocation_method ~key:t ~data:`Unallocated
        |> dedup __LINE__;
      let assign_stmt = make_reduce op
        [ temp_to_var acc;
          Many_fn.result_exn ~msg:"Reduce result_exn." (index_fn loop_temp);
        ]
      in
      [ CU.DeclareAssign (trans_type ty, temp_name acc, trans_op_exn ctx init);
        CU.Loop (hdr, [
          CU.Assign (CU.Index (dest_to_lvalue_exn ctx dest, temp_to_var loop_temp), temp_to_var acc);
          assign_stmt;
        ]);
        (* TODO: allow client to bind final reduced result. *)
        (* dest_to_stmt ctx dest (temp_to_var acc); *)
      ]

  | _ -> trans_seq_stmt_stmt ctx seq_stmt

and trans_par_stmt (ctx : context) (stmt : Air.par_stmt) : CU.cuda_stmt list =
  match stmt with
  | Air.Seq seq_stm -> trans_seq_stmt ctx seq_stm

  (* TODO: Make these actually run in parallel. *)
  | Air.Par_stmt (Air.Run (a, b)) -> trans_seq_stmt ctx (Air.Seq_stmt (Air.Run (a, b)))
  | Air.Par_stmt (Air.Reduce (a, b, c, d)) -> trans_seq_stmt ctx (Air.Seq_stmt (Air.Reduce (a, b, c, d)))
  | Air.Par_stmt (Air.Filter_with (a, b, c)) -> trans_seq_stmt ctx (Air.Seq_stmt (Air.Filter_with (a, b, c)))
  | Air.Par_stmt (Air.Scan (a, b, c, d)) -> trans_seq_stmt ctx (Air.Seq_stmt (Air.Scan (a, b, c, d)))
  | Air.Par_stmt par_stmt -> trans_par_stmt_stmt ctx par_stmt

  | Air.Parallel (dest, id, bound_array_views, body) ->
      let kernel_info =
        match Map.find Ano.(ctx.result.kernel_infos) id with
        | Some k_inf -> k_inf
        | None -> failwith "Failed to find kernel info. (trans_stmt_par)"
      in

      List.iter bound_array_views ~f:(fun (_, _, av) ->
        fix
          (fun loop (_, av) -> match av with
            | Air.Array t -> ()
            | Air.Zip_with (_, avs) -> List.iter ~f:loop avs
            | Air.Reverse av -> loop av
            | Air.Transpose av -> loop av
            | Air.Tabulate _ -> ()
            | Air.Array_index (t, _) ->
                if Map.mem ctx.result.Ano.backing_temps t
                then Hash_set.add ctx.backed_temps t)
          av);

      (* Get the temps of the array views to pass to the kernel launch. *)
      let rec temps_in_array_view (av : Air.array_view) : Temp.Set.t =
        match snd av with
        | Air.Zip_with (_, avs) -> Temp.Set.union_list (List.map avs ~f:temps_in_array_view)
        | Air.Reverse av -> temps_in_array_view av
        | Air.Array t -> Temp.Set.singleton t
        | Air.Transpose av -> temps_in_array_view av
        | Air.Tabulate (b,e,s) -> Temp.Set.of_list [b;e;s]
        | Air.Array_index (t1, t2) -> Temp.Set.of_list [t1; t2;]
      in

      let don't_ask = Temp.Hash_set.create () in
      let array_view_host_args =
        let x = List.folding_map bound_array_views
          ~init:Temp.Set.empty
          ~f:(fun ctx (t1, t2, av) -> (Set.add (Set.add ctx t1) t2, Set.diff (temps_in_array_view av) ctx))
          |> Temp.Set.union_list
        in
        let y =
          Hash_set.fold ctx.backed_temps ~init:x ~f:(fun acc elem ->
            if Set.mem acc elem
              then Set.fold (Map.find_exn ctx.result.Ano.backing_temps elem) ~init:(Set.remove acc elem) ~f:(fun acc x ->
                Hash_set.add don't_ask x;
                Set.add acc x)
              else acc)
        in
        Set.to_list y
      in

      let host_args = List.concat [
        array_view_host_args;
        Set.to_list Ano.(kernel_info.free_variables);
      ] in

      let device_args =
        List.map host_args ~f:(fun host_t ->
          match Temp.Table.find ctx.allocation_method host_t with
          | Some `Just_device -> failwithf "I don't think this is possible (jd) `%d`" (Temp.to_int host_t) ()
          | Some (`Host_and_device t) -> t
          | Some (`Host_and_device_no_malloc_on_host t) -> t
          | Some `Unallocated ->
              let new_temp = Temp.next (Temp.to_type host_t) () in
              Temp.Table.set ctx.allocation_method ~key:host_t ~data:(`Host_and_device_no_malloc_on_host new_temp);
              new_temp
          | None ->

          match Temp.to_type host_t with
          | Tc.Array _ ->
              let new_temp = Temp.next (Temp.to_type host_t) () in
              Temp.Table.add ctx.allocation_method ~key:host_t ~data:(`Host_and_device new_temp)
                |> dedup __LINE__;
              new_temp
          | Tc.Int | Tc.Float (*TODO: which others?*) -> host_t
          | _ -> failwith "not yet")
      in

      let device_dest_temp =
        let new_temp = Temp.next (Ir.type_of_dest dest) () in
        Temp.Table.add ctx.allocation_method ~key:(match dest with
          | Ir.Return _ -> Option.value_exn ctx.out_param
          | Ir.Dest t -> t) ~data:(`Host_and_device new_temp) |> dedup __LINE__;
        new_temp
      in

      (* Mark additional buffers as just-device. *)
      let additional_buffers = Set.to_list Ano.(kernel_info.additional_buffers) in
      List.iter additional_buffers ~f:(fun key ->
        Temp.Table.add ctx.allocation_method ~key ~data:`Just_device |> dedup __LINE__);

      (* In addition to the array view args, also get the args for the
       * additional buffers and free variables to pass to the kernel launch.
       *)
      let args = List.concat [
        device_args;
        additional_buffers;

        (* We also need to pass in the dimensions of the original parameters! :) *)
        ctx.dims_of_params;
      ] in

      (* we need this for naming parameters *)
      let args_with_host_args = List.concat [ host_args; additional_buffers; ctx.dims_of_params; ] in

      (* Process:
       - Allocate what needs to be allocated of them on the device.
       - Translate the kernel body.
       - Place the args / body into the kernel.
       - Launch.
      *)

      (* We can re-use argument names as parameter names since all the arguments
       * are unique temps (and therefore will have unique parameter names).
       *)
      let output_buffer_param = Temp.next (Ir.type_of_dest dest) () in
      let (kernel_params, kernel_args) =
        (* List of tuples of types, params, and args *)
        let tpas = List.concat [
          (* Don't forget to include as a parameter the dest. *)
          List.return ((trans_type (Ir.type_of_dest dest), temp_name output_buffer_param), temp_to_var device_dest_temp);
          List.map2_exn args args_with_host_args ~f:(fun t t' -> ((trans_type (Temp.to_type t), temp_name t'), temp_to_var t));
        ]
        in List.unzip tpas
      in

      (* Total length of all the things we're parallel over. *)
      let bound_temps = List.map ~f:Tuple3.get1 bound_array_views in
      List.iter bound_temps ~f:(fun key ->
        Temp.Table.set ctx.allocation_method ~key ~data:`Unallocated);
      let lengths = List.map ~f:(get_length ctx) bound_temps in
      let total_length = multiply_cuda_exprs lengths in

      let gdim = (CU.IConst 256L, CU.IConst 1L, CU.IConst 1L) in
      let bdim = (CU.Binop (CU.DIV, total_length, CU.IConst 256L), CU.IConst 1L, CU.IConst 1L) in (* Todo: make this blocks/thrd  *)
      let index_expr =
        (* blockDim.x * blockIdx.x + threadIdx.x *)
        (* TODO: change this? *)
        CU.(Binop (ADD, Binop (MUL, KVar (BlockDim X), KVar (BlockIdx X)),
                        KVar (ThreadIdx X)))
      in

      (* Ok, so we must split the index_expr into the corresponding indices for each of the
       * bound_temps. *)
      let indices = List.map bound_array_views ~f:Tuple3.get2 in
      let start_indices =
        Option.value_map ctx.lvalue
          ~default:[]
          ~f:(fix (fun loop -> function
                | Expr.Index (e, Expr.Temp t) -> t :: loop e
                | _ -> []))
      in

      let kernel = CU.({
        typ = Device;
        ret = Void;
        name = "K_" ^ fn_next ();
        params = kernel_params;
        body =
          begin
            (* In the body, the user assigns to this guy. *)
            let i_temp = Temp.next Tc.Int () in
            let i = temp_to_var i_temp in
            let acc_temp = Temp.next Tc.Int () in
            let acc = temp_to_var acc_temp in
            let lvalue = List.fold_left (start_indices @ indices)
              ~init:(Expr.Temp output_buffer_param) ~f:(fun acc i -> Expr.Index (acc, Expr.Temp i)) in
            List.concat [
              [ CU.DeclareAssign (CU.Integer, temp_name i_temp, index_expr);
                CU.DeclareAssign (CU.Integer, temp_name acc_temp, i);
              ];
              List.concat_map (List.zip_exn indices lengths) ~f:(fun (idx, len) ->
                [ CU.DeclareAssign (CU.Integer, temp_name idx, CU.Binop (CU.MOD, acc, len));
                  CU.Assign (acc, CU.Binop (CU.DIV, acc, len));
                ]
              );
              trans_seq_stmt { ctx with lvalue = Some lvalue; } body;
            ]
          end;
      }) in

      let memcpy tfr a b = List.map2_exn a b ~f:(fun dest src ->
        let typ = Temp.to_type dest in
        if Hash_set.mem don't_ask src then None else
        match typ with
        | Tc.Array _ ->
            let lengths =
              try get_lengths ctx dest
              with _ -> get_lengths ctx src
            in
            let len = CU.Binop (CU.MUL, multiply_cuda_exprs lengths, CU.Size_of (remove_arrays typ)) in
            Some (CU.Transfer (temp_to_var dest, temp_to_var src, len, tfr))
        | Tc.Int | Tc.Float | Tc.Bool -> None
        | _ -> failwith "Not yet."
      ) |> List.filter_opt in

      List.concat [
        (* Memcpy the host args into the device args. *)
        memcpy CU.(Host, Device) device_args host_args;
        [CU.Launch (gdim, bdim, kernel, kernel_args)];
        CU.[Transfer (
          dest_to_lvalue_exn ctx dest,
          trans_expr ctx (List.fold_right start_indices ~init:(Expr.Temp device_dest_temp) ~f:(fun i acc -> Expr.Index (acc, Expr.Temp i))),
          CU.Binop (CU.MUL, CU.Size_of (remove_arrays (Ir.type_of_dest dest)),
            multiply_cuda_exprs (match dest with
            | Ir.Return _ -> get_lengths ctx (Option.value_exn ctx.out_param)
                |> Fn.flip List.drop (List.length start_indices)
            | Ir.Dest t -> get_lengths ctx t)),
          (Device, Host))
        ]
      ]

and trans_seq_stmt_stmt ctx stmt = trans_a_stmt trans_seq_stmt ctx stmt
and trans_par_stmt_stmt ctx stmt = trans_a_stmt trans_par_stmt ctx stmt

(* Store array_view in dest using loop_var as the counter variable. *)
and trans_array_view (ctx : context) (dest, loop_temp, index_fn, lengths) : CU.cuda_stmt =
  let dest_arr = dest_to_expr_lvalue ctx dest in
  let lvalue = Expr.Index (dest_arr, Expr.Temp loop_temp) in

  (* Ok, we have to case on the type of the destination in order to figure out whether we
   * need to memcpy or if a simple assignment is sufficient. *)
  let rec loop lvalue lens = function
    | Many_fn.Result src -> CU.Assign (trans_expr ctx lvalue, src)
    | Many_fn.Fun f ->
        let t_idx = Temp.next Tc.Int () in
        (* TODO: do more than one level *)
        let hdr = create_loop ~counter:t_idx (List.hd_exn lens) in
        CU.Loop (hdr, [ loop (Expr.Index (lvalue, Expr.Temp t_idx)) (List.tl_exn lens) (f (Expr.Temp t_idx)) ])
  in
  loop lvalue lengths (index_fn loop_temp)

let rec extract_kernel_launches : CU.cuda_stmt list -> CU.cuda_func list =
  List.fold_left ~init:[] ~f:(fun kernel_launches -> function
    | CU.Return _ | CU.Sync | CU.Nop | CU.DeclareArray _ | CU.DeclareAssign _ | CU.Assign _ | CU.Declare _
    | CU.AssignOp _ | CU.Cuda_malloc _ | CU.Malloc _ | CU.Free _ | CU.Transfer _ | CU.InitStruct _
    | CU.Expression _ | CU.Memcpy _ -> kernel_launches
    | CU.Launch (_, _, cuda_func, _) -> cuda_func :: kernel_launches
    | CU.Loop (_, stmts) -> extract_kernel_launches stmts @ kernel_launches
    | CU.Condition (_, stmts1, stmts2) ->
        let gstmts1 = extract_kernel_launches stmts1 in
        let gstmts2 = extract_kernel_launches stmts2 in
        gstmts1 @ gstmts2 @ kernel_launches)

let rec extract_kernel_launches : CU.cuda_stmt list -> CU.cuda_func list =
  List.fold_left ~init:[] ~f:(fun kernel_launches -> function
    | CU.Sync | CU.Nop | CU.DeclareArray _ | CU.DeclareAssign _ | CU.Assign _ | CU.Declare _
    | CU.AssignOp _ | CU.Malloc _ | CU.Cuda_malloc _ | CU.Free _ | CU.Transfer _
    | CU.Expression _ | CU.Memcpy _ | CU.Return _ | CU.InitStruct _ -> kernel_launches
    | CU.Launch (_, _, cuda_func, _) -> cuda_func :: kernel_launches
    | CU.Loop (_, stmts) -> extract_kernel_launches stmts @ kernel_launches
    | CU.Condition (_, stmts1, stmts2) ->
        let gstmts1 = extract_kernel_launches stmts1 in
        let gstmts2 = extract_kernel_launches stmts2 in
        gstmts1 @ gstmts2 @ kernel_launches)

let trans_struct_decl ~(key : Tc.ident) ~(data : Tc.struct_type) : CU.cuda_gstmt =
  CU.StructDecl (key, List.map data ~f:Tc.(fun f -> (trans_type f.field_type, f.field_name)))

(* The translated gstmt contains within it kernel launches.
 * These kernel launches actually include the kernel DEFINITION.
 * It's up to a later phase to float all these kernel definitions to the top level.
 *)
let trans (program : Air.t) (struct_decls : Tc.struct_type Tc.IdentMap.t) (result : Ano.result) : CU.cuda_gstmt list =
  let params = trans_params Ano.(result.params) in
  let lvalue = Ano.(result.out_param) in
  let allocation_method = Temp.Table.create () in
  let body = trans_par_stmt {
    result;
    allocation_method;
    lvalue = Option.map ~f:(fun t -> Expr.Temp t) lvalue;
    lvalue_lengths = begin
      let open Option.Monad_infix in
      lvalue >>= fun t ->
      Map.find Ano.(result.buffer_infos) t >>= fun x ->
      Option.return Ano.(x.length)
    end;
    out_param = lvalue;
    backed_temps = Temp.Hash_set.create ();
    dims_of_params = List.concat_map Ano.(result.params) ~f:(function
      | Ano.Param.Array (_, dims) -> dims
      | Ano.Param.Not_array _ -> []);
  } Air.(program.body) in

  (* Malloc on the device and on the host. *)
  let malloc'ing =

    List.concat_map (Map.to_alist Ano.(result.buffer_infos)) ~f:(fun (t, bi) ->
      let f t k =
        let typ = Temp.to_type t in k CU.(
          trans_type typ,
          temp_name t,
          Binop (MUL,
            multiply_cuda_exprs (List.map ~f:trans_len_expr Ano.(bi.length)),
            Size_of (remove_arrays typ)))
      in
      match Temp.Table.find allocation_method t with
      | None when
          List.exists Ano.(result.params)
          ~f:(function | (Ano.Param.Array (t', _)) -> Temp.equal t t'
                       | _ -> false) -> []
      | Some `Unallocated -> []
      | None -> f t (fun (a, b, c) -> [ CU.Malloc (a, b, c) ])
      | Some `Just_device -> f t (fun (a, b, c) -> [ CU.Cuda_malloc (a, b, c) ])
      | Some `Host_and_device_no_malloc_on_host t' -> f t' (fun (a, b, c) -> [ CU.Cuda_malloc (a, b, c) ])
      | Some `Host_and_device t' when
          List.exists Ano.(result.params)
          ~f:(function | (Ano.Param.Array (t', _)) -> Temp.equal t t'
                       | _ -> false) -> f t' (fun (a, b, c) -> [ CU.Cuda_malloc (a, b, c) ])
      | Some `Host_and_device t' -> f t (fun (a, b, c) -> f t' (fun (a', b', c') -> [ CU.Malloc (a, b, c); CU.Cuda_malloc (a', b', c'); ])))
  in

  let struct_decls = Map.mapi ~f:trans_struct_decl struct_decls |> Map.to_alist |> List.map ~f:snd in
  let gdecls = extract_kernel_launches body in
  List.concat [
    [ CU.Include "dag_utils.cpp"; ];
    struct_decls;
    gdecls |> List.map ~f:(fun x -> CU.Function x);
    List.return
      CU.(Function { typ = Host;
            ret = begin
              match Ano.(result.out_param) with
              | None -> trans_type Air.(program.return_type)
              | Some _ -> Void
            end;
            name = "dag_" ^ Air.(program.fn_name);
            params;
            body = malloc'ing @ body; });
  ]
