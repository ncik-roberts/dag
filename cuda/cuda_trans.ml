open Core

module Ano = Annotated_air
module Length_expr = Annotated_air.Length_expr
module Expr = Annotated_air.Expr
module Op = Ir.Operator
module CU = Cuda_ir

let failwith n = failwith ("AIR -> CUDA : " ^ n)

type context = {
  result : Ano.result;
  lvalue : CU.cuda_expr; (* this is an lvalue *)

  (* Temps indicating the dimensions of parameters. *)
  dims_of_params : Temp.t list;
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

(* on_return is the lvalue to use upon encountering a return stmt. *)
let dest_to_lvalue (ctx : context) (dest : Ir.dest) : CU.cuda_expr =
  match dest with
  | Ir.Return _ -> ctx.lvalue
  | Ir.Dest t -> temp_to_var t

(* These flatten out any nesting. We're presuming the nd_array struct takes care of it. *)
let rec typ_name (typ : Tc.typ) : string =
  match typ with
  | Tc.Array t -> typ_name t ^ "[]"
  | Tc.Int -> "int"
  | _ -> failwith "Not supported."

let rec trans_typ (typ : Tc.typ) : CU.cuda_type =
  match typ with
  | Tc.Int -> CU.Integer
  | Tc.Array t -> CU.Pointer (trans_typ t)
  | _ -> failwith "Don't currently support other types"

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
         let array_param = (trans_typ (Temp.to_type t), temp_name t) in
         let dim_params = List.map dims ~f:(fun t -> (CU.Integer, temp_name t)) in
         array_param :: dim_params

     (* A non-array parameter, sadly, is not. *)
     | Ano.Param.Not_array t -> List.return (CU.Integer, temp_name t))

(* Translate AIR operands into their cuda equivalents. s*)
let trans_op = function
  | Air.Const c -> CU.Const (Int64.of_int32_exn c)
  | Air.Temp t -> CU.Var (temp_name t)
  | Air.Dim (n, view) -> CU.Const (Int64.of_int_exn n)

let trans_binop = function
  | Ast.Plus -> CU.ADD
  | Ast.Minus -> CU.SUB
  | Ast.Times -> CU.MUL
  | Ast.Div -> CU.DIV
  | Ast.Mod -> CU.MOD

let trans_unop = function
  | Ast.Negate -> CU.NEG
  | Ast.Logical_not -> CU.NOT

(* Creates the loop header (initial value, max value, and increment stride)
 * -> for (var = init;var < limit; var += stride) *)
let trans_incr_loop_hdr ~loop_var ~lo ~hi ~stride =
 (CU.Assign (loop_var, lo), CU.Cmpop (CU.LT, loop_var, hi), CU.AssignOp (CU.ADD, loop_var, stride))

(* Actually perform the reduction! *)
let make_reduce op args =
  match (op, args) with
    | Op.Binop bin, [a;b] -> CU.AssignOp (trans_binop bin, a, b)
    | Op.Binop _, _ -> failwith "Binary operators have two arguments."
    | Op.Unop _, _ -> failwith "Cannot reduce with a unary operator."

let rec trans_len_expr : Length_expr.t -> CU.cuda_expr = function
  | Length_expr.Temp t -> temp_to_var t
  | Length_expr.Mult (e1, e2) -> CU.Binop (CU.MUL, trans_len_expr e1, trans_len_expr e2)

let rec trans_expr : Expr.t -> CU.cuda_expr = function
  | Expr.Temp t -> temp_to_var t
  | Expr.Const i -> CU.Const (Int64.of_int32 i)
  | Expr.Index (arr, i) -> CU.Index (trans_expr arr, trans_expr i)
  | Expr.Call (op, xs) ->
  let open Ir.Operator in
  match op, xs with
  | Binop b, [ e1; e2; ] -> CU.Binop (trans_binop b, trans_expr e1, trans_expr e2)
  | Unop u, [ e1; ] -> CU.Unop (trans_unop u, trans_expr e1)
  | _ -> failwith "Invalid."

(* Find a buffer info's length. *)
let get_length (ctx : context) (t : Temp.t) : CU.cuda_expr =
  match Map.find Ano.(ctx.result.buffer_infos) t with
  | Some inf -> trans_len_expr (List.hd_exn Ano.(inf.length))
  | None -> failwith "Couldn't find buffer size (get_length)"

let get_index (ctx : context) (t : Temp.t) : Temp.t -> CU.cuda_expr =
  match Map.find Ano.(ctx.result.buffer_infos) t with
  | Some inf ->
      (* Lookup function *)
      fun t ->
        trans_expr Ano.(inf.index [Expr.Temp t])
  | None -> failwith "Couldn't find buffer index (get_index)"

(* Create a loop that iterates over the buffer t. *)
let create_loop (ctx : context) (t : Temp.t) =
  let loop_temp = Temp.next Tc.Int () in
  let hdr = trans_incr_loop_hdr
    ~loop_var:(temp_to_var loop_temp)
    ~lo:(CU.Const 0L)
    ~hi:(get_length ctx t)
    ~stride:(CU.Const 1L)
  in (loop_temp, hdr)

(* Used to perform operations with length expressions. *)
let build_cached_reduce_expr (op : CU.binop) (exprs : CU.cuda_expr list) =
  let rec reduce_expr exprs =
    match exprs with
    | [] -> failwith "Cannot bootstrap empty expression."
    | [expr] -> expr
    | ex :: xs -> (CU.Binop (op, ex, reduce_expr xs))
  in
  reduce_expr exprs

(* Translate a statement irrespective of parallel/sequential semantics *)
let rec trans_a_stmt : type a. (context -> a -> CU.cuda_stmt list) -> context -> a Air.stmt -> CU.cuda_stmt list =
  fun continuation ctx -> function
    | Air.For (dest, (bound_temp, view), stmt) ->
        (* Get the destination buffer which the for loop is being assigned into. *)
        let dest_array = dest_to_lvalue ctx dest in

        (* Iterate over the bound temp (which Annotate stores as having all the
         * information of the view array). *)
        let (loop_temp, hdr) = create_loop ctx bound_temp in

        let body =
          (* The lvalue for the for body is an index into the destination array. *)
          (* Translate the body under the new lvalue. *)
          let ctx' = { ctx with lvalue = CU.(Index (dest_array, temp_to_var loop_temp)) } in
          continuation ctx' stmt in
        [ CU.Loop (hdr, body); ]

    | Air.Block s -> List.concat_map ~f:(continuation ctx) s
    | Air.Nop -> []
    | Air.Reduce _ | Air.Run _ ->
        failwith
          "Unable to perform reduce/run without specifically associated parallel or sequential semantics."

(* Translate a sequential statement *)
and trans_seq_stmt (ctx : context) (stmt : Air.seq_stmt) : CU.cuda_stmt list =
  let (<--) d src = CU.Assign (dest_to_lvalue ctx d, src) in

  match stmt with
  | Air.Binop (d, op, s1, s2) ->
      [ d <-- CU.Binop (trans_binop op, trans_op s1, trans_op s2) ]
  | Air.Unop (d, op, s) ->
      [ d <-- CU.Unop (trans_unop op, trans_op s) ]
  | Air.Assign (d, s) ->
      [ d <-- trans_op s ]
  | Air.Seq_stmt seq_stmt ->

  (* Run/reduce given sequential semantics. *)
  match seq_stmt with
  (* Don't need to bind view: we already figured out how to index into
   * it in the annotation phase. Based on the value of view, the
   * call to get_index will return the right indexing function. *)
  | Air.Run (dest, _) ->
      let dest_temp = Ir.temp_of_dest dest in
      let index_fn = get_index ctx dest_temp in
      let (loop_temp, hdr) = create_loop ctx dest_temp in
      let body = trans_array_view ctx (dest, loop_temp, index_fn) in
      [ CU.Loop (hdr, body) ]

  (* For reduce, we associate the buffer_info for the view with t. *)
  | Air.Reduce (dest, op, init, (t, _)) ->
      let lvalue = dest_to_lvalue ctx dest in
      let index_fn = get_index ctx t in
      let (loop_temp, hdr) = create_loop ctx (Ir.temp_of_dest dest) in
      let assign_stmt = make_reduce op [ lvalue; index_fn loop_temp; ] in
      [ CU.Assign (lvalue, trans_op init);
        CU.Loop (hdr, [assign_stmt]);
      ]
  | _ -> trans_seq_stmt_stmt ctx seq_stmt

and trans_par_stmt (ctx : context) (stmt : Air.par_stmt) : CU.cuda_stmt list =
  match stmt with
  | Air.Seq seq_stm -> trans_seq_stmt ctx seq_stm

  (* TODO: Make these actually run in parallel. *)
  | Air.Par_stmt (Air.Run (a, b)) -> trans_seq_stmt_stmt ctx (Air.Run (a, b))
  | Air.Par_stmt (Air.Reduce (a, b, c, d)) -> trans_seq_stmt_stmt ctx (Air.Reduce (a, b, c, d))
  | Air.Par_stmt par_stmt -> trans_par_stmt_stmt ctx par_stmt

  | Air.Parallel (dest, id, bound_array_views, body) ->
      let kernel_info =
        match Map.find Ano.(ctx.result.kernel_infos) id with
        | Some k_inf -> k_inf
        | None -> failwith "Failed to find kernel info. (trans_stmt_par)"
      in

      (* Get the temps of the array views to pass to the kernel launch. *)
      let rec temps_in_array_view (av : Air.array_view) : Temp.Set.t =
        match snd av with
        | Air.Zip_with (_, avs) -> Temp.Set.union_list (List.map avs ~f:temps_in_array_view)
        | Air.Reverse av -> temps_in_array_view av
        | Air.Array t -> Temp.Set.singleton t
        | Air.Transpose av -> temps_in_array_view av
      in
      let array_view_args =
        List.map bound_array_views ~f:(fun (_, av) -> temps_in_array_view av)
          |> Temp.Set.union_list
          |> Set.to_list
      in

      (* In addition to the array view args, also get the args for the
       * additional buffers and free variables to pass to the kernel launch.
       *)
      let args = List.concat [
        (* TODO: figure out how to pass the returned dest as an arg. *)
        array_view_args;
        Set.to_list Ano.(kernel_info.additional_buffers);
        Set.to_list Ano.(kernel_info.free_variables);

        (* We also need to pass in the dimensions of the original parameters! :) *)
        ctx.dims_of_params;
      ] in

      (* Process:
       - Allocate what needs to be allocated of them on the device.
       - Translate the kernel body.
       - Place the args / body into the kernel.
       - Launch.
      *)

      (* We can re-use argument names as parameter names since all the arguments
       * are unique temps (and therefore will have unique parameter names).
       *)
      let kernel_params = List.map args ~f:(fun t ->
        (Temp.to_type t |> trans_typ, temp_name t))
      in

      let kernel = CU.({
        typ = Device;
        ret = Void;
        name = "K_" ^ fn_next ();
        params = kernel_params;
        body = [];
      }) in

      (* (Necessary?) Optimisation: Evaluate this before launching kernel. *)
      (* TODO: Nick, figure this out. *)
      let len =
        let bound_temps = List.map ~f:fst bound_array_views in
        (* Length expression *)
        bound_temps
          |> List.map ~f:(get_length ctx)
          |> build_cached_reduce_expr CU.MUL
      in
      let gdim = (CU.Const 256L, CU.Const 1L, CU.Const 1L) in
      let bdim = (len, CU.Const 1L, CU.Const 1L) in (* Todo: make this blocks/thrd  *)
      let args = List.map ~f:(fun (_, a) -> CU.Var a) kernel_params in
      [CU.Launch (gdim, bdim, kernel, args)]

and trans_seq_stmt_stmt ctx stmt = trans_a_stmt trans_seq_stmt ctx stmt
and trans_par_stmt_stmt ctx stmt = trans_a_stmt trans_par_stmt ctx stmt

(* Store array_view in dest using loop_var as the counter variable. *)
and trans_array_view (ctx : context) (dest, loop_temp, index_fn) : CU.cuda_stmt list =
  let dest_arr = dest_to_lvalue ctx dest in
  let lvalue = CU.Index (dest_arr, temp_to_var loop_temp) in
  [ CU.Assign (lvalue, index_fn loop_temp) ]

(* The translated gstmt contains within it kernel launches.
 * These kernel launches actually include the kernel DEFINITION.
 * It's up to a later phase to float all these kernel definitions to the top level.
 *)
let trans (program : Air.t) (result : Ano.result) : CU.cuda_gstmt =
  let params = trans_params Ano.(result.params) in
  let lvalue = match Ano.(result.out_param) with
    (* When there is an output buffer, this is the lvalue that an Air `return`
     * stmt should write into. *)
    | Some t -> t

    (* Just make up a new temp in the case that there is no buffer
     * to write into. *)
    | None -> Temp.next Tc.Int ()
  in
  let body = trans_par_stmt {
    result;
    lvalue = temp_to_var lvalue;
    dims_of_params = List.concat_map Ano.(result.params) ~f:(function
      | Ano.Param.Array (_, dims) -> dims
      | Ano.Param.Not_array _ -> []);
  } Air.(program.body) in
  CU.(Function {
    typ = Host;
    ret = Void;
    name = "dag_main";
    params;
    body;
  })
