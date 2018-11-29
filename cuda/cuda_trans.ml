open Core

module Ano = Annotated_air
module Length_expr = Annotated_air.Length_expr
module Expr = Annotated_air.Expr
module Op = Ir.Operator
module CU = Cuda_ir

let failwith n = failwith ("AIR -> CUDA : "^n)

type context = {
  result : Ano.result;
  lvalue : Temp.t;
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
let dest_to_lvalue ~(on_return : Temp.t) (dest : Ir.dest) : CU.cuda_expr =
  match dest with
  | Ir.Return _ -> temp_to_var on_return
  | Ir.Dest t -> temp_to_var t

let dest_to_temp ~(on_return : Temp.t) (dest : Ir.dest) : Temp.t =
  match dest with
  | Ir.Return _ -> on_return
  | Ir.Dest t -> t

(* Get the device version of a variable *)
let var_to_dev var =
  match var with
  | CU.Var v -> CU.Var ("d_" ^ v)
  | _ -> failwith "Cannot device a non-variable."

(* Convert to cuda constant *)
let con i = CU.Const (Int64.of_int_exn i)

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

(* Pointer, Lengths of dimensions *)
type nd_array = Temp.t * (Temp.t list)
let nd_array_id = "dag_nd_array_t"
let nd_array_dims = "dim"
let nd_array_lens = "lens"
let nd_array_data = "data"

(* Global struct representing a multidimensional array.
   Polymorphic - feed it the type to get the decl you want.
   Should probably be declared in every DAG program. s*)
let dag_array_struct typ : CU.cuda_gstmt =
  CU.StructDecl (nd_array_id^"_"^typ_name typ,
    [
     (CU.Integer, nd_array_dims);
     (CU.Pointer CU.Integer, nd_array_lens);
     (CU.Pointer (trans_typ typ), nd_array_data);
    ]
  )

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

let nop = CU.Nop

(* Creates the loop header (initial value, max value, and increment stride)
 * -> for (var = init;var < limit; var += stride) *)
let trans_incr_loop_hdr var init limit stride =
 (CU.Assign (var, init), CU.Cmpop (CU.LT, var, limit), CU.AssignOp (CU.ADD, var, stride))

(* Actually perform the reduction! *)
let make_reduce op args =
  match (op, args) with
    | Op.Binop bin, [a;b] -> CU.AssignOp (trans_binop bin, a, b)
    | Op.Binop _, _ -> failwith "Binary operators have two arguments."
    | Op.Unop _, _ -> failwith "Cannot reduce with a unary operator."

let rec trans_len_expr : Length_expr.t -> CU.cuda_expr = function
  | Length_expr.Temp t -> temp_to_var t
  | Length_expr.Mult (e1, e2) -> CU.Binop (CU.MUL, trans_len_expr e1, trans_len_expr e2)

(* Find length of current lvalue. *)
let get_length (result : Ano.result) (t : Temp.t) : CU.cuda_expr =
  let buf_info = Map.find Ano.(result.buffer_infos) t in
   match buf_info with
     | Some inf -> trans_len_expr (Ano.(inf.length) 0)
     | None -> failwith "Couldn't find buffer size (get length)"

(* Used to perform operations with length expressions. *)
let build_cached_reduce_expr (op : CU.binop) (exprs : CU.cuda_expr list) =
  let rec reduce_expr exprs =
    match exprs with
    | [] -> failwith "Cannot bootstrap empty expression."
    | [expr] -> expr
    | ex::xs -> (CU.Binop (op, ex, reduce_expr xs))
  in
  reduce_expr exprs

(* Initialize a loop body from a given ctx. *)
let init_loop (ctx : context) (loop_var : Temp.t) (dest : Ir.dest) =
  let len = get_length ctx.result ctx.lvalue in
  let dest_var = dest_to_lvalue ctx.lvalue dest in
  let hdr = trans_incr_loop_hdr dest_var (con 0) (len) (con 1) in
  (dest_to_temp ctx.lvalue dest, dest_var, temp_to_var loop_var, hdr)

(* Todo: Verify that trans_array_view actually works. *)
let rec trans_a_stmt : type a. (context -> a -> CU.cuda_stmt list) -> context -> a Air.stmt -> CU.cuda_stmt list =
  fun continuation ctx -> function
    | Air.For (dest, (loop_var, view), stmt) ->
        let (dest_tmp, dest_var, loop_var, hdr) = init_loop ctx loop_var dest in
        let stms = trans_array_view ctx (loop_var, dest_tmp, view) in
        let body = continuation ctx stmt in
        [ CU.Loop (hdr, stms @ body) ]
    | Air.Run (dest, view) ->
        let (dest_tmp, dest_var, loop_var, hdr) =
          init_loop ctx (Temp.next (Ir.type_of_dest dest) ()) dest in
        let stms = trans_array_view ctx (loop_var, dest_tmp, view) in
        [ CU.Loop (hdr, stms) ]
    | Air.Block s -> List.concat_map ~f:(continuation ctx) s
    | Air.Reduce (dest, op, init, view) ->
        let (dest_tmp, dest_var, loop_var, hdr) = init_loop ctx (Temp.next (Ir.type_of_dest dest) ()) dest in
        let stms = trans_array_view ctx (loop_var, dest_tmp, snd view) in
        let asnop = make_reduce op [ dest_var; loop_var; ] in
        [ CU.Assign (dest_var, trans_op init);
          CU.Loop (hdr, stms @ [asnop]);
        ]
    | Air.Nop -> []

and trans_seq_stmt (ctx : context) (stmt : Air.seq_stmt) : CU.cuda_stmt list =
  match stmt with
  | Air.Binop (d, op, s1, s2) ->
      [CU.Assign (dest_to_lvalue ctx.lvalue d,
       CU.Binop (trans_binop op, trans_op s1, trans_op s2))]
  | Air.Unop (d, op, s) ->
      [CU.Assign (dest_to_lvalue ctx.lvalue d,
      CU.Unop (trans_unop op, trans_op s))]
  | Air.Assign (d, s) ->
      [CU.Assign (dest_to_lvalue ctx.lvalue d, trans_op s)]
  | Air.Seq_stmt seq_stm -> trans_seq_stmt_stmt ctx seq_stm

and trans_seq_stmt_stmt (ctx : context) (stmt : Air.seq_stmt Air.stmt) : CU.cuda_stmt list =
  trans_a_stmt trans_seq_stmt ctx stmt

and trans_par_stmt_stmt (ctx : context) (stmt : Air.par_stmt Air.stmt) : CU.cuda_stmt list =
  trans_a_stmt trans_par_stmt ctx stmt

and trans_par_stmt (ctx : context) (stmt : Air.par_stmt) : CU.cuda_stmt list =
  match stmt with
  | Air.Parallel (dest, id, bound_array_views, body) ->
      let kernel_info =
        match Map.find Ano.(ctx.result.kernel_infos) id with
        | Some k_inf -> k_inf
        | None -> failwith "Failed to find kernel info. (trans_stmt_par)"
      in
      let buffer_infos = Ano.(ctx.result.buffer_infos) in
      let var_set = Ano.(kernel_info.free_variables) in
      let ad_bufs = Ano.(kernel_info.additional_buffers) in

      (* Create both the arguments and parameters for a buffer to be passed to a kernel launch. *)
      let kernel_args_and_types (info : Ano.buffer_info) (t : Temp.t) : (CU.cuda_type * CU.cuda_expr) list =
        (* If the array is n-dimensional, it has n many length arguments that must be passed to
         * the kernel. *)
        let array_param = (trans_typ Ano.(info.typ), temp_to_var t) in
        let dim_params = List.init Ano.(info.dim) ~f:(fun i ->
          (CU.Integer, trans_len_expr Ano.(info.length i))) in
        array_param :: dim_params
      in

      (* Look up the buffer info in the map and calculate its type and expression. *)
      let lookup_kernel_args_and_types (t : Temp.t) : (CU.cuda_type * CU.cuda_expr) list =
        match Map.find buffer_infos t with
        | Some info -> kernel_args_and_types info t
        | None -> failwith "Buffer not found."
      in

      let bound_temps = List.map ~f:fst bound_array_views in

      (* - Translate the lengths and free variables. *)
      (* - Determine the arg list. *)
      let args_and_types = List.concat [
        List.concat_map ~f:lookup_kernel_args_and_types bound_temps;
        List.concat_map ~f:lookup_kernel_args_and_types (Set.to_list ad_bufs);
        List.map ~f:(fun t -> (CU.Integer, temp_to_var t)) (Set.to_list var_set);
      ] in

        (* Process:
         - Allocate what needs to be allocated of them on the device.
         - Translate the kernel body.
         - Place the args / body into the kernel.
         - Launch.
        *)

      let kernel_args = [] in

      let kernel = CU.({
        typ = Device;
        ret = Void;
        name = "K_"^fn_next ();
        params = kernel_args;
        body = [];
      }) in

      (* (Necessary?) Optimisation: Evaluate this before launching kernel. *)
      let len = bound_temps (* Length expression *)
        |> List.map ~f:(get_length ctx.result)
        |> (build_cached_reduce_expr CU.MUL)
      in
      let gdim = (con 256, con 1, con 1) in
      let bdim = (len, con 1, con 1) in (* Todo: make this blocks/thrd  *)
      let params = List.map ~f:(fun (_, a) -> CU.Var a) kernel_args in
      [CU.Launch (gdim, bdim, kernel, params)]

  | Air.Par_stmt par_stm -> trans_par_stmt_stmt ctx par_stm
  | Air.Seq seq_stm -> (* Pipe it in. *)
      trans_seq_stmt ctx seq_stm

(* Returns:  (list of [list of processing per loop run], array temp, array length) *)
and trans_array_view (ctx : context) state : CU.cuda_stmt list =
  let (loop_var, dest_arr, arr_view) = state in
  let len = get_length ctx.result dest_arr in
  let dest_var = CU.Index (temp_to_var dest_arr, loop_var) in
  match snd arr_view with
  | Air.Array arr ->  (* Extract and assign to the loop temporary. *)
      let asgn_instr = CU.Assign (dest_var, CU.Index (temp_to_var arr, loop_var)) in
      [asgn_instr]

  | Air.Zip_with (op, subviews) ->
    (* Inner loop operation function *)
    let make_op extracted_temps =
      (match op, extracted_temps with
        | Op.Unop u, [t]->
            CU.Assign (dest_var, CU.Unop (trans_unop u, t))
        | Op.Binop b, [t1;t2] ->
            CU.Assign (dest_var, CU.Binop (trans_binop b, t1, t2))
        | _ -> failwith "Incorrect arg lengths to ZipWith!")
    in
      (* Pipe this into the zip function. *)
      let dests = List.map subviews ~f:(fun (typ, _) -> Temp.next typ ()) in
      let oper = dests |> List.map ~f:(temp_to_var) |> make_op in
      let trans_list =  List.map dests ~f:(fun d -> trans_array_view ctx (loop_var, d, arr_view)) in
      List.concat trans_list @ [oper]

  | Air.Reverse subview -> (* Flip the indexing strategy. *)
      let rev_lvar = CU.Binop (CU.SUB, len, loop_var) in
      trans_array_view ctx (rev_lvar, dest_arr, subview)

  | Air.Transpose subview -> (* Not sure this is where we want to do this. *)
      failwith "We don't support transposing runs. Or maybe we do. But not yet."

      (* let dev_dest = var_to_dev (temp_to_var dest_arr) in
      let prev_ins = trans_array_view context (loop_var, dest_arr, subview, dest_var) in
      let ins = CU.launch_transpose (temp_to_var arr) dev_arr dev_dest in
      prev_ins @ [ins] *)

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
  let body = trans_par_stmt { result; lvalue; } Air.(program.body) in
  CU.(Function {
      typ = Host;
      ret = Void;
      name = "dag_main";
      params;
      body;
  })
