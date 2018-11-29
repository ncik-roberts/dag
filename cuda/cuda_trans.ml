open Core

module Ano = Annotated_air
module Length_expr = Annotated_air.Length_expr
module Expr = Annotated_air.Expr
module Op = Ir.Operator
module CU = Cuda_ir

let failwith n = failwith ("AIR -> CUDA : "^n)

(* Unique identifier for functions. s*)
let fn_name_ctr = ref 0
let fn_next () =
  incr fn_name_ctr;
  sprintf ("fn_%d") !fn_name_ctr

(* Translate a temp into a variable.*)
let temp_name t =
  sprintf ("t_%d") (Temp.to_int t)

(* Put that variable into a CUDA one. *)
let temp_to_var t =
  CU.Var (temp_name t)

let dest_to_var lval dest =
  match dest with
  | Ir.Return _ -> temp_to_var lval
  | Ir.Dest t -> temp_to_var t

let dest_to_temp lval dest =
  match dest with
  | Ir.Return _ -> lval
  | Ir.Dest t -> t

(* Get the device version of a variable *)
let var_to_dev var =
  match var with
  | CU.Var v -> CU.Var ("d_" ^ v)
  | _ -> failwith "Cannot device a non-variable."

(* Convert to cuda constant *)
let con i = CU.Const (Int64.of_int_exn i)

(* These flatten out any nesting. We're presuming the nd_array struct takes care of it. *)
let rec typ_name (typ : Tc.typ) =
  match typ with
  | Tc.Array t -> typ_name t ^ "[]"
  | Tc.Int -> "int"
  | _ -> failwith "Not supported."

let rec trans_typ (typ : Tc.typ) =
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
  CU.StructDecl(nd_array_id^"_"^typ_name typ,
    [
     (CU.Integer,nd_array_dims);
     (CU.Pointer CU.Integer,nd_array_lens);
     (CU.Pointer (trans_typ typ),nd_array_data);
    ]
  )


(* Translate an AIR parameter list into a list of CUDA fnargs.
   This requires the actual struct objects to be initialized elsewhere. *)
let trans_params (params : (Ano.Param.t) list) : (CU.cuda_type * CU.cuda_ident) list =
   List.map params ~f:(fun t ->
      match t with  (* TODO: NEED TYPES *)
      | Ano.Param.Array (t,dims) -> (CU.Pointer (CU.Integer), temp_name t)
      | Ano.Param.Not_array t -> (CU.Integer, temp_name t))

(* Translate AIR operands into their cuda equivalents. s*)
let trans_op =
  function
  | Air.Const c -> CU.Const (Int64.of_int32_exn c)
  | Air.Temp t -> CU.Var (temp_name t)
  | Air.Dim (n,view) -> CU.Const (Int64.of_int_exn n)

let trans_binop =
  function
  | Ast.Plus -> CU.ADD
  | Ast.Minus -> CU.SUB
  | Ast.Times -> CU.MUL
  | Ast.Div -> CU.DIV
  | Ast.Mod -> CU.MOD

let trans_unop =
  function
  | Ast.Negate -> CU.NEG
  | Ast.Logical_not -> CU.NOT

let nop = CU.Nop

(* Creates the loop header (initial value, max value, and increment stride)
 * -> for (var = init;var < limit; var += stride) *)
let trans_incr_loop_hdr var init limit stride =
 (CU.Assign(var,init),CU.Cmpop(CU.LT,var,limit),CU.AssignOp(CU.ADD,var,stride))

(* Actually perform the reduction! *)
let make_reduce op args =
  match (op,args) with
    | Op.Binop bin,[a;b] -> CU.AssignOp(trans_binop bin,a,b)
    | Op.Binop _,_ -> failwith "Binary operators have two arguments."
    | Op.Unop _,_ -> failwith "Cannot reduce with a unary operator."

let rec trans_len_expr : Length_expr.t -> CU.cuda_expr = function
  | Length_expr.Temp t -> temp_to_var t
  | Length_expr.Mult (e1, e2) -> CU.Binop (CU.MUL, trans_len_expr e1, trans_len_expr e2)

let get_length context temp =
   let buf_info = Map.find Ano.(context.buffer_infos) temp in
    match buf_info with
      | Some inf -> trans_len_expr (Ano.(inf.length) 0)
      | None -> failwith "Couldn't find buffer size (get length)"

(* Used to perform operations with length expressions. *)
let build_cached_reduce_expr (op : CU.binop) (exprs : CU.cuda_expr list) =
    let rec reduce_expr exprs =
      match exprs with
      | [] -> failwith "Cannot bootstrap empty expression."
      | [expr] -> expr
      | ex::xs -> (CU.Binop(op,ex,reduce_expr xs))
    in
    reduce_expr exprs

(* Initialize a loop body from a given context. *)
let init_loop context loop_var cur_lval dest =
    let len = get_length context cur_lval in
    let dest_var = dest_to_var cur_lval dest in
    let hdr = trans_incr_loop_hdr (dest_var) (con 0) (len) (con 1) in
    (dest_to_temp cur_lval dest,dest_var,temp_to_var loop_var,hdr)

(* Todo: Verify that trans_array_view actually works. *)
let rec trans_a_stmt : type a. (Ano.result -> Temp.t -> a -> CU.cuda_stmt list) -> Ano.result -> Temp.t -> a Air.stmt -> CU.cuda_stmt list =
    fun continuation context cur_lval ->
    function
    | Air.For (dest,(loop_var,view),stmt) ->
        let (dest_tmp,dest_var,loop_var,hdr) = init_loop context loop_var cur_lval dest in
        let stms = trans_array_view context (loop_var,dest_tmp,view) in
        let body = continuation context cur_lval stmt in
        [CU.Loop(hdr,stms @ body)]
    | Air.Run (dest, view) ->
        let (dest_tmp,dest_var,loop_var,hdr) = init_loop context (Temp.next (Ir.type_of_dest dest) ()) cur_lval dest in
        let stms = trans_array_view context (loop_var,dest_tmp,view) in
        [CU.Loop(hdr,stms)]
    | Air.Block s -> List.map ~f:(continuation context cur_lval) s |> List.concat
    | Air.Reduce (dest,op,init,view) ->
        let (dest_tmp,dest_var,loop_var,hdr) = init_loop context (Temp.next (Ir.type_of_dest dest) ()) cur_lval dest in
        let stms = trans_array_view context (loop_var,dest_tmp,snd view) in
        let asnop = make_reduce op [dest_var;loop_var] in
        [CU.Assign(dest_var,trans_op init);CU.Loop(hdr,stms@[asnop])]
    | Air.Nop -> []

and trans_stmt_seq (context: Ano.result) cur_lval (stmt : Air.seq_stmt) : CU.cuda_stmt list =
  match stmt with
  | Air.Binop (d,op,s1,s2) ->
      [CU.Assign(dest_to_var cur_lval d,
       CU.Binop(trans_binop op, trans_op s1, trans_op s2))]
  | Air.Unop (d,op,s) ->
      [CU.Assign(dest_to_var cur_lval d,
      CU.Unop(trans_unop op,trans_op s))]
  | Air.Assign (d,s) ->
      [CU.Assign(dest_to_var cur_lval d,trans_op s)]
  | Air.Seq_stmt seq_stm ->
      trans_a_stmt trans_stmt_seq context cur_lval seq_stm

and trans_stmt_par (context : Ano.result) cur_lval (stmt : Air.par_stmt) =
   match stmt with
  | Air.Parallel (seqs,id,lv_temp_list,seq_stm) ->
      let kernel_info =
        match Map.find Ano.(context.kernel_infos) id with
        | Some k_inf -> k_inf
        | None -> failwith "Failed to find kernel info. (trans_stmt_par)"
      in
      let main_bufs = Ano.(context.buffer_infos) in
      let var_set = Ano.(kernel_info.free_variables) in
      let ad_bufs = Ano.(kernel_info.additional_buffers) in

      let mk_kernel_buf_arg buf_map tmp =
        match Map.find buf_map tmp with
        | Some info -> [(trans_typ Ano.(info.typ),temp_to_var tmp);(CU.Integer, trans_len_expr (Ano.(info.length) 0))]
        | None -> failwith "Buffer not found (mk_kernel_buf_arg)"
      in

      let main_temps = lv_temp_list |> List.map ~f:(fun (temp,_) -> temp) in

      (* - Translate the lengths and free variables. *)
      (* - Determine the arg list. *)
      let args = (List.map ~f:(mk_kernel_buf_arg main_bufs) main_temps @
                 List.map ~f:(mk_kernel_buf_arg ad_bufs) (Map.keys ad_bufs) |> List.concat)
                 @ List.map ~f:(fun t -> (CU.Integer,temp_to_var t)) (Set.to_list var_set)

      in

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
        args = kernel_args;
        body = [];
      }) in

      (* (Necessary?) Optimisation: Evaluate this before launching kernel. *)
      let len = main_temps (* Length expression *)
                |> List.map ~f:(get_length context)
                |> (build_cached_reduce_expr CU.MUL)
      in
      let gdim = (con 256,con 1,con 1) in
      let bdim = (len,con 1,con 1) in (* Todo: make this blocks/thrd  *)
      let params = List.map ~f:(fun (_,a) -> CU.Var a) kernel_args in
      [CU.Launch(gdim,bdim,kernel,params)]

  | Air.Par_stmt par_stm ->
      trans_a_stmt trans_stmt_par context cur_lval par_stm
  | Air.Seq seq_stm -> (* Pipe it in. *)
      trans_stmt_seq context cur_lval seq_stm

(* Returns:  (list of [list of processing per loop run],array temp,array length) *)
and trans_array_view context state : (CU.cuda_stmt list) =
  let (loop_var,dest_arr,arr_view) = state in
  let len = get_length context dest_arr in
  let dest_var = CU.Index(temp_to_var dest_arr,loop_var) in
  match arr_view with
  | Air.Array (arr) ->  (* Extract and assign to the loop temporary. *)
      let asgn_instr = CU.Assign(dest_var,CU.Index(temp_to_var arr,loop_var)) in
      [asgn_instr]

  | Air.Zip_with(op,subviews) ->
    (* Inner loop operation function *)
    let make_op extracted_temps =
      (match op,extracted_temps with
        | Op.Unop u,[t]->
            CU.Assign(dest_var,CU.Unop(trans_unop u,t))
        | Op.Binop b,[t1;t2] ->
            CU.Assign(dest_var,CU.Binop(trans_binop b,t1,t2))
        | _ -> failwith "Incorrect arg lengths to ZipWith!")
    in
      (* Pipe this into the zip function. *)
      let dests = List.map subviews ~f:(fun _ -> Temp.next ()) in
      let oper = dests |> List.map ~f:(temp_to_var) |> make_op in
      let trans_list =  List.map dests ~f:(fun d -> trans_array_view context (loop_var,d,arr_view)) in
      List.concat trans_list @ [oper]

  | Air.Reverse subview -> (* Flip the indexing strategy. *)
      let rev_lvar = CU.Binop(CU.SUB,len,loop_var) in
      trans_array_view context (rev_lvar,dest_arr,subview)

  | Air.Transpose subview -> (* Not sure this is where we want to do this. *)
      failwith "We don't support transposing runs. Or maybe we do. But not yet."

      (* let dev_dest = var_to_dev (temp_to_var dest_arr) in
      let prev_ins = trans_array_view context (loop_var,dest_arr,subview,dest_var) in
      let ins = CU.launch_transpose (temp_to_var arr) dev_arr dev_dest in
      prev_ins @ [ins] *)

let trans (program : Air.t) (context : Ano.result) : Cuda_ir.t =
  let args = trans_params Ano.(context.params) in
  let body = trans_stmt_par context (Temp.next ()) Air.(program.body) in
  CU.(Function {
    typ = Host; (* Main function. *)
    ret = Void;
    name = "dag_main";
    args = args;
    body = body;
  })
