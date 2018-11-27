open Core

module D = Ir.Dest.T
module CU = CudaIR
module Op = Ir.Operator

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
  | D.Return -> temp_to_var lval
  | D.Dest t -> temp_to_var t

let dest_to_temp lval dest = 
  match dest with 
  | D.Return -> lval
  | D.Dest t -> t

(* Get the device version of a variable *)
let var_to_dev var =
  match var with 
  | CU.Var v -> CU.Var ("d_"^v)
  | _ -> failwith "Cannot device a non-variable."

(* Convert to cuda constant *)
let con i = CU.Const (Int64.of_int_exn i)

(* These flatten out any nesting. We're presuming the nd_array struct takes care of it. *)
let rec typ_name (typ : Ast.typ) = 
  match typ with 
  | Ast.Array t -> typ_name t
  | Ast.Ident t -> (t : string)

let rec trans_typ (typ : Ast.typ) = 
  match typ with
  | Ast.Ident t -> 
    (match t with
    | "int" -> CU.Integer
    | "bool" -> CU.Boolean
    | "float" -> CU.Float
    | "double" -> CU.Double
    | _ -> failwith ("Don't currently support complex type trans."))
  | Ast.Array t -> trans_typ t

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

(* Extract the length information from the recursive AIR param. *)

(* Translate an AIR parameter list into a list of CUDA fnargs. 
   This requires the actual struct objects to be initialized elsewhere. *)
let trans_params (params : (Temp.t) list) : (CU.cuda_type * CU.cuda_ident) list = 
   List.map params ~f:(fun (t) -> (CU.Integer, temp_name t))
   (* TODO: NEED TYPES *)

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

let trans_incr_loop_hdr var init limit stride = 
 (CU.Assign(var,init),CU.Cmpop(CU.LT,var,limit),CU.AssignOp(CU.ADD,var,stride))


let rec trans_seq_stmt cur_lval stmt : CU.cuda_stmt list = 
  match stmt with 
  | Air.Binop (d,op,s1,s2) -> 
      [CU.Assign(dest_to_var cur_lval d,
       CU.Binop(trans_binop op, trans_op s1, trans_op s2))]
  | Air.Unop (d,op,s) -> 
      [CU.Assign(dest_to_var cur_lval d,
      CU.Unop(trans_unop op,trans_op s))]
  | Air.Assign (d,s) -> 
      [CU.Assign(dest_to_var cur_lval d,trans_op s)]
  | Air.Seq_stmt stm ->
    (* Todo: Add length information from the context. *)
    (* Todo: Verify that trans_array_view actually works. *)
    match stm with 
    | Air.For (dest,(loop_var,view),seq_stmt) -> 
        (* Sequential map. *)
        let loop_var = temp_to_var loop_var in
        let dest_var = dest_to_var cur_lval dest in 
        let dest_tmp = dest_to_temp cur_lval dest in
        let len = con 0 in (* <--- Length goes here *)
        let hdr = trans_incr_loop_hdr (dest_var) (con 0) (len) (con 1) in
        let (stms,_) = trans_array_view (loop_var,dest_tmp,view,dest_var) in
        let body = trans_seq_stmt cur_lval seq_stmt in 
        [CU.Loop(hdr,stms @ body)]  

    | Air.Run (dest,view) -> 
        (* Builds a loop and runs it. needs length info. *)
        let loop_var = temp_to_var (Temp.next ()) in
        let dest_var = dest_to_var cur_lval dest in 
        let dest_tmp = dest_to_temp cur_lval dest in
        let len = con 0 in (* <--- Length goes here *)
        let hdr = trans_incr_loop_hdr (dest_var) (con 0) (len) (con 1) in
        let (stms,_) = trans_array_view (loop_var,dest_tmp,view,dest_var) in
        [CU.Loop(hdr,stms)]

    | Air.Block s -> List.map ~f:(trans_seq_stmt cur_lval) s |> List.concat
    
    (* Sequential Reduction, oh boy. *)
    | Air.Reduce (dest,op,init,view) -> 
        let loop_var = temp_to_var (Temp.next ()) in
        let dest_var = dest_to_var cur_lval dest in 
        let dest_tmp = dest_to_temp cur_lval dest in
        let len = con 0 in (* <--- Length goes here *)
        let hdr = trans_incr_loop_hdr (dest_var) (con 0) (len) (con 1) in
        let (stms,_) = trans_array_view (loop_var,dest_tmp,view,dest_var) in
        (* Actually perform the reduction! *)
        let make_reduce op args = 
        (match (op,args) with  
         | Op.Binop bin,[a;b] -> CU.AssignOp(trans_binop bin,a,b)
         | Op.Binop _,_ -> failwith "Binary operators have two arguments."
         | Op.Unop _,_ -> failwith "Cannot reduce with a unary operator."
        ) in
        let asnop = make_reduce op [dest_var;loop_var] in
       [CU.Assign(dest_var,trans_op init);CU.Loop(hdr,stms@[asnop])]

    | Air.Nop -> []

and trans_stmt_parallel (stmt : (Air.par_stmt Air.stmt)) = 
   match stmt with
  | Air.For (seqs,seq_stms,_) ->
      (* This implies either a kernel launch (if we're not in a par block) 
         or an additional level of indexing (if we are) *)

      (* Process: 
         - Allocate an additional pointer for the list of sequences.
         - Translate the instructions and length.
         - Allocate all of them on the device.
         - Place that in the kernel. 
         - (Todo: adjust the return type to have kernels, or use a mutable ref)
         - Launch the kernel.
        *)  

      (* [CU.Loop(hdr,ins @ (List.map ~f:trans_seq_stmt seq_stms))] *)
      
      (* This is just a for loop. I think. But how does the loop variable integrate? *)
      (* Gotta do that continuation passing on the aViews and actually merge things. *) 
      nop
      (* let lvar = CU.Var "s" in
      let arrvar = temp_to_var dest in
      let idxvar = CU.Index(arrvar,lvar) in
      let init_state = (lvar,dest,aview,idxvar) in
      let (ins,arr,len) = trans_array_view init_state in
   
      let hdr = trans_incr_loop_hdr arrvar (con 0) (temp_to_var len) (con 1) in
      [CU.Loop(hdr,ins @ List.concat (List.map ~f:trans_par_stmt par_stms))] *)

  | Air.Run (dest,view) -> nop
      (* Actually run a queued sequence comptuation. *) 

  | Air.Block stms -> nop (* [trans_seq_stmt stm] *)
  | Air.Reduce (dest,op,src,view) -> nop
  | Air.Nop -> nop

(* Returns:  (list of [list of processing per loop run],array temp,array length) *)
and trans_array_view state : (CU.cuda_stmt list * Temp.t) = 
  let (loop_var,dest_arr,arr_view,dest_var) = state in
  match arr_view with 
  | Air.Array (arr) -> 
      (* We're gonna need a block context! *)
      (* Extract and assign to the loop temporary. *)
      (* But really we only want to do this on a run. *)
      let asgn_instr = CU.Assign(dest_var,CU.Index(temp_to_var arr,loop_var)) in
      ([asgn_instr],arr)

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
      let dests = List.map subviews ~f:(fun _ -> temp_to_var (Temp.next ())) in
      let trans_list =  List.map dests ~f:(fun d -> trans_array_view (loop_var,dest_arr,arr_view,d)) in
      let instr = make_op dests in
      let prev_instrs = trans_list |> List.map ~f:(fun (i,_) -> i) |> List.concat in
      (* let (_,_) = List.nth_exn trans_list 0 in *)
      (instr :: prev_instrs,dest_arr)

  | Air.Reverse subview -> (* Flip the indexing strategy. *)
      (* let rev_temp = temp_to_var (Temp.next ()) in *)
      (* Dummy allocation to extract the length. Sigh. *)
      ([],Temp.next ())
      (* let (_,_) = trans_array_view (loop_var,dest_arr,subview,rev_temp) in
      let rev_lvar = CU.Binop(CU.SUB,temp_to_var len,loop_var) in 
      trans_array_view (rev_lvar,dest_arr,subview,rev_temp) *)

  | Air.Transpose subview -> (* Not sure this is where we want to do this. *)
      let dev_dest = var_to_dev (temp_to_var dest_arr) in
      let (prev_ins,arr) = trans_array_view (loop_var,dest_arr,subview,dest_var) in
      let dev_arr = var_to_dev (temp_to_var arr) in
      let ins = CU.launch_transpose (temp_to_var arr) dev_arr dev_dest in
      (prev_ins @ [ins],dest_arr)

and trans_par_stmt stmt = 
  match stmt with 
  | Air.Parallel (dest,id,n_views,seq_stm) -> []
  | Air.Par_stmt par_stm -> []
  | Air.Seq seq_stm -> []

let translate (program : Air.t) : CU.cuda_gstmt = 
  let args = trans_params Air.(program.params) in
  let body = trans_par_stmt Air.(program.body) in
  CU.(Function {
    typ = Host; (* Might make other calls. This isn't those. *)
    ret = Void;
    name = "dag_main";
    args = args;
    body = body;
  })