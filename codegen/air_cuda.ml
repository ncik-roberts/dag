open Core

module IR = Air
module CU = CudaIR

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

(* Pointer, Lengths of dimensions *)
type nd_array = Temp.t * (Temp.t list)
let nd_array_id = "dag_nd_array_t"
let nd_array_dims = "dim"
let nd_array_lens = "lens"
let nd_array_data = "data"

(* Global struct representing a multidimensional array. 
   Should probably be declared in every DAG program. s*)
let dag_array_struct : CU.cuda_gstmt = 
  CU.StructDecl(nd_array_id,
    [
     (CU.Integer,nd_array_dims);
     (CU.Pointer CU.Integer,nd_array_lens);
     (CU.Pointer CU.Integer,nd_array_data);
    ]
  )

(* Extract the length information from the recursive AIR param. *)
let flatten_array p l : nd_array=
  let rec flatten par len acc =
    match par with 
    | IR.Temp_param t -> (t,List.rev (len::acc))
    | IR.Array_param {param = subp; length = subl} -> 
      flatten subp subl (len::acc)
  in
  flatten p l []

(* Translate an AIR parameter list into a list of CUDA fnargs. 
   This requires the actual struct objects to be initialized elsewhere. *)
let trans_params (params : IR.param list) : (CU.cuda_type * CU.cuda_ident) list = 
   let trans_param = function
   | IR.Array_param {param = p; length = l} -> 
      let (ptr,_) = flatten_array p l in
      (CU.Pointer (CU.Struct nd_array_id), temp_name ptr)
   | IR.Temp_param t -> (CU.Integer, temp_name t)
   in
   List.map params ~f:(trans_param)

(* Translate AIR operands into their cuda equivalents. s*)
let trans_op = 
  function
  | IR.Const c -> CU.Const (Int64.of_int32_exn c)
  | IR.Temp t -> CU.Var (temp_name t)

let trans_binop = 
  function
  | IR.Add -> CU.ADD
  | IR.Sub -> CU.SUB
  | IR.Mul -> CU.MUL
  | IR.Div -> CU.DIV
  | IR.Mod -> CU.MOD

let trans_unop op = CU.INCR (* Dummy translation, until it comes down the pipe. *)

let trans_seq_stmt : IR.seq_stmt -> CU.cuda_stmt = 
  function 
  | IR.Binop (d,op,s1,s2) -> 
      CU.Assign(temp_to_var d,
      CU.Binop(trans_binop op, trans_op s1, trans_op s2))
  | IR.Unop (d,op,s) -> 
      (* This will depend on the nature of the unop. *)
      CU.Assign(temp_to_var d,
      CU.Unop(trans_unop op,trans_op s))
  | IR.Assign (d,s) -> 
      CU.Assign(temp_to_var d,trans_op s)

let trans_par_stmt = function
  | IR.Par_for (seqs,seq_stms) -> 
      (* This implies either a kernel launch (if we're not in a par block) 
         or an additional level of indexing (if we are) *) []
  | IR.Seq_for (seq,par_stms) -> 
      (* This is just a for loop. I think. *) []
  | IR.Run (t,v) -> 
      (* Call the queued sequence computation *) []
  | IR.Seq stm -> [trans_seq_stmt stm]

let trans_body (body : IR.par_stmt list) : (CU.cuda_stmt) list = 
  body |> List.map ~f:(trans_par_stmt) |> List.concat

let translate (func : IR.t) : CU.cuda_gstmt = 
  let args = trans_params IR.(func.params) in
  let body = trans_body IR.(func.body) in 
  CU.(Function {
    typ = Device; (* Todo: Get this information! *)
    ret = Void;
    name = fn_next ();
    args = args;
    body = body;
  })