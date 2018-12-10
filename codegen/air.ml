open Core

(** Abstract IR *)

type array_view = Tc.typ * array_view'
  [@@deriving sexp]

and array_view' =
  | Array of Temp.t
  | Array_index of Temp.t * Temp.t
  | Zip_with of Ir.Operator.t * array_view list
    (* first array view: thing to filter.
     * second array view: thing of booleans.
     *)
  | Reverse of array_view
  | Tabulate of Temp.t * Temp.t * Temp.t (* lo, hi, step? *)
  | Transpose of array_view
  [@@deriving sexp]

type operand =
  | Const of Int32.t
  | Float of float
  | Bool of bool
  | Temp of Temp.t
  | IndexOp of Temp.t * Temp.t
  | Dim of int * array_view (* Nth dimension of an array *)
  [@@deriving sexp]

let type_of_operand : operand -> Tc.typ = function
  | Const _ -> Tc.Int
  | Float _ -> Tc.Float
  | Bool _ -> Tc.Bool
  | Temp t -> Temp.to_type t
  | IndexOp (t, _) -> (match Temp.to_type t with
                       | Tc.Array typ -> typ
                       | _ -> failwith "Oops.")
  | Dim _ -> Tc.Int

type primitive =
  | Log2 of operand
  | Max of operand * operand
  | Min of operand * operand
  | F2I of operand
  | I2F of operand
  [@@deriving sexp]

(* Statement with either parallel or sequential semantics. *)
type 'a stmt = (* Type param stands for either par_stmt or seq_stmt *)
  (* _Sequential_ for loop.
   * Sort of like a map.
   *)
  | For of Ir.dest * (Temp.t * Temp.t * array_view) * 'a

  (* Explicitly allocate array_view and put it in memory. *)
  | Run of Ir.dest * array_view

  (* List of statements *)
  | Block of 'a list

  (* dest <- reduce(Operator, operand, array_view) *)
  (* The extra temp is used as a key into the annotated array view
   * buffer_infos map so that it's easier to translate the reduction.
   *)
  | Reduce of Ir.dest * Ir.Operator.t * operand * (Temp.t * array_view)
  | Scan of Ir.dest * Ir.Operator.t * operand * (Temp.t * array_view)
  | Filter_with of Ir.dest * (Temp.t * array_view) * (Temp.t * array_view)
  | Nop
  [@@deriving sexp]

(** Parallel statement *)
type par_stmt =
  (* The list of (temp, array_view) pairs is like a nested loop.
   * We just need to bring all parallelism out to the top level.
   * Id.t is the unique id of the parallel block that allows us to
   * use it as key into a map. *)
  | Parallel of Ir.dest * Id.t * (Temp.t * Temp.t * array_view) list * seq_stmt

  (* Wrapper for stmt *)
  | Par_stmt of par_stmt stmt (* ---> Par_stmt (Reduce (...))
                                      Par_stmt (Run (...)) *)

  (* Allows you to have a sequential statement at the top-level of your program. *)
  (* This does NOT run in parallel. *)
  | Seq of seq_stmt
  [@@deriving sexp]

and seq_stmt =
  (* Wrapper for stmt *)
  | Seq_stmt of seq_stmt stmt (* --> Seq_stmt (Reduce (...)) *)
  | Binop of Ir.dest * Ast.binop * operand * operand
  | Unop of Ir.dest * Ast.unop * operand
  | Primitive of Ir.dest * primitive
  | Struct_Init of Ir.dest * Tc.typ * (Ast.ident * operand) list
  | Index of Ir.dest * operand * operand
  | Access of Ir.dest * operand * Ast.ident
  | Assign of Ir.dest * operand
  [@@deriving sexp]

type t = {
  return_type : Tc.typ;
  params : Temp.t list;
  body : par_stmt;
  fn_name : string;
} [@@deriving sexp]

module Pretty_print : sig
  val pp_operand : operand -> string
  val pp_array_view : array_view -> string
  val pp_par_stmt : par_stmt -> string
  val pp_seq_stmt : seq_stmt -> string
  val pp_stmt : prefix:string -> ('a -> string) -> 'a stmt -> string
  val pp_t : t -> string
end = struct

  let rec pp_array_view (_, av) = pp_array_view' av
  and pp_array_view' = function
    | Array_index (t1, t2) -> sprintf "%%%d[%%%d]" (Temp.to_int t1) (Temp.to_int t2)
    | Array t -> sprintf "%%%d" (Temp.to_int t)
    | Zip_with (o, avs) ->
        sprintf "zip_with(%s, %s)"
          (Sexp.to_string_hum (Ir.Operator.sexp_of_t o))
          (String.concat ~sep:", " (List.map avs ~f:pp_array_view))
    | Reverse av -> sprintf "reverse(%s)" (pp_array_view av)
    | Tabulate (b, e, s) -> sprintf "tabulate(%%%d, %%%d, %%%d)"
        (Temp.to_int b) (Temp.to_int e) (Temp.to_int s)
    | Transpose av -> sprintf "transpose(%s)" (pp_array_view av)

  let pp_operand = function
    | Const c -> Int32.to_string_hum c
    | Float f -> string_of_float f
    | Bool b -> string_of_bool b
    | Temp t -> sprintf "%%%d" (Temp.to_int t)
    | IndexOp (a, i) -> sprintf "%%%d[%%%d]" (Temp.to_int a) (Temp.to_int i)
    | Dim (i, av) -> sprintf "dim%d(%s)" i (pp_array_view av)

  let pp_dest = function
    | Ir.Dest t -> sprintf "%%%d" (Temp.to_int t)
    | Ir.Return _ -> "ret"

  let rec pp_par_stmt ?(indent="") = function
    | Parallel (dst, id, tavs, seq_stmt) ->
        sprintf "%s%s <- parallel[%d](%s) {\n%s\n%s}"
          indent
          (pp_dest dst)
          (Id.to_int id)
          (String.concat ~sep:","
             (List.map tavs ~f:(fun (t, t_idx, av) -> sprintf "(%%%d, %%%d) <- %s"
               (Temp.to_int t) (Temp.to_int t_idx) (pp_array_view av))))
          (pp_seq_stmt ~indent:(indent ^ "  ") seq_stmt)
          indent
    | Par_stmt par_stmt ->
        pp_stmt ~prefix:"p" ~indent (pp_par_stmt ~indent:(indent ^ "  ")) par_stmt
    | Seq seq_stmt -> pp_seq_stmt ~indent seq_stmt
  and pp_seq_stmt ?(indent="") = function
    | Seq_stmt seq_stmt ->
        pp_stmt ~prefix:"s" ~indent (pp_seq_stmt ~indent:(indent ^ "  ")) seq_stmt
    | Binop (dst, binop, src1, src2) -> sprintf "%s%s <- %s %s %s" indent (pp_dest dst)
        (pp_operand src1)
        (Sexp.to_string_hum (Ast.sexp_of_binop binop))
        (pp_operand src2)
    | Unop (dst, unop, src) -> sprintf "%s%s <- %s%s" indent (pp_dest dst)
        (Sexp.to_string_hum (Ast.sexp_of_unop unop))
        (pp_operand src)
    | Index (dst,src,expr) -> 
        sprintf "%s%s <- %s[%s]" indent (pp_dest dst) (pp_operand src) (pp_operand expr)
    | Struct_Init (dst,_,flx) ->
        let fields = String.concat ~sep:"; " (List.map flx ~f:(fun (n,o) -> n^" = "^pp_operand o)) in
        sprintf "%s%s <- { %s }" indent (pp_dest dst) (fields)
    | Access (dest,strc,fld) -> 
        sprintf "%s%s <- %s.%s" indent (pp_dest dest) (pp_operand strc) (fld)
    | Assign (dst, src) -> sprintf "%s%s <- %s" indent (pp_dest dst)
        (pp_operand src)
    | Primitive (dst,src) -> sprintf "%s%s <- %s" indent (pp_dest dst) 
        (pp_primitive src)
  and pp_stmt : type t. ?indent:string -> prefix:string -> (t -> string) -> t stmt -> string =
    fun ?(indent="") ~prefix pp -> function
    | Nop -> indent ^ prefix ^ "nop"
    | Block stmts -> String.concat ~sep:"\n" (List.map ~f:pp stmts)
    | Run (dst, av) -> sprintf "%s%s <- %srun(%s)" indent (pp_dest dst) prefix (pp_array_view av)
    | For (dst, (t, t_idx, av), stmt) ->
        sprintf "%s%s <- %sfor ((%%%d, %%%d) <- %s) {\n%s\n%s}" indent (pp_dest dst) prefix
        (Temp.to_int t) (Temp.to_int t_idx) (pp_array_view av)
        (pp stmt)
        indent
    | Scan (dst, op, id, (_, av)) ->
        sprintf "%s%s <- %sscan(%s, %s, %s)" indent (pp_dest dst) prefix
          (Sexp.to_string_hum (Ir.Operator.sexp_of_t op))
          (pp_operand id)
          (pp_array_view av)
    | Reduce (dst, op, id, (_, av)) ->
        sprintf "%s%s <- %sreduce(%s, %s, %s)" indent (pp_dest dst) prefix
          (Sexp.to_string_hum (Ir.Operator.sexp_of_t op))
          (pp_operand id)
          (pp_array_view av)
    | Filter_with (dst, (_, av1), (_, av2)) ->
        sprintf "%s%s <- filter_with(%s, %s)" indent (pp_dest dst) (pp_array_view av1) (pp_array_view av2)
  and pp_primitive = function
    | Log2 a -> sprintf "log2(%s)" (pp_operand a)
    | Max (a,b) -> sprintf "max(%s, %s)" (pp_operand a) (pp_operand b)
    | Min (a,b) -> sprintf "min(%s, %s)" (pp_operand a) (pp_operand b)
    | F2I (a) -> sprintf "(int)(%s)" (pp_operand a)
    | I2F (a) -> sprintf "(float)(%s)" (pp_operand a)


  let pp_t { params; body; } =
    sprintf "(%s) {\n%s\n}"
      (String.concat ~sep:", " (List.map params ~f:(fun (p) -> "%" ^ string_of_int (Temp.to_int p))))
      (pp_par_stmt ~indent:"  " body)

  let pp_par_stmt = pp_par_stmt ?indent:None
  let pp_seq_stmt = pp_seq_stmt ?indent:None
  let pp_stmt ~prefix f stmt = pp_stmt ?indent:None ~prefix f stmt
end
