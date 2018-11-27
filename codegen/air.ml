open Core

(** Abstract IR *)

type array_view =
  | Array of Temp.t
  | Zip_with of Ir.Operator.t * array_view list
  | Reverse of array_view
  | Transpose of array_view
  [@@deriving sexp]

type operand =
  | Const of Int32.t
  | Temp of Temp.t
  | Dim of int * array_view (* Nth dimension of an array *)
  [@@deriving sexp]

(* Statement with either parallel or sequential semantics. *)
type 'a stmt = (* Type param stands for either par_stmt or seq_stmt *)
  (* _Sequential_ for loop.
   * Sort of like a map.
   *)
  | For of Ir.dest * (Temp.t * array_view) * 'a

  (* Explicitly allocate array_view and put it in memory. *)
  | Run of Ir.dest * array_view

  (* List of statements *)
  | Block of 'a list

  (* dest <- reduce(Operator, operand, array_view) *)
  | Reduce of Ir.dest * Ir.Operator.t * operand * array_view
  | Nop
  [@@deriving sexp]

(** Parallel statement *)
type par_stmt =
  (* The list of (temp, array_view) pairs is like a nested loop.
   * We just need to bring all parallelism out to the top level.
   * Id.t is the unique id of the parallel block that allows us to
   * use it as key into a map.
   *)
  | Parallel of Ir.dest * Id.t * (Temp.t * array_view) list * seq_stmt

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
  | Assign of Ir.dest * operand
  [@@deriving sexp]

type t = {
  params : Temp.t list;
  body : par_stmt;
} [@@deriving sexp]

module Pretty_print : sig
  val pp_operand : operand -> string
  val pp_array_view : array_view -> string
  val pp_par_stmt : par_stmt -> string
  val pp_seq_stmt : seq_stmt -> string
  val pp_stmt : prefix:string -> ('a -> string) -> 'a stmt -> string
  val pp_t : t -> string
end = struct
  let rec pp_array_view = function
    | Array t -> Printf.sprintf "%%%d" (Temp.to_int t)
    | Zip_with (o, avs) ->
        Printf.sprintf "zip_with(%s, %s)"
          (Sexp.to_string_hum (Ir.Operator.sexp_of_t o))
          (String.concat ~sep:", " (List.map avs ~f:pp_array_view))
    | Reverse av -> Printf.sprintf "reverse(%s)" (pp_array_view av)
    | Transpose av -> Printf.sprintf "transpose(%s)" (pp_array_view av)

  let rec pp_operand = function
    | Const c -> Int32.to_string_hum c
    | Temp t -> Printf.sprintf "%%%d" (Temp.to_int t)
    | Dim (i, av) -> Printf.sprintf "dim%d(%s)" i (pp_array_view av)

  let pp_dest = function
    | Ir.Dest t -> Printf.sprintf "%%%d" (Temp.to_int t)
    | Ir.Return _ -> "ret"

  let rec pp_par_stmt ?(indent="") = function
    | Parallel (dst, id, tavs, seq_stmt) ->
        Printf.sprintf "%s%s <- parallel[%d](%s) {\n%s\n%s}"
          indent
          (pp_dest dst)
          (Id.to_int id)
          (String.concat ~sep:","
             (List.map tavs ~f:(fun (t, av) -> Printf.sprintf "%%%d <- %s" (Temp.to_int t) (pp_array_view av))))
          (pp_seq_stmt ~indent:(indent ^ "  ") seq_stmt)
          indent
    | Par_stmt par_stmt ->
        pp_stmt ~prefix:"p" ~indent (pp_par_stmt ~indent:(indent ^ "  ")) par_stmt
    | Seq seq_stmt -> pp_seq_stmt ~indent seq_stmt
  and pp_seq_stmt ?(indent="") = function
    | Seq_stmt seq_stmt ->
        pp_stmt ~prefix:"s" ~indent (pp_seq_stmt ~indent:(indent ^ "  ")) seq_stmt
    | Binop (dst, binop, src1, src2) -> Printf.sprintf "%s%s <- %s %s %s" indent (pp_dest dst)
        (pp_operand src1)
        (Sexp.to_string_hum (Ast.sexp_of_binop binop))
        (pp_operand src2)
    | Unop (dst, unop, src) -> Printf.sprintf "%s%s <- %s%s" indent (pp_dest dst)
        (Sexp.to_string_hum (Ast.sexp_of_unop unop))
        (pp_operand src)
    | Assign (dst, src) -> Printf.sprintf "%s%s <- %s" indent (pp_dest dst)
        (pp_operand src)
  and pp_stmt : type t. ?indent:string -> prefix:string -> (t -> string) -> t stmt -> string =
    fun ?(indent="") ~prefix pp -> function
    | Nop -> indent ^ prefix ^ "nop"
    | Block stmts -> String.concat ~sep:"\n" (List.map ~f:pp stmts)
    | Run (dst, av) -> Printf.sprintf "%s%s <- %srun(%s)" indent (pp_dest dst) prefix (pp_array_view av)
    | For (dst, (t, av), stmt) ->
        Printf.sprintf "%s%s <- %sfor (%%%d <- %s) {\n%s\n%s}" indent (pp_dest dst) prefix (Temp.to_int t) (pp_array_view av)
        (pp stmt)
        indent
    | Reduce (dst, op, id, av) ->
        Printf.sprintf "%s%s <- %sreduce(%s, %s, %s)" indent (pp_dest dst) prefix
          (Sexp.to_string_hum (Ir.Operator.sexp_of_t op))
          (pp_operand id)
          (pp_array_view av)

  let pp_t { params; body; } =
    Printf.sprintf "(%s) {\n%s\n}"
      (String.concat ~sep:", " (List.map params ~f:(fun p -> "%" ^ string_of_int (Temp.to_int p))))
      (pp_par_stmt ~indent:"  " body)

  let pp_par_stmt = pp_par_stmt ?indent:None
  let pp_seq_stmt = pp_seq_stmt ?indent:None
  let pp_stmt ~prefix f stmt = pp_stmt ?indent:None ~prefix f stmt
end
