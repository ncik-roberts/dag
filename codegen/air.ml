open Core

(** Abstract IR *)

module Op = struct
  type t =
    | Binop of Ast.binop
    | Unop of Ast.unop
    | Fun_ptr of Ast.ident
end

type array_view =
  | Array of Temp.t * Temp.t  (* Pointer, Size *)
  | Zip_with of Op.t * array_view list
  | Reverse of array_view
  | Transpose of array_view

type operand =
  | Const of Int32.t
  | Temp of Temp.t (* This temp is the only source temp *)

(** Parallel statement *)
type par_stmt =
  (* All temps here are destinations. *)
  | Par_for of (Temp.t * array_view) list * seq_stmt list
  | Seq_for of (Temp.t * array_view) * par_stmt list
  | Run of Temp.t * array_view
  | Seq of seq_stmt

and seq_stmt =
  | Binop of Temp.t * Ast.binop * operand * operand
  | Unop of Temp.t * Ast.unop * operand
  | Assign of Temp.t * operand

type param =
  | Array_param of {
      param : param;
      length : Temp.t;
    }
  | Temp_param of Temp.t * Ast.typ

type t = {
  params : param list;
  body : par_stmt list;
}
