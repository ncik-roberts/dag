open Core

module Cuda = Cuda_ir

type t = {

  (** Max dimensions we're looping over in the main function. *)
  nested_loops : int;

  (* Number of total transfer instructions, where the first element
   * of the tuple is the number of nested levels of loops. (This
   * list is reverse-sorted in lexicographic order of the tuples
   * so that the smallest array is the one that performs the
   * least number of transfers at the least levels of nesting.
   *)
  transfers : (int * int) list;

  (* Memory allocation counts the total number of
   * multiplicands in calls to cudaMalloc.
   * If we perform: cudaMalloc(&x, i1 * i2 * i3 * sizeof(int)),
   * this would contribute 3 to memory_allocation (because there
   * are three multiplicands: i1, i2, i3).
   *)
  memory_allocation : int;

} [@@deriving sexp, compare]

let empty : t = {
  memory_allocation = 0;
  transfers = [];
  nested_loops = 0;
}

type 'a binop = 'a -> 'a -> 'a

let rec merge_transfers : (int * int) list binop = fun xs1 xs2 ->
  match xs1, xs2 with
  | [], [] -> []
  | ys, [] -> ys
  | [], ys -> ys
  | (i1, qt1) :: ys1, (i2, qt2) :: ys2 ->
      match compare_int i1 i2 with
      | c when c < 0 -> (i2, qt2) :: merge_transfers xs1 ys2
      | c when c > 0 -> (i1, qt1) :: merge_transfers ys1 xs2
      | _ -> (i1, qt1 + qt2) :: merge_transfers ys1 ys2

let merge : t binop = fun t1 t2 -> {
  memory_allocation = t1.memory_allocation + t2.memory_allocation;
  transfers = merge_transfers t1.transfers t2.transfers;
  nested_loops = max t1.nested_loops t2.nested_loops;
}

let rec count_multiplicands = function
  | Cuda.Binop (Cuda.MUL, e1, e2) -> count_multiplicands e1 + count_multiplicands e2
  | Cuda.Size_of _ -> 0
  | _ -> 1

let rec count_multiplicands_in_loop_guard = function
  | Cuda.Binop (Cuda.LT, _lhs, rhs) -> count_multiplicands rhs
  | _ -> 1

let rec convert_stmt : Cuda.cuda_stmt -> t =
  let open Cuda in
  function
    | Nop | Sync | DeclareArray _ | Return _ | InitStruct _ | Assign _ | AssignOp _ | Expression _
    | Launch _ | Declare _ | DeclareAssign _ | Malloc _ | Free _  | Memcpy _ -> empty
    | ThrustCall ("reduce", _, [_;_;_;length;]) ->
        let n = count_multiplicands length in
        { nested_loops = 0;
          memory_allocation = n;
          transfers = [(n, 1)];
        }
    | ThrustCall ("scan", _, [_;_;_;length;]) ->
        let n = count_multiplicands length in
        { nested_loops = 0;
          memory_allocation = 2 * n;
          transfers = [(n, 2)];
        }
    | ThrustCall _ -> failwith "That's it."
    | Cuda_malloc (_, _, expr) ->
        { empty with memory_allocation = count_multiplicands expr }
    | Transfer (_, _, size, _) -> { empty with transfers = [ (count_multiplicands size, 1) ] }
    | Loop ((_, loop_guard, _), body) ->
        let t = convert_stmts body in
        let n = count_multiplicands_in_loop_guard loop_guard in
        { memory_allocation = n * t.memory_allocation;
          nested_loops = t.nested_loops * n;
          transfers = List.map ~f:(Tuple2.map_fst ~f:((+) n)) t.transfers;
        }
    | Condition (_, body1, body2) ->
        let t1, t2 = convert_stmts body1, convert_stmts body2 in
        if compare t1 t2 < 0 then t2 else t1

and convert_stmts : Cuda.cuda_stmt list -> t = fun stmts ->
  stmts
    |> List.map ~f:convert_stmt
    |> List.reduce ~f:merge
    |> Option.value ~default:empty

let into f = convert_stmts Cuda.(f.body)
