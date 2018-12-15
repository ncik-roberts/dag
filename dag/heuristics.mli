type t [@@deriving compare]

(* Way of casting a cuda function into something that can be
 * sorted. *)

(* The semantics of comparing t is this:
 * smaller is better.
 * bigger is worse.
 *)

val into : Cuda_ir.cuda_func -> t
