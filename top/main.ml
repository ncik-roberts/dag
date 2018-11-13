(** Run toplevel. *)

let () =
  try Top.run ()
  with Failure str ->
    prerr_endline str;
    exit 1
