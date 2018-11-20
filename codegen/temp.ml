open Core

include Int

let to_int = Fn.id
let counter = ref 0
let next () = incr counter; !counter
