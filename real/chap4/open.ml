
open Core.Std
open Option.Monad_infix  (* Try to only open modules designed to opened *)

(* Local open *)
let average x y =
  let open Int64 in
  x + y / of_int 2;; (* the infix ops are from Int64 as well *)

(* Even more lightweight *)
let avg x y =
  Int64.(x + y / of_inf 2);;

(* Local rebind *)
let print_median m =
  let module C = Counter in (* Doing this very high up isn't so nice *)
  match m with
  | C.Median string -> printf "True median:\n   %s\n" string
  | C.Lo_hi (before,after) -> 
    printf "Before and after median:\n   %s\n   %s\n" before after



