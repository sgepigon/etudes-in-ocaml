
open Core.Std

(* Remainder of 'list' *)
include List

(* New function to add *)
let rec intersperse list el =
  match list with
    | [] | [_] -> list
    | x :: y :: tl -> x :: el :: intersperse (y::tl) el

