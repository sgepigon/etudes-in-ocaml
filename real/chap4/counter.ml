
open Core.Std

type t = (string * int) list
let empty = []
let to_list x = x

let touch t s =
  let count =
    match List.Assoc.find t s with
    | None -> 0
    | Some x -> x
  in
  List.Assoc.add t s (count + 1)


(* Illegal cyclic self-dependency
   let singleton l = counter.touch Counter.empty
*)

(* Illegal cyclic dependency, caught by the compiler
   let _build_coutns = Freq.build_counts
*)

