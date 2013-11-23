
open Core.Std

type t = int String.Map.t
let empty = String.Map.empty
let to_list t = Map.to_alist t

let touch t s =
  let count =
    match Map.find t s with
    | None -> 0
    | Some x -> x
  in
  Map.add t ~key:s ~data:(count + 1)

(** Represents the median computer from a set of strings.
    In the case where there is an even number of choices, 
    the one just low and one just high of the median is returned. *)
(* Note that this order matters; 
   Lo_hi above Median breaks the compiler. 
   Same goes for type declarations and record fields and arguments. *)
type median = | Median of string
	      | Lo_hi of string * string



let median t =
  let sorted_strings = List.sort (Map.to_alist t)
    ~cmp:(fun (_,x) (_,y) -> Int.descending x y)
  in
  let len = List.length sorted_strings in
  if len = 0 then failwith "median: empty frequency count";
  let nth n = fst (List.nth_exn sorted_strings n) in
  if len mod 2 = 1
  then Median (nth (len/2))
  else Lo_hi (nth (len/2 - 1), nth (len/2))
;;
