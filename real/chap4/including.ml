
open Core.Std

module Interval = struct 
  type t = | Interval of int * int
	   | Empty

  let create low high =
    if high < low then Empty else Interval (low, high)
end;;

module Ext_interval = struct 
  include Interval  (* Note that 'open' would give completely different behavior *)

  let contains t x =
    match t with
    | Empty -> false
    | Interval (low,high) -> x >= low && x <= high
end;;

let main =
  printf "\n 4 in 3..10: %b \n\n" (Ext_interval.contains (Ext_interval.create 3 10) 4)
;;
