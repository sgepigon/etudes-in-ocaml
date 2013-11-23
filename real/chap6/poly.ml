
open Core.Std;;

let three = `Int 3;;
let four = `Float 4.;;
let nan = `Not_a_number;;
let arr = [three; four; nan];;

let five = `Int "five";;

(*
  # [three; four; five];;
  Characters 14-18: Error: This expression has type [> `Int of string ] 
             but an expression was expected of type [> `Float of float | `Int of int ]
  Types for tag `Int are incompatible 
*)


let is_positive = function
  | `Int   x -> x > 0
  | `Float x -> x > 0.
  ;;
(* 
   val is_positive : 
   [< `Float of float | `Int of int ] -> bool = <fun>
*)

(* We can think of these < and > markers as indications
   of upper and lower bounds on the tags involved. *)


let is_positive_permissive = function
  | `Int   x -> Ok (x > 0)
  | `Float x -> Ok (x > 0.)
  | _ -> Error "Unknown number type"
  ;;
(*
  val is_positive_permissive : 
  [> `Float of float | `Int of int ] -> 
      (bool, string) Result.t = <fun>
*)
(* is_positive_permissive (`Int 0);;
   - : (bool, string) Result.t = Ok false
   is_positive_permissive (`Ratio (3,4));;
   - : (bool, string) Result.t = Error "Unknown number type"  

   BUT TYPOS GO ALSO WITHOUT COMPLAINT

   is_positive_permissive (`Floot 3.5);;
   - : (bool, string) Result.t = Error "Unknown number type" 
*)
