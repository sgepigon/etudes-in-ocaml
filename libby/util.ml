
open Printf;;

(* Utilities library *)

(** Pretty print **)

let endl = (fun _ -> printf ";;\n")
let printcl xs = printf "["; List.iter (printf " %c") xs; printf " ]";;
let printcln xs = printcl xs; endl ();;
let printil xs = printf "["; List.iter (printf " %d") xs; printf " ]";;
let printiln xs = printil xs; endl ();;

(** Testing **)

let check p = (if p then "OK" else "FAIL");;

let test name call inA outA = 
  printf "%s test ... %s\n" name (check (call inA = outA ));;
let test2 name call inA inB outA = 
  printf "%s test ... %s\n" name (check (call inA inB = outA ));;
let test3 name call inA inB inC outA = 
  printf "%s test ... %s\n" name (check (call inA inB inC = outA ));;
let test4 name call inA inB inC inD outA = 
  printf "%s test ... %s\n" name (check (call inA inB inC inD = outA ));;

let testu name call inA outA outB =
  call inA;
  printf "%s test ... %s\n" name (check (!outA = outB ));;
let testu2 name call inA inB outA outB =
  call inA inB;
  printf "%s test ... %s\n" name (check (!outA = outB ));;
let testu3 name call inA inB inC outA outB =
  call inA inB inC;
  printf "%s test ... %s\n" name (check (!outA = outB ));;


(** from 4.00 **)

let iteri f xs =
  let rec it f i = function
    | [] -> ()
    | y::ys -> (f i y); (it f (i+1) ys)
  in
  it f 0 xs
;;

let mapi f xs = 
  let rec it f i = function
    | [] -> []
    | y::ys -> (f i y) :: (it f (i+1) ys)
  in
  it f 0 xs
;;
