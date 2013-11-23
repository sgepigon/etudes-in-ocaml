
open Core.Std;;

let decorate str =
  let bar = "---\n"in 

  print_string bar;
  print_string str;
  print_string bar;
;;

(* Variables *)
let run_vars =
  let langs = "Ocaml,Perl,C++,C" in
  let dashed = 
    let langs = String.split langs ~on:',' in
    String.concat ~sep:"-" langs
  in

  decorate (sprintf " %s %s\n" langs dashed);

  let area_of_ring inner outer =
    let pi = acos (-1.) in
    let area_of_circle r = pi *. r *. r in
    area_of_circle outer -. area_of_circle inner
  in

  decorate (sprintf " area_of_ring 1. 3. = %F\n" (area_of_ring 1. 3.));  
;;


(* Pattern matching and let *)
let run_pattmatch =
  let base = "abc def ghi" in
  let upcase_first_entry line =
    match String.split ~on:' ' line with
    | [] -> assert false
    | first :: rest -> String.concat ~sep:" " (String.uppercase first :: rest)
  in

  decorate (sprintf " %s --> %s\n" base (upcase_first_entry base));
;;


(* Anonymous functions *)
let run_anonfunc =
  let incs = [ (fun x -> x + 1); (fun x -> x + 2) ] in
  let apps = List.map ~f:(fun g -> string_of_int (g 5)) incs in
  
  decorate (" " ^ (String.concat ~sep:" " apps) ^ "\n")
;;


(* Multi-argument functions *)
let run_multiarg =
  let abs_diff = (fun x -> (fun y -> abs (x - y))) in
  let dist_from_three = abs_diff 3 in                     (* curry *) 
  let abs_diff_tup (x,y) = abs (x - y) in
  
  decorate (sprintf " 5 from three @ %d, 8 from 9 @ %d\n" (dist_from_three 5) (abs_diff_tup (8,9)));
;;
    

(* Recursive functions *)
let run_rec =
  let rec is_even x = if x = 0 then true else is_odd (x - 1)
  and is_odd x = if x = 0 then false else is_even (x - 1) in

  let nums = [0;1;2;3;4;5] in
  let evals = 
    match (List.zip nums (List.map ~f:is_odd nums)) with
    | None -> []
    | Some arr -> arr
  in
  
  let toString (i,b) = "(" ^ (string_of_int i) ^ ", " ^ (string_of_bool b) ^ ")" in
  let toSentence = String.concat (List.map ~f:toString evals) ~sep:"; " in

  decorate (" Odditites: " ^ toSentence ^ "\n")
;;

(* Prefix and infix operators *)
let run_fixes =
  let (+!) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2) in
  let toString (a,b) = "(" ^ (string_of_int a) ^ ", " ^ (string_of_int b) ^ ")" in
  
  decorate(" (3,2) +! (-2,4) == " ^ toString ((3,2) +! (-2,4)) ^ "\n");
  
  let (|>) x f = f x in
  let path = "/usr/bin:/usr/local/bin:/bin:/sbin" in

  String.split ~on:':' path
  |> List.dedup ~compare:String.compare
  |> List.iter ~f:print_endline
;;

(* Declaring functions within function *)
let run_internal = 
  let some_or_zero = function
    | Some x -> x
    | None -> 0
  in

  decorate (sprintf " Zero is not quite %d.\n" (some_or_zero (Some 42)))
;;

(* Labeled arguments *)
let run_labeled =
  let ratio ~num ~denom = float num /. float denom in
  
  let first = ratio ~num:3 ~denom:10
  and second = ratio ~denom:10 ~num:3 in

  decorate (sprintf " %F == %F \n" first second)
;;

(* Higher-order functions and labels *)
(** Omit **)

(* Optional arguments *)
let run_optional =
  let concat ?sep x y =
    let sep = match sep with 
      | None -> "" 
      | Some x -> x 
    in
    x ^ sep ^ y
  in
  
  let one = (concat "foo" "bar") 
  and two = (concat "foo" "bar" ~sep:":") in  
  
  decorate (sprintf " '%s' isn't quite '%s'\n" one two);

  (* Explicit passing of an optional argument *)

  let uppercase_concat ?sep a b =
    concat ?sep (String.uppercase a) b 
  in

  let three = (uppercase_concat "foo" "bar") 
  and four = (uppercase_concat "foo" "bar" ~sep:":") in  

  decorate (sprintf " '%s' isn't quite '%s'\n" three four);    

;;

(* Inference of labeled and optional arguments *)
(* Optional arguments and partial application *)
(** Omit **)

