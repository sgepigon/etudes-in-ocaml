
open Core.Std;;
open Myutil;;

(* Utils *)
let endl = (fun _ -> printf ";;\n");;
let printcl xs = printf "["; List.iter ~f:(printf " %c") xs; printf " ]";;
let printcln xs = printcl xs; endl ();;
let printil xs = printf "["; List.iter ~f:(printf " %d") xs; printf " ]";;
let printiln xs = printil xs; endl ();;

(* Trivialities *)

let rec last = function
  | [] -> None
  | x::[] -> Some x
  | _::l -> last l
;;
  
let rec last_two = function
  | [] | [_] -> None
  | [a;b] -> Some (a,b)
  | _::xs -> last_two xs
;;

let rec at i xs = match xs with
  | [] -> None
  | y::ys -> if (i=1) then Some y else at (i-1) ys
;;

let rec remove_at i = function
    | [] -> failwith "nth"
    | y::ys -> if (i=0) then ys else y::(remove_at (i-1) ys)
;;

let rec length = function
  | [] -> 0
  | _::xs -> 1 + (length xs)
;;

let rec is_palindrome2 xs ys = match (xs,ys) with
  | ([], []) -> true
  | ([], _) | (_, []) -> false
  | (x::xx, y::yy) -> if (x = y) then (is_palindrome2 xx yy) else false
;;

let is_palindrome xs = is_palindrome2 (reverse xs) xs ;;

(* More challenge *)

type 'a tree_t = | Leaf of 'a | Node of 'a tree_t list;;

let rec flatten = function (* accumulator func would be more efficient *)
  | [] -> []
  | (Leaf t)::tt -> [t] @ (flatten tt)
  | (Node t)::tt -> (flatten t) @ (flatten tt)
;;

let compress xs = 
  let rec press c = function (* accumulator func would make more sense *)
    | [] -> []
    | z::zs -> (if ((not (c = None)) && ((Some z) = c)) then [] else [z]) @ (press (Some z) zs)
  in press None xs;
;;

type 'a rle_t = | One of 'a | Many of (int * 'a);;

let encode =
  let accumulate acc c = function
    | 1 -> (One c)::acc
    | i -> (Many (i, c))::acc in      
  let rec build acc c i = function   
    | [] -> (accumulate acc c i)               (* list done *)
    | y::ys -> match (y = c) with
	| true -> build acc c (i+1) ys                (* another, continue *)
	| false -> build (accumulate acc c i) y 1 ys; (* a new one *)	      
  in function 
    | [] -> []
    | x::xs -> reverse (build [] x 1 xs);
;;

let replicate xx i= 
  let rec rep c = function
    | 0 -> []
    | x -> (c)::(rep c (x-1)) 
  in 
  let rec acc i = function
    | [] -> []
    | y::yy -> (rep y i) @ (acc i yy)
  in 
  acc i xx
;;

let slice xs i k =
  let sane xs i k = ((length xs) > 0) && (i <= k) && (k < (length xs)) in

  let rec sliceparse j l xs = begin
    if (j > 0) then (sliceparse (j-1) (l-1) (List.tl_exn xs)) else
	if (j < 0) then failwith "j >= 0 broke" else
	  if (l >= 0) then (List.hd_exn xs)::(sliceparse j (l-1) (List.tl_exn xs)) else [];
  end in
  if (sane xs i k) then (sliceparse i k xs) else failwith "insane"
;;

let rand_select xs i = 
  let l = length xs in
  let rec rep xs = function
    | 0 -> []
    | x -> (List.nth_exn xs (Random.int l))::(rep xs (x-1))
  in
  rep xs i
;;    

(* int -> 'a list -> 'a list list *)
let extract k list =
  let rec aux k acc emit = function
    | [] -> acc
    | h :: t ->
        if k = 1 then aux k (emit [h] acc) emit t else
          let new_emit x = emit (h :: x) in
          aux k (aux (k-1) acc new_emit t) emit t
  in
  let emit x acc = x :: acc in
  aux k [] emit list
;;

(** MAIN **)
    
let main () =   
  let check p = (if p then "OK" else "FAIL") in

  let in1 = ['a'; 'b'; 'c'; 'd'; 'e'] 
  and in2 = ['a']
  and in3 = [] in
  printf "Last element test A ... %s\n"  (check (last in1     = Some 'e'        ));
  printf "Last element test B ... %s\n"  (check (last in3     = None            ));
  printf "(Pen)ultimate test A ... %s\n" (check (last_two in1 = Some ('d', 'e') ));
  printf "(Pen)ultimate test A ... %s\n" (check (last_two in2 = None            ));
  printf "kth element test A ... %s\n"   (check (at 3 in1     = (Some 'c')      ));
  printf "kth element test B ... %s\n"   (check (at 3 in2     = None            ));

  printf "length test A ... %s\n" (check (length in1 = 5));
  printf "length test A ... %s\n" (check (length in3 = 0));

  let in4 = ['e'; 'd'; 'c'; 'b'; 'a']
  and in5 = ['a'; 'b'; 'c'; 'b'; 'a'] in
  printf "reverse test A ... %s\n" (check (reverse in1 = in4));
  printf "reverse test A ... %s\n" (check (reverse in3 = in3));
  printf "is_palindrome test A ... %s\n" (check (is_palindrome in5));
  printf "is_palindrome test A ... %s\n" (check (not (is_palindrome in1)));
  
  let in6 = [ Leaf 'a' ; Node [ Leaf 'b' ; Node [ Leaf 'c' ; Leaf 'd' ] ; Leaf 'e' ] ] in
  printf "flatten test A ... %s\n" (check (flatten in6 = in1));

  let in7 = ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'd'; 'e'; 'e'; 'e'; 'e'] in
  printf "compress test A ... %s\n" (check (compress in7 = in1));

  let in8 = ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e'] 
  and out8 = [Many (4,'a') ; One 'b' ; Many (2,'c') ; Many (2,'a') ; One 'd' ; Many (4,'e')] in
  printf "encode test A ... %s\n" (check (encode in8 = out8));

  let out9 = ['a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'c'; 'c'; 'c'; 'd'; 'd'; 'd'; 'e'; 'e'; 'e'] in
  printf "replicate test A ... %s\n" (check (replicate in1 3 = out9));
  
  let in10 = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j']
  and out10 = ['c'; 'd'; 'e'; 'f'; 'g'] in
  let res = (slice in10 2 6) in
  printf "slice test ... %s\n" (check (res = out10));
        
  let in11 = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h']
  and out11 = ['g'; 'f'; 'a'] in
  let res = (rand_select in11 3) in  
  printf "rand_select test ... %s\n" (check (res = out11));

  let in12 = ['a'; 'b'; 'c'; 'd']
  and out12 = ['a'; 'c'; 'd'] in
  let res = (remove_at 1 in12) in
  printf "remove_at test ... %s\n" (check (res = out12));
    
  let in_ = ['a'; 'b'; 'c'; 'd']
  and out_ = [['c'; 'd']; ['b'; 'd']; ['b'; 'c']; ['a'; 'd']; ['a'; 'c']; ['a'; 'b']] in
  let res = (extract 2 in_) in
  printf "extract test ... %s\n" (check (res = out_));
  printf "["; List.iter ~f:(printcl) res; printf "]"; endl();

;;


main ()
  
