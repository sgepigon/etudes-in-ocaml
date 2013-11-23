
open Core.Std;;
open Core_kernel.Quickcheck;;
module QC = Core_kernel.Quickcheck;;
open Batteries.Char;;
open Batteries.Array;;

let alpha = BatEnum.fold (fun xs x -> (sprintf "%c" x) :: xs) [] (' ' -- 'z');;

let charCounts s =
  let counts = Array.create ~len:(1 + List.length alpha) 0 in
  let addCount c =
    let i = (code c) - 32 in
    counts.(i) <- 1 + counts.(i);
  in
  BatString.iter addCount s;
  counts
;;

let charRemover s arr =
  let counts = Array.copy arr in
  let takeCount c =
    let i = (code c) - 32 in
    counts.(i) <- counts.(i) - 1;
  in
  BatString.iter takeCount s;
  counts
;;

let isAnagram a b = 
  let counts = charRemover b (charCounts a) in  
  Batteries.Array.iter (fun x -> printf "%d" x) counts; print_newline ();
  let errs = Batteries.Array.fold_left 
    (fun s x -> s + (if x = 0 then 0 else 1)) 0 counts in  
  errs = 0
;;

let () =
  (* Tests *)
  (* print_string (String.concat alpha); *)
  let str1 = "hello world"
  and str2 = "hello dorld"
  and str3 = "wello horld" in
  let tryAnagrams s t =
    let res = isAnagram s t in
    printf " '%s' %ca= '%s'\n" s (if res then '=' else '!') t;
  in

  tryAnagrams str1 str2;
  tryAnagrams str1 str3

;;
  
  
