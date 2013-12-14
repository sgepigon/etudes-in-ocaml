
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

let isAnagram a b p = 
  let counts = charRemover b (charCounts a) in  
  if p then begin 
    Batteries.Array.iter (fun x -> printf "%d" x) counts; print_newline ()
  end
  else ();
  
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
    let res = isAnagram s t true in
    printf " '%s' %ca= '%s'\n" s (if res then '=' else '!') t;
  in

  tryAnagrams str1 str2;
  tryAnagrams str1 str3;

  Random.init (Int.of_float (Time.to_float (Time.now ()))); (* seedin' *)

  let alphabet = Batteries.String.concat "" alpha in
  let asciiGen dict = (fun () -> dict.[Random.int (String.length dict)]) in
  let lengthGen max = (fun () -> Random.int max) in
  let maxlen = 20 in
  let stringGen = QC.sg ~char_gen:(asciiGen alphabet) ~size_gen:(lengthGen maxlen) in 

  printf "\n Generating some randy chars: ";
  QC.repeat 10 (printf "%c ") (asciiGen alphabet);
  printf "\n";

  printf "\n Generating some randy strings:\n";
  let shuf s = BatString.of_list (BatArray.to_list (Batteries.Random.shuffle (BatString.enum s))) in
  QC.repeat 10 (fun s -> printf "   %20s -- %s\n" s (shuf s)) stringGen;
  printf "\n";

  printf "Trying out laws.\n";
  let law_try law should_fail f =
    begin
      let on_ok = (if should_fail then "(bad)" else "(ok)") in
      let on_fail = (if should_fail then "(ok)" else "(bad)") in      
      try 
	QC.laws_exn law 10 stringGen f;
	printf "law %s passed %s\n" law on_ok;
      with Failure s -> print_string s; printf " %s\n" on_fail;
    end
  in
  law_try "shuffle" false (fun a -> isAnagram a (shuf a) false);
  law_try "doubling" true (fun a -> isAnagram a (BatString.repeat a 2) false);

;;
  
  
