
open Core.Std;;
open Batteries;;

(* swap  *)
(* let tmp = array.(i) in *)
(* array.(i) <- array.(j); *)
(* array.(j) <- tmp *)

(* let qsort arr = *)
(*   let l = Array.length !arr in *)
(*   let rec qsortR a i j = *)
(*     let p = a.(i) in *)
(*     if i >= j then () *)
(*     else begin *)
(*       for iter = (i+1) to j do *)
	
(*       done; *)
(*     end *)

(*   qsortR arr 0 (l-1) *)
(* ;; *)

(* let qsort arr =  *)
(*   let larr = BatArray.to_list !arr in *)
(*   let lesseq e = List.filter ~f(fun x -> x <= e) in *)
(*   let more e = List.filter ~f(fun x -> x > e) in *)
(*   let rec qsortR arrR = *)
(*     let head = List.hd arrR in *)
(*     let tail = List.tl arrR in *)
(*     match arrR with  *)
(*       |  *)
(*     (lesseq e arrR)  *)


let () =
  let maxlen = 25 in
  let arr = ref (BatArray.make maxlen 0) in
  
  for i = 0 to maxlen-1 do
    !arr.(i) <- Random.int maxlen;
  done;

  let print_arr a = 
    BatArray.iter (printf "%d ") a; printf "\n";
  in

  print_arr !arr;
  
  (* qsort arr *)

  print_arr !arr;
;;
