
open Core.Std;;

let reverse original =
  let rec rev acc = function
    | [] -> acc
    | hd::tl -> rev (hd::acc) tl
  in
  rev [] original
;;
  
let () =
  let a = [1;2;3;4;5;6] in
  let ra = List.rev a in
  let rb = reverse a in
  
  let print = List.iter ~f:(fun e -> printf "%d " e) in
  
  print a; print_endline "";
  print ra; print_endline "";
  print rb; print_endline "";
;;
