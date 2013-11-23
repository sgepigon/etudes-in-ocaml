
open Core.Std
open Common

let main = 
  let l = List.intersperse [1;2;3;4;5] 42 in
  
  List.iter l ~f:(printf "%d ");
  printf "\n"
;;
  

  
