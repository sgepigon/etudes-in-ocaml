
(* Code CracklePop *)

(* 
   Write a program that prints out the numbers 1 to 100 (inclusive). If the
   number is divisible by 3, print Crackle instead of the number. If it's
   divisible by 5, print Pop. If it's divisible by both 3 and 5, print
   CracklePop. You can use any language. 
*)

(* 
   >  corebuild cpop.byte; ./cpop.byte 
*)

open Core.Std;;

let cpop =
  let id = (fun a -> a) in
  let cpop_str a = 
    let divs_by_three = a % 3 = 0 
    and divs_by_five = a % 5 = 0 in
    let crackle = if divs_by_three then "Crackle" else ""
    and pop = if divs_by_five then "Pop" else "" 
    and num = if divs_by_five || divs_by_three then "" else (string_of_int a)
    in
    printf "%s%s%s\n" crackle pop num
  in
  let rec cpopR i f =
    if i > 100 then f () else cpopR (i+1) (fun _ -> f (); cpop_str i)
  in
  cpopR 1 id;
;;

let () =
  cpop;
;;
