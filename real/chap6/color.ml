
open Core.Std;;

type basecolor =
  | Black | Red | Green | Yellow 
  | Blue | Magenta | Cyan | White
;;

let basecolor_to_int = function
  | Black -> 0 | Red     -> 1 | Green -> 2 | Yellow -> 3
  | Blue  -> 4 | Magenta -> 5 | Cyan  -> 6 | White  -> 7 ;;

let color_by_number number text =
    sprintf "\027[38;5;%dm%s\027[0m" number text;;

let basecol = Red;;
let basecolarr = [Red; Green; Blue];;

let showbase = 
  (fun c -> printf "  Basecolor %d\n" (basecolor_to_int c));;

type weight = Regular | Bold;;
type color = 
  | Basic of basecolor * weight
  | RGB of int * int * int
  | Gray of int;;

let colarr = 
  [Gray 123; RGB (255,255,255); Basic (Red, Bold)];;

let color_to_int = function
  | Basic (basecolor,weight) ->
    let base = match weight with Bold -> 8 | Regular -> 0 in
    base + basecolor_to_int basecolor
  | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | Gray i -> 232 + i ;;

let showcol c = 
  let num = color_to_int c in
  printf "%s\n" (color_by_number num (string_of_int num))
;;

let () =
  print_string "OneCol:\n";
  showbase basecol;
  print_string "OneArr:\n";
  List.iter ~f:showbase basecolarr;

  List.iter ~f:showcol colarr;
;;
