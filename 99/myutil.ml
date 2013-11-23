
let rec reverse = function
  | [] -> []
  | x::[] -> [x]
  | x::xs -> (reverse xs) @ [x]
;;

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
;;

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
    | [] -> res
    | c :: l -> res.[i] <- c; imp (i + 1) l in imp 0 l
;;
