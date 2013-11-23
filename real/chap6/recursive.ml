
open Core.Std;;

type 'a expr =
  | Base  of 'a
  | Const of bool
  | And   of 'a expr list
  | Or    of 'a expr list
  | Not   of 'a expr       (* Note recursive structure *)
;;

type mail_field = To | From | CC | Date | Subject
type mail_predicate = { field: mail_field;
			contains: string };;

let qparam field contains = Base {field; contains};;

let myquery =
  And [ Or [qparam To "doligez"; qparam CC "doligez"];
	qparam Subject "runtime";
      ];;

let rec eval expr base_eval =
  (* a shortcut, avoid repeat explicit pass of [base_eval] to [eval] *)
  let eval' expr = eval expr base_eval in
  match expr with
    | Base  base   -> base_eval base
    | Const bool   -> bool
    | And   exprs -> List.for_all exprs ~f:eval'
    | Or    exprs -> List.exists  exprs ~f:eval'
    | Not   expr  -> not (eval' expr)
;;

(* helper functions *)
let and_ l =
    if List.mem l (Const false) then Const false
    else
      match List.filter l ~f:((<>) (Const true)) with
	| [] -> Const true
	| [ x ] -> x
	| l -> And l

let or_ l =
    if List.mem l (Const true) then Const true
    else
      match List.filter l ~f:((<>) (Const false)) with
	| [] -> Const false
	| [x] -> x
	| l -> Or l

let not_ = function
  | Const b -> Const (not b)
  | e -> Not e
  ;;

let rec simplify = function
  | Base _ | Const _ as x -> x
  | And l -> and_ (List.map ~f:simplify l)
  | Or l  -> or_  (List.map ~f:simplify l)
  | Not e -> not_ (simplify e)
  ;;

let render_string str = sprintf "'%s' " str

let render_field {field; contains} =
  match field with 
    | To -> sprintf "TO:%s " contains
    | From -> sprintf "FROM:%s " contains
    | CC -> sprintf "CC:%s " contains
    | Date -> sprintf "DATE:%s " contains
    | Subject -> sprintf "SUBJ:%s " contains
;;

let rec stringify render_f = function
  | Base b -> render_f b 
  | Const c -> if c then "True" else "False";
  | And l -> sprintf "[ %s] "
    (String.concat ~sep:"AND " 
       (List.map ~f:(stringify render_f) l));
  | Or l  -> sprintf "[ %s] " 
    (String.concat ~sep:"OR " 
       (List.map ~f:(stringify render_f) l));
  | Not e -> sprintf "NOT [ %s] " (stringify render_f e)

let () =
  let simplified = simplify 
    (Not (And [ Or [Base "it's snowing"; Const true];
		Base "it's raining"])) in
  
  printf "%s\n" (stringify render_field myquery);

  printf "%s\n" (stringify render_string simplified);
;;
