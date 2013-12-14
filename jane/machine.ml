
open Core.Std;;

type addr = Addr of int
type instr = 
  | Add of int * int | Sub of int * int | Div of int * int | Mul of int * int
  | Push of int | AddS | SubS | DivS | MulS | Store of addr | Load of addr | NOP
  | Swap | Rot | Fib | Fibn of int

type stack = Stack of int list
type mem = Mem of int array
type machine = M of stack * mem

exception Runfail_exn;;
exception EmptyStack_exn;;
exception NOP_exn;;
let rec run_instr (Stack s, Mem m) i = 
  let default = (Stack s, Mem m) in
  try
    match i with
      | Push a -> (Stack (a :: s), Mem m)
      | Add (a, b) -> (Stack ((a+b) :: s), Mem m)
      | Swap -> begin match s with 
	  | a :: b :: c -> (Stack (b :: a :: c), Mem m)
	  | _ -> raise Runfail_exn
      end
      | AddS -> begin match s with 
	  | a :: b :: c -> (Stack ((a+b) :: c), Mem m)
	  | _ -> raise Runfail_exn
      end
      | MulS -> begin match s with 
	  | a :: b :: c -> (Stack ((a*b) :: c), Mem m)
	  | _ -> raise Runfail_exn
      end
      | DivS -> begin match s with 
	  | a :: b :: c -> (Stack ((a/b) :: c), Mem m)
	  | _ -> raise Runfail_exn
      end
      | Fib -> begin match s with 
	  | a :: b :: c -> (Stack ((a+b) :: a :: b :: c), Mem m)
	  | _ -> raise Runfail_exn
      end
      | Fibn n -> begin
	if (n = 0) then default
	else begin
	  let ss,mm = run_instr (Stack s, Mem m) Fib in
	  run_instr (ss,mm) (Fibn (n - 1))
	end;
      end
      | Rot -> (Stack (List.rev s), Mem m)
      | Store Addr(addr) -> 
	begin match s with
	  | [] -> raise EmptyStack_exn
	  | x::xx -> Array.set m addr x; (Stack xx, Mem m) 
	end
      | Load Addr(addr) -> (Stack (Array.get m addr :: s), Mem m)
      | _ -> raise NOP_exn
  with 
    | EmptyStack_exn -> print_string "Stack fail.\n"; default
    | Division_by_zero -> print_string "Div by zero.\n"; default
    | NOP_exn -> print_string "Not implemented yet!\n"; default      
    | Runfail_exn | Invalid_argument _ -> print_string "Something went wrong...\n"; default
;;
  
let getMemStr (_, Mem m) = List.to_string ~f:(Int.to_string) (Array.to_list m);;
let getStackStr (Stack s, _) = List.to_string ~f:(Int.to_string) s;;

let printM m = 
  printf "Stack: %s\n" (getStackStr m);
  printf "Mem: %s\n" (getMemStr m);
;;

exception Parsefail_exn;;
let parse input = 
  try 
    let words = String.split ~on:' ' input in
    let atoi = Int.of_string in
    match words with
      | "add" :: a :: b :: [] -> Add (atoi a, atoi b)
      | "adds" :: [] -> AddS
      | "muls" :: [] -> MulS
      | "divs" :: [] -> DivS
      | "swap" :: [] -> Swap
      | "rot" :: [] -> Rot
      | "fib" :: [] -> Fib
      | "fibn" :: n :: [] -> Fibn (atoi n)
      | "store" :: a :: [] -> Store (Addr (atoi a ))
      | "load" :: a :: [] -> Load (Addr (atoi a ))
      | "push" :: a :: [] -> Push (atoi a)
      | _ -> NOP
  with 
      _ -> raise Parsefail_exn;
;;

let () =
  let memsize = 32 in
  let boot = (Stack [], Mem (Array.create ~len:memsize 0)) in
  
  (* let testops = [Add (4,5); Add (2,3); Store (Addr(10))] in *)
  (* let res = List.fold ~init:boot ~f:run_instr testops in *)
  (* printM res; *)
  
  (* I/O *)
  let m = ref boot in
  let quit = ref false in
  printf "\nStarting I/O, 'q' quits, 'h' helps\n";
  printM !m;
  while not !quit do
    printf "> ";
    let input = read_line () in
    if (String.equal input "q") then quit := true
    else if (String.equal input "h") 
    then begin
      print_endline "Syntax:";
      print_endline " push a    --> a to stack.top";
      print_endline " store p   --> stack.top to mem[p]";
      print_endline " load p    --> mem[p] to stack.top";
      print_endline " add a b   --> stack a+b";
      print_endline " adds      --> in-stack add";
      print_endline " muls      --> in-stack mul";
      print_endline " divs      --> in-stack div";
      print_endline " swap      --> swap the first two";
      print_endline " fib       --> in-stack Fibonacci";
      print_endline " fibn n    --> in-stack nth next Fibonacci";
    end
    else begin
      try 
	m := run_instr !m (parse input);
	printM !m;
      with Parsefail_exn -> printf "Parse error. Try something else.\n";
    end
  done
    
;;


