
open Core.Std;;
module QC = Quickcheck;;


type 'a bintree = 
  | Empty 
  | Node of 'a * ('a bintree) * ('a bintree)
;;

(* not tail recursive *)
let rec treelen = function
  | Empty -> 0
  | Node(_, left, right) -> 1 + (max (treelen left) (treelen right))

let rec treesum = function
  | Empty -> 0
  | Node(a, left, right) -> a + (treesum left) + (treesum right)

(* tail recursive *)
let depth bt =
  let id = (fun d -> d) in
  let rec depthR bt hf = match bt with
    | Empty -> hf 0
    | Node(_,left,right) ->
      depthR left (fun dleft ->
	depthR right (fun dright -> 
	  hf (1 + max dleft dright)));
  in  
  depthR bt id
;;

let treesum2 bt =
  let id = (fun d -> d) in
  let rec sumR bt hf = match bt with
    | Empty -> hf 0
    | Node(a,left,right) ->
      sumR left (fun lsum ->
	sumR right (fun rsum ->
	  hf (a + lsum + rsum)));
  in
  sumR bt id
;;

(* tail recursive, variables up front *)
type ('a, 'b) cont =
  | Cleft of 'a bintree * ('a, 'b) cont (* r and f *)
  | Cright of 'b * ('a, 'b) cont        (* dleft and f *)
  | Cid                                 (* dleft, dright and f *)

let depth2 bt =
  let rec depthR bt f = match bt with
    | Empty -> eval f 0
    | Node(_,left,right) ->      
      depthR left (Cleft(right, f))
  and eval f d = match f with
    | Cleft(right, f) -> depthR right (Cright(d, f))
    | Cright(dl, f) -> eval f (1 + max d dl)
    | Cid -> d
  in  
  depthR bt Cid
;;

(* list-y tail recursive *)
type ('a, 'b) next_item = 
  | Kleft of 'a bintree
  | Kright of 'b
type ('a, 'b) continue = ('a, 'b) next_item list

let depth3 bt =
  let rec depthR bt f = match bt with
    | Empty -> eval f 0
    | Node(_,left,right) -> depthR left (Kleft(right) :: f)
  and eval k d = match k with
    | Kleft(right) :: k -> depthR right (Kright(d) :: k)
    | Kright(dleft) :: k -> eval k (1 + (max d dleft))
    | [] -> d
  in 
  depthR bt [];
;;

type ('a, 'b) sum_item =
  | Sleft of 'a * 'a bintree
  | Sright of 'a * 'b
type ('a, 'b) sum_cont = ('a, 'b) sum_item list

let treesum3 bt =
  let rec sumR bt f = match bt with
    | Empty -> eval f 0
    | Node(a,left,right) -> sumR left (Sleft(a, right) :: f)
  and eval s d = match s with
    | Sleft(a, right) :: s -> sumR right (Sright(a, d) :: s)
    | Sright(a, dleft) :: s -> eval s (a + d + dleft)
    | [] -> d
  in
  sumR bt []
;;

(* main *)
let () =
  let testtree = Node (1, Node (2, Node (42, Empty, Empty), Node (16, Empty, Empty)), Empty) in
  let testtree2 = Node (10, Empty, Node (20, Empty, Node (30, Empty, Node (40, Empty, Empty)))) in 
  
  printf "Treelen of testtree is %d == %d == %d == %d.\n" 
    (treelen testtree) (depth testtree) (depth2 testtree) (depth3 testtree);

  printf "Treesum of testtree is %d == %d == %d.\n" 
    (treesum testtree) (treesum2 testtree) (treesum3 testtree);

  (* QC *)
  let alpha = "abcdefgh" in
  let charGen = (fun () -> alpha.[Random.int (String.length alpha)]) in
  let lenGen = (fun () -> Random.int 16) in
  QC.repeat 10 (printf "%s ") (QC.sg ~char_gen:charGen ~size_gen:lenGen);
  print_newline ();

  let treePrint bt = 
    let rec treePrintD bt d = match bt with  
      | Empty -> ()
      | Node(a,left,right) -> 
	printf "%sNode %d\n" (String.make d '.') a;
	treePrintD left (d + 2);
	treePrintD right (d + 2);
    in
    treePrintD bt 1;    
    print_newline ();
  in
 
  treePrint testtree;
  treePrint testtree2;

  let rec treeString = function
    | Empty -> "-"
    | Node(a,left,right) -> 
      sprintf "Node(%d,%s,%s)" a (treeString left) (treeString right)
  in

  let rec fullAndCompleteTreeGen valGen d =
    if (d = 0) then Empty else
      let t = Node(valGen (), 
		   fullAndCompleteTreeGen valGen (d-1), 
		   fullAndCompleteTreeGen valGen (d-1));
      in
      (* treePrint t; *)
      t
  in
  
  printf "A randy full and complete tree:\n";
  let atree = fullAndCompleteTreeGen lenGen 5 in
  treePrint atree;   
  
  let runner = QC.laws 100 (fun () -> fullAndCompleteTreeGen lenGen 5) 
    (fun t -> (treelen t) = (depth3 t))
  in
  match runner with
    | None -> printf "A-OK.\n"
    | Some t -> printf "Failed on '%s'\n" (treeString t); (treePrint t)
     
;;
