
open Core.Std;;

type 'a dnode =
  | Dnil
  | Dnode of 'a * 'a dnode * 'a dnode
  | DLnode of 'a * 'a dnode Lazy.t * 'a dnode
type 'a dlist = 'a dnode * 'a dnode
type dir = Up | Down

let print_llist ll dir = 
  let hstr = "===\n" 
  and tstr = "---\n" in
  let rec printR_down = function
    | Dnil -> print_string tstr
    | Dnode(a,_,down) | DLnode(a,_,down) -> printf " * %d\n" a; printR_down down
  in 
  let rec printR_up = function
    | Dnil -> print_string hstr
    | Dnode(a,up,_) -> printR_up up; printf " * %d\n" a;
    | DLnode(a,up,_) -> printR_up (Lazy.force up); printf " * %d\n" a;
  in 
  match dir with
    | Up -> printR_up ll; print_string tstr;
    | Down ->  print_string hstr; printR_down ll;
;;
  
let list2dlist ls = 
  let rec render acc = function
    | [] -> Dnil
    | x :: xx -> let rec node = lazy (DLnode (x, acc, render node xx)) in
		 Lazy.force node
  in
  render (Lazy.from_val Dnil) ls
;;

let dlistlen ls =
  let rec dlistlenR d = function
    | Dnil -> d
    | Dnode(_,_,down) | DLnode(_,_,down) -> (dlistlenR (d+1) down)
  in 
  dlistlenR 0 ls
;;

let dlistsum ls =
  let rec dlistsumR d = function
    | Dnil -> d
    | Dnode(a,_,down) | DLnode(a,_,down) -> (dlistsumR (d+a) down)
  in 
  dlistsumR 0 ls
;;

let () =
  let rec l1 = Dnode(42, Dnil, l2)
  and l2 = Dnode(11, l1, l3)
  and l3 = Dnode(89, l2, Dnil) in

  print_string " Printing up\n";
  print_llist l3 Up;
  print_string " Printing down\n";
  print_llist l1 Down;
    
  let longlist = list2dlist [1;2;3;4;5;6;7;8;9]
  and longerlist = list2dlist [10;20;30;40;50;60;70;80;90;
			       100;200;300;400;500;600;700;800;900]
  and shortlist = list2dlist [12;24;36]
  and blacklist = list2dlist [6;66;666;6666;66666] in
  let them_lists = [longlist; longerlist; shortlist; blacklist] in

  print_string " Printing a long one\n";
  print_llist longlist Down;

  let findByFunc func lss =
    let fmaps = List.map ~f:(fun ls -> (func ls, ls)) lss in
    let sorted_fmaps = List.sort fmaps
      ~cmp:(fun als bls -> if fst als < fst bls then -1 else 1)
    in
    List.hd_exn sorted_fmaps;
  in

  let findMinLen = findByFunc dlistlen
  and findMinSum = findByFunc dlistsum in
  
  let (wlen,winner) = findMinLen them_lists in
  printf "minlen winner at length %d\n" wlen;
  print_llist winner Down;

  let (wsum,winner2) = findMinSum them_lists in
  printf "minsum winner at sum %d\n" wsum;
  print_llist winner2 Down;

;;


      
