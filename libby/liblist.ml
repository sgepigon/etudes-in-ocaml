
open Printf
open Util ;;

(* List library play *)

let main () =
  
  (* common *)

  let arr = [1;2;3;4;5] in

  test   "length"      List.length     arr             5;
  test   "head"        List.hd         arr             1;
  test   "tail"        List.tl         arr             [2;3;4;5];
  test2  "nth"         List.nth        arr 2           3;
  test   "rev"         List.rev        arr             [5;4;3;2;1];
  test2  "append"      List.append     arr [1;2]       [1;2;3;4;5;1;2];
  test2  "rev_append"  List.rev_append arr [1;2]       [5;4;3;2;1;1;2];
  test   "concat"      List.concat     [arr;[1;2]]     [1;2;3;4;5;1;2];
  test   "flatten"     List.flatten    [arr;[1;2]]     [1;2;3;4;5;1;2];
    
  (* iteration *)

  let count = ref 0 in
  let incr _ = count := !count + 1; in  
  testu2 "iter"       List.iter       incr arr         count 5;

  let count = ref 0 in
  let incr i x = count := !count + i + x; in  
  testu2 "iteri"      Util.iteri      incr arr         count 25;

  let incr x = x + 1 in
  test2  "map"         List.map        incr arr        [2;3;4;5;6];
  
  let incr i x = x + i in
  test2  "mapi"        Util.mapi       incr arr        [1;3;5;7;9];
  
  let incr x = x + 1 in
  test2  "rev_map"     List.rev_map    incr arr        [6;5;4;3;2];

  let add x y = x + y in
  test3  "fold_left"    List.fold_left  add 0 arr      15; (* tail recursive *)
  test3  "fold_right"   List.fold_right add arr 0      15; (* not tail recursive *)

  (* iteration on two lists *)

  let count = ref 0 in
  let incr x y = count := !count + x + y; in    
  testu3 "iter2"        List.iter2      incr arr arr   count 30;

  let incr x y = x + y; in    
  test3  "map2"         List.map2       incr arr arr   [2;4;6;8;10];
  test3  "rev_map2"     List.rev_map2   incr arr arr   [10;8;6;4;2];

  let incr x y z = x + y + z; in    
  test4  "fold_left2"   List.fold_left2  incr 0 arr arr  30;
  test4  "fold_right2"  List.fold_right2 incr arr arr 0  30;
  
  (* scanning *)

  let isodd x = (x mod 2) == 1
  and grz   x = (x > 0) 
  and ngrz  x = (x <= 0) in
  test2  "for_all A"    List.for_all    isodd arr      false;
  test2  "for_all B"    List.for_all    grz arr        true;
  test2  "exists A"     List.exists     isodd arr      true;
  test2  "exists B"     List.exists     ngrz arr       false;

  let equal x y = (x == y) in
  test3  "for_all2 A"   List.for_all2   equal arr arr  true; 
  test3  "for_all2 B"   List.for_all2   equal arr (List.rev arr) false; 

  test2  "mem A"        List.mem        5 arr          true;
  test2  "mem B"        List.mem        6 arr          false;

  let x = Some 3 
  and y = Some 3 in 
  let xx = [Some 1; Some 2; x] 
  and yy = [Some 1; Some 2; Some 3] in
  test2  "memq A"       List.memq       x yy           false;
  test2  "memq B"       List.memq       y yy           false;
  test2  "memq C"       List.memq       x xx           true;
  test2  "memq D"       List.memq       y xx           false;

  (* Searching a list *)
  
  test2  "find"         List.find       grz arr        1;
  test2  "filter"       List.filter     isodd arr      [1;3;5];
  test2  "find_all"     List.find_all   isodd arr      [1;3;5];
  test2  "partition"    List.partition  isodd arr      ([1;3;5], [2;4]);

  (* Association lists *)

  let x = Some 2 in
  let ass = [(Some 1,'a'); (x,'b'); (Some 3,'c')] in
  test2  "assoc"        List.assoc      (Some 2) ass          'b';
(*test2  "assoc"        List.assq       (Some 2) ass          'b'; (* Not_found *) *)
  test2  "assq"         List.assq       x ass                 'b';
  test2  "mem_assoc"    List.mem_assoc  (Some 2) ass          true;
  test2  "mem_assq A"   List.mem_assq   (Some 2) ass          false;
  test2  "mem_assq B"   List.mem_assq   x ass                 true;
  
  test2  "remove_assoc" List.remove_assoc (Some 3) ass        [(Some 1, 'a'); (x,'b')];
  test2 "remove_assq A" List.remove_assq (Some 2) ass         [(Some 1, 'a'); (x, 'b'); (Some 3,'c')];
  test2 "remove_assq B" List.remove_assq (x) ass              [(Some 1, 'a'); (Some 3,'c')];
;;

main ()
