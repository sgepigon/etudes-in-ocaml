
open Core.Std;;

let hmapsize = 10;;
let hmap = Hashtbl.create ~hashable:Int.hashable ();;
let hist = ref (Array.create ~len:hmapsize 0);;
let idx = ref 0;;

let factorial nn =
  let printer = 
    print_string "HTbl: ";
    Hashtbl.iter ~f:(fun ~key ~data -> printf "(%d->%d), " key data) hmap;
    printf "\n";
  in      
  let adder k v = 
    Hashtbl.replace hmap ~key:k ~data:v;
    !hist.(!idx) <- k;
    !idx <- !idx + 1;
    if !idx = hmapsize then 
      !idx <- 0; Hashtbl.remove hmap (Array.get !hist !idx);    
    printer;    
  in
    
  let rec fac n =
    match Hashtbl.find hmap n with
      | Some a -> 
	printf "Hit %d\n" n;
	a
      | None ->
	printf "Miss %d\n" n;
	if n = 0 then 1 else
	  let ans = n * (fac (n-1)) in
	  adder n ans;
	  ans
  in
  fac nn;
;;

let () =
  let pr = printf "factorial %d = %d\n" in
  let range = Batteries.(BatList.of_enum (1--20)) in
  let randypairs = List.map ~f:(fun c -> (Random.bits (), c)) range in
  let sond = List.sort ~cmp:compare randypairs in
  let testlist = List.map ~f:snd sond in
  
  List.iter ~f:(fun a -> pr a (factorial a)) testlist;
;;
  



