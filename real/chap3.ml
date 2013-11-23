
(* On lists. *)

open Core.Std
open Core_bench.Std

let decorate str =
  let bar = "---\n"in 

  print_string bar;
  print_string (str ^ "\n");
  print_string bar;
;;

let run_bench tests =
  Bench.bench tests
    ~ascii_table:true
    ~ci_absolute:true
    ~time_quota:(Time.Span.of_sec 2.)
    ~display:Textutils.Ascii_table.Display.column_titles
;;

let run_basics =
  let x = [1;2;3]
  and y = 1 :: (2 :: (3 :: []))
  and non = [] in 
  let z = 1 :: 2 :: 3 :: non
  and str = "world" :: non in
  let str2 = "hello" :: str
  in

  let mush = (fun a -> String.concat ~sep:" " a) in
  let mush_int = (fun a -> mush (List.map ~f:string_of_int a)) in
  
  decorate ((mush str2) ^ " ** ");
  decorate (mush (List.map ~f:(fun a -> (mush a) ^ " ** ") [non; str; str2]));
  decorate (mush (List.map ~f:(fun a -> (mush_int a) ^ " ** ") [x; y; z;]))
;;

let run_matching =
  let rec sum l =
    match l with
    | [] -> 0
     (* patterns can't do arbitrary condition checking; just match data layout *)
    | hd :: tl -> hd + sum tl
  in

  decorate (string_of_int (sum [1;2;3;4;5;6;7;8;9;10]));
  
  let rec sum_if l =
    if List.is_empty l then 0
    else List.hd_exn l + sum_if (List.tl_exn l)
  in

  let numbers = List.range 0 1000 in
  [ Bench.Test.create ~name:"sum_if" (fun () -> ignore (sum_if numbers))
  ; Bench.Test.create ~name:"sum"    (fun () -> ignore (sum numbers)) ]
  |> run_bench      
;;


let run_benching =
  let plus_one_match x =
    match x with
    | 0 -> 1
    | 1 -> 2
    | 2 -> 3
    | _ -> x + 1
  and plus_one_if x =
    if      x = 0 then 1
    else if x = 1 then 2
    else if x = 2 then 3
    else x + 1
  in

  let test1 = Bench.Test.create ~name:"plus_one_match"
    (fun () -> ignore (plus_one_match 10)) 
  and test2 = Bench.Test.create ~name:"plus_one_if"
    (fun () -> ignore (plus_one_if 10)) 
  in

  [test1; test2] |> run_bench      
;;

let run_pretty =
  let max_widths header rows =
    let lengths l = List.map ~f:String.length l in
    List.fold rows
      ~init:(lengths header)
      ~f:(fun acc row ->
        List.map2_exn ~f:Int.max acc (lengths row))
  in

  let render_separator widths =
    let pieces = List.map widths
      ~f:(fun w -> String.make (w + 2) '-')
    in
    "|" ^ String.concat ~sep:"+" pieces ^ "|"
  in

  let pad s length =
    " " ^ s ^ String.make (length - String.length s + 1) ' '
  in

  let render_row row widths =
    let padded = List.map2_exn row widths ~f:pad in
    "|" ^ String.concat ~sep:"|" padded ^ "|"
  in

  let render_table header rows =
    let widths = max_widths header rows in
    String.concat ~sep:"\n"
      (render_row header widths
       :: render_separator widths
       :: List.map rows ~f:(fun row -> render_row row widths)
      )
  in

  decorate(
    sprintf "\n---\n%s\n" 
      (render_table
	 ["language";"architect";"first release"]
	 [ ["Lisp" ;"John McCarthy" ;"1958"] ;
	   ["C"    ;"Dennis Ritchie";"1969"] ;
	   ["ML"   ;"Robin Milner"  ;"1973"] ;
	   ["OCaml";"Xavier Leroy"  ;"1996"] ;
	 ])    
  )
;;


let run_misc =
  let nums = [1;2;3;4;5] in
  let red xs = match (List.reduce ~f:(+) xs) with
    | None -> "-1"
    | Some x -> string_of_int x
  in
  let phil = List.filter ~f:(fun x -> x mod 2 = 0) nums
  in

  decorate ("Reduced to: " ^ (red nums));
  decorate ("Filtered reduced to: " ^ (red phil))
;;

let run_ls =
  let lsdir = List.filter_map (Sys.ls_dir ".") ~f:(fun fname ->
    match String.rsplit2 ~on:'.' fname with
    | None  | Some ("",_) -> None
    | Some (_,ext) -> Some ext)
  |> List.dedup
  in
    
  let mush = String.concat ~sep:"; " in
  
  decorate (mush lsdir);

  let is_ocaml_source s =
    match String.rsplit2 s ~on:'.' with
    | Some (_,("ml"|"mli")) -> true
    | _ -> false
  in
  let (ml_files,other_files) =
    List.partition_tf (Sys.ls_dir ".") ~f:is_ocaml_source
  in

  decorate (" ml_files: " ^ (mush ml_files) ^ "\nothers: " ^ (mush other_files));    

  let rec ls_rec s =
    if Sys.is_file_exn ~follow_symlinks:true s
    then [s]
    else
      Sys.ls_dir s
  |> List.concat_map ~f:(fun sub -> ls_rec (s ^/ sub))
  in

  decorate (" RECURSIVE:\n" ^ (mush (ls_rec ".")));    
;;

let run_tailr =
  let make_list n = List.init n ~f:(fun x -> x) in
  let rec length_plus_n l n =
    match l with
    | [] -> n
    | _ :: tl -> length_plus_n tl (n + 1)
  in 
  let length l = length_plus_n l 0 in

  let rec length2 = function
    | [] -> 0
    | _ :: tl -> 1 + length2 tl
  in

  let testlist = (make_list 100) in
  (* let testlist = (make_list 10_000_000) in *)
  (** Fatal error: exception Stack_overflow **)
  
  decorate (" Length1:" ^ (string_of_int (length testlist)));
  decorate (" Length2:" ^ (string_of_int (length2 testlist)));
;;

let run_destutter =
  let rec destutter = function
    | [] | [_] as l -> l
    | hd :: (hd' :: _ as tl) when hd = hd' -> destutter tl
    | hd :: tl -> hd :: destutter tl
  in
  let vec = [1;1;2;6;4;5;5;3] in

  decorate (String.concat ~sep:"; " (List.map ~f:string_of_int (destutter vec)))
;;


let run_counting =
  let rec count_some_fun list =
    match list with
    | [] -> 0
    | None   :: tl -> count_some_fun tl
    | Some _ :: tl -> 1 + count_some_fun tl
  in

  let count_some_real l = List.count ~f:Option.is_some l
  in

  let x = ref 0 in
  let y = ref 0 in
  let numbers = List.range 0 100000 in
  let optioned = List.map numbers ~f:(fun x -> if (x mod 3 = 0) then Some x else None) in 
  [ Bench.Test.create ~name:"count_fun" (fun () -> (x := count_some_fun optioned))
  ; Bench.Test.create ~name:"count_real" (fun () -> (y := count_some_real optioned))]
  |> run_bench;

  decorate(sprintf " Got x=%d y=%d\n" (!x) (!y))
;;
