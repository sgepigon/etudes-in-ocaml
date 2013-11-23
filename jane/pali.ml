
open Core.Std;;
open Core_kernel.Quickcheck;;
module QC = Core_kernel.Quickcheck;;

let reverse s l = 
  let t = String.create l in
  for i = 0 to (l-1) do
    t.[i] <- s.[l-1-i];  
  done;
  t

let isPalin str =
  let l = (String.length str) in
  let oddset = (l mod 2) in
  let mid = (l/2) in
  
  let front = String.sub str ~pos:0 ~len:mid 
  and back = reverse (String.sub str ~pos:(mid+oddset) ~len:mid) mid in
  
  String.equal front back;
;;


let () =
  let s = "Hello world!" in
  let palinStatus s = 
    let filler = if (isPalin s) then "" else "not " in
    sprintf " '%s' is %sa palindrome\n" s filler 
  in

  let rec checkPalins xs = 
    match xs with
      | [] -> ()
      | x::xs -> 
	print_string (palinStatus x);
	checkPalins xs
  in
  
  printf "\n %s|%s\n\n" s (reverse s (String.length s));
  print_string (palinStatus s);

  let my_strings = ["HelloolleH"; "HelloOLLEH"; "Hello"; "Th"; "ere"; "World"; 
		    "saippuakauppias"; "xyzzyx"; "xxx"; ""] in
  checkPalins my_strings;

  (* Testing *)
  
  let isPaliTest s =
    if (isPalin s) then begin
      printf "  '%s' should be a pali :G\n" s;
      String.equal s (reverse s (String.length s)) 
    end
    else begin
      printf "  '%s' is not a pali :/\n" s;
      true 
    end
  in
  
  let getPos max = Random.int max in

  let chars = "abcdefghijklmnopqrstuvwxyzy" in
  let asciiGen dict = (fun () -> chars.[getPos (String.length dict)]) in
  let lengthGen bound = (fun () -> Random.int bound) in 
  let stringGen = QC.sg ~char_gen:(asciiGen chars) ~size_gen:(lengthGen 16) in
    
  printf "\n Generating some randy chars: ";
  QC.repeat 10 (printf "%c ") (asciiGen chars);
  printf "\n";

  printf "\n Generating some randy strings:\n";
  QC.repeat 10 (printf "   %s\n") stringGen;
  printf "\n";

  let small_alpha = "ab" in
  let runnerGen = QC.sg ~char_gen:(asciiGen small_alpha) ~size_gen:(lengthGen 16) in

  printf "\n Generating some randy small alphabet strings:\n";
  QC.repeat 10 (printf "   %s\n") runnerGen;
  printf "\n\n";

  let runner = QC.laws 100 runnerGen isPaliTest in
  match runner with
    | Some case -> printf "Test failed on '%s' :(" case;
    | None -> ()
;;
