
open Core.Std

let build_counts () =
  In_channel.fold_lines stdin ~init:Mapcounter.empty ~f:Mapcounter.touch

let () =
  let stringify x = 
    match x with
    | Mapcounter.Median s -> s
    | Mapcounter.Lo_hi (s,t) -> s ^ "|" ^ t
  in
  build_counts ()    
  |> (fun x -> printf "Median: %S \n" (stringify (Mapcounter.median x)); x)
  |> Mapcounter.to_list
  |> List.sort ~cmp:(fun (_,x) (_,y) -> Int.descending x y)
  (* |> (fun l -> List.take l 10) *)
  |> List.iter ~f:(fun (line, count) -> printf "%3d: %s\n" count line)
