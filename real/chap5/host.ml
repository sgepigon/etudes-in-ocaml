
open Core.Std
open Core_extended.Std

type host_info = {
  host_name : string;
  os_name : string;
  cpu_arch : string;
  timestamp : Time.t;
}

type 'a timestamped = { item: 'a; time: Time.t };;

let () =  
  let my_host =
    let sh = Shell.sh_one_exn in
    { host_name = sh "hostname";
      os_name = sh "uname -s";
      cpu_arch = sh "uname -p";
      timestamp = Time.now ();
    }
  and first_stamped list = 
    List.reduce list ~f:(fun a b -> if a.time < b.time then a else b)
  in
  
  let the_list = [{item = "a"; time = Time.now ()}; 
		  {item = "b"; time = Time.now ()};
		  {item = "c"; time = Time.now ()};] in
  let (first,first_time) = 
    match (first_stamped the_list) with
      | None -> ("None", Time.now ())
      | Some f -> (f.item, f.time)
  in
  
  printf "\n%s %s %s %s\n" 
    my_host.host_name my_host.os_name my_host.cpu_arch (Time.to_string my_host.timestamp);
  
  printf "\n First in time was '%s' at '%s'\n"
    first (Time.to_string first_time);
    
  let thefirst = 
    match List.hd the_list with
      | None -> ();
      | Some h -> printf "\n First in order was '%s' at '%s'\n" h.item (Time.to_string h.time)
  in
      
  let thesecond =
    match List.hd (List.tl_exn the_list) with
      | None -> ()
      | Some { item = i; time = t } ->
	printf "\n Second in order was '%s' at '%s'\n" i (Time.to_string t)
  in
  
  (* punning *)
  let host_info_to_string { host_name; os_name; cpu_arch; timestamp; _ } =
    sprintf "%s (%s / %s) <%s>" host_name os_name cpu_arch (Time.to_string timestamp)
  in
  
  thefirst;
  thesecond;
  printf " My info again: %s\n" (host_info_to_string my_host);

(* 
  let create_host_info ~host_name ~os_name ~cpu_arch = 
    { os_name; cpu_arch; hostname = String.lowercase hostname;
      timestamp = Time.now () }
  and cumbersome_create_host_info 
      ~hostname:hostname ~os_name:os_name ~cpu_arch:cpu_arch ~os_release:os_release =
    { os_name = os_name; 
      cpu_arch = cpu_arch;
      os_release = os_release;
      hostname = String.lowercase hostname;
      timestamp = Time.now () };;
  in
*) 
(* Together, labeled arguments, field names, and field and label punning, encourage a style where you propagate the same names throughout your code-base. This is generally good practice, since it encourages consistent naming, which makes it easier to navigate the source. *)
;;

  
