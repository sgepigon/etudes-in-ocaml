
(* Packing types into modules *)

open Core.Std;;

module Log_entry = struct
  type t = (* standard naming convention, type t *)
      { session_id: string;
        time: Time.t;
        important: bool;
        message: string;
      }
end
module Heartbeat = struct
  type t =
      { session_id: string;
        time: Time.t;
        status_message: string;
      }
end
module Logon = struct
  type t =
      { session_id: string;
        time: Time.t;
        user: string;
        credentials: string;
      }
end;;


let create_log_entry ~session_id ~important message =
  { Log_entry.time = Time.now (); 
    Log_entry.session_id;
    Log_entry.important; 
    Log_entry.message }
;;

let create_log_entry_concise ~session_id ~important message =
  { Log_entry.     (* Note this whitespace *)
    time = Time.now (); session_id; important; message }
;;

let msg_to_string {Log_entry.important; message; _ } = (* Works other way as well *)
  if important then String.uppercase message else message
;;
  
let is_important t = t.Log_entry.important (* access record field in a type module *)
;;

type client_info =
    { addr: Unix.Inet_addr.t;
      port: int;
      user: string;
      credentials: string;
      last_heartbeat_time: Time.t;
    };;

let register_heartbeat_verbose t hb =
  { addr = t.addr;
    port = t.port;
    user = t.user;
    credentials = t.credentials;
    last_heartbeat_time = hb.Heartbeat.time;
  };;

let register_heartbeat t hb =  (* concise! functioal update *)
    { t with last_heartbeat_time = hb.Heartbeat.time };;

type server_info =
    { addr: Unix.Inet_addr.t;
      prot: int;
      user: string;
      credentials: string;
      mutable last_heartbeat_time: Time.t;
      mutable last_heartbeat_status: string;
    };;   

let regster_hearbeat_server t hb =
  t.last_heartbeat_time   <- hb.Heartbeat.time;
  t.last_heartbeat_status <- hb.Heartbeat.status_message
;; (* arrows are not used on creation, only on mutation *)

module Logon2 = struct
  type t =
      { session_id: string;
        time: Time.t;
        user: string;
        credentials: string;
      }
  with fields    (* fieldslib in action; auto helper functions *)
(*
  Field.name         -- returns the name of a field
  Field.get          -- returns the content of a field
  Field.fset         -- does a functional update of a field
  Field.setter       -- returns None     if the field is not mutable 
                                Some f   for mutating if it is

  Logon2.Fields.session_id   --  (Logon2.t, string) Field.t
  Logon2.Fields.time         --  (Logon2.t, Time.t) Field.t
*)
end;;

let get_users logons = List.dedup (List.map logons ~f:Logon2.user);;

let user_extraction_fun = Field.get Logon2.Fields.user;;

(* First class fields *)
let show_field field to_string record =
  let name = Field.name field in
  let field_string = to_string (Field.get field record) in
    name ^ ": " ^ field_string
  ;;

let () =
  let logon = { Logon2.
		session_id = "12345";
		time = Time.now ();
		user = "ahalme";
		credentials = "AbCdE"; } in
  
  let out_string1 = show_field Logon2.Fields.user Fn.id logon (* the identity function *)
  and out_string2 = show_field Logon2.Fields.time Time.to_string logon in

  print_string (out_string1 ^ "\n");
  print_string (out_string2 ^ "\n\n");

  (* and higher funtionality, too, in Fields *)
  let print_logon logon =
    let print to_string field =
      printf "%s\n" (show_field field to_string logon)
    in
    Logon2.Fields.iter
      ~session_id:(print Fn.id)
      ~time:(print Time.to_string)
      ~user:(print Fn.id)
      ~credentials:(print Fn.id)
  in

  print_logon logon;
;;
  

  
  
    
