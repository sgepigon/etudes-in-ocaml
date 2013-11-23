
open Core.Std;;

(* The structured types *)
module Log_entry = struct
  type t = { important: bool;
             message: string; }
end
module Heartbeat = struct
  type t = { status_message: string; }
end
module Logon = struct
  type t = { user: string;
             credentials: string; }
end ;;

(* Variant type combines the types *)
type details =
  | Logon of Logon.t
  | Heartbeat of Heartbeat.t
  | Log_entry of Log_entry.t
 ;;

(* Another modules describes further, common fields *)
module Common = struct
  type t = { session_id: string;
             time: Time.t; }
end ;;

let handle_logon _ _ = ()
and handle_log_entry _ _ = ()
and handle_heartbeat _ _ = ();;

let handle_message server_state (common,details) =
  match details with
    | Logon     m -> handle_logon     server_state (common,m)
    | Log_entry m -> handle_log_entry server_state (common,m)
    | Heartbeat m -> handle_heartbeat server_state (common,m)
;;

let messages_for_user user messages =
  let (user_messages,_) =
    List.fold messages ~init:([],String.Set.empty)
      ~f:(fun ((messages,user_sessions) as acc) ((common,details) as message) ->
        let session_id = common.Common.session_id in
        match details with
          | Logon m -> 
	    if m.Logon.user = user then
              (message::messages, 
	       Set.add user_sessions session_id)
            else acc
          | Heartbeat _ | Log_entry _ ->
            if Set.mem user_sessions session_id then
              (message::messages,user_sessions)
            else acc
      )
  in
  List.rev user_messages
;;


let () =
  let player1 = { Logon.user = "Gary Player"; 
		  Logon.credentials = "creds" } in 
  let player2 = { Logon.user = "Player 2"; 
		  Logon.credentials = "creds" } in 
  let msgs = [({Common.session_id = "S1"; 
		Common.time = Time.now ()}, Logon player1); 
	      ({Common.session_id = "S2"; 
		Common.time = Time.now ()}, Logon player2);
	      ({Common.session_id = "S3"; 
		Common.time = Time.now ()}, Logon player1)] in

  let queryuser = "Gary Player" in
  List.iter (messages_for_user queryuser msgs)
    ~f:(fun entry ->
      let (common, details) = entry in
      printf " %s : %s\n %s : %s \n\n" 
	common.Common.session_id 
	(Time.to_string common.Common.time)
	common.Common.session_id 
	(match details with 
	  | Logon m -> "Login by " ^ m.Logon.user
	  | _ -> "-"));
;;
