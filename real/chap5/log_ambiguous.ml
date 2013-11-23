
open Core.Std;

type log_entry =
    { session_id: string;
      time: Time.t;
      important: bool;
      message: string;
    }
type heartbeat =
    { session_id: string;
      time: Time.t;
      status_message: string;
    }
type logon =
    { session_id: string;
      time: Time.t;
      user: string;
      credentials: string;
    }


let get_session_id t = t.session_id;; (* uses most recent *)
(*
  let status_and_session t = (t.status_message, t.session_id);;
  let session_and_status t = (t.session_id, t.status_message);; (* order matters *)
*)

let get_heartbeat_session_id (t:heartbeat) = t.session_id;; (* explicit *)
  
  
