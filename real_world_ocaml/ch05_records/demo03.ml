(* Functional update *)
(* Solve the problem when only need to update a small set of record *)
open Core

(* When multiple records has common name,  the best practise is to put each in a different module*)

(** The log_entry message is used to deliver a log entry to the server *)
module Log_entry = struct
  type t = { session_id : string; time : Time_ns.t; important : bool; message : string }
end

(** the logon message is sent when initiating a connection and includes the identity of the user
    connecting and credentials used for authentication *)
module Heartbeat = struct
  type t = { session_id : string; time : Time_ns.t; status_message : string }
end

(** heartbeat message is periodically sent by the client to demonstrate to the server that the
    client is alive and connected *)
module Logon = struct
  type t = { session_id : string; time : Time_ns.t; user : string; credentials : string }
end

type client_info = {
  addr : Core_unix.Inet_addr.t;
  port : int;
  user : string;
  credentials : string;
  last_heartbeat_time : Time_ns.t;
  last_heartbeat_status : string;
}

(* Functional updates make your code independent of the identity of the fields in the record that are not changing. *)
let register_heartbeat t hb =
  {
    t with
    last_heartbeat_time = hb.Heartbeat.time;
    last_heartbeat_status = hb.Heartbeat.status_message;
  }
