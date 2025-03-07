(* Reusing Field Names *)
(* let’s consider a collection of types representing the protocol of a logging server.  *)
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

let create_log_entry_v1 ~session_id ~important message =
  { Log_entry.time = Time_ns.now (); Log_entry.session_id; Log_entry.important; Log_entry.message }

let create_log_entry_v2 ~session_id ~important message =
  (* OCaml only requires the module qualification for one record field, so write it more concisely *)
  { Log_entry.time = Time_ns.now (); session_id; important; message }

(* Use a type annotation make it more concise*)
let create_log_entry_v3 ~session_id ~important message : Log_entry.t =
  { time = Time_ns.now (); session_id; important; message }

(* When used in parameter patter matching *)
let message_to_string_v1 { Log_entry.important; message; _ } =
  if important then String.uppercase message else message

(* When used in parameter patter matching with type annotations  *)
let message_to_string_v2 ({ important; message; _ } : Log_entry.t) =
  if important then String.uppercase message else message

let is_important (t : Log_entry.t) = t.important
