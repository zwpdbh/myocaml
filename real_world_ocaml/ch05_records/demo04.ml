open Core

(* When multiple records has common name, the best practise is to put each in a different module*)

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
  [@@deriving fields]
end

(*  a small function (fun x -> x.Logon.user) to access the user field*)
let get_users_v1 logons =
  List.dedup_and_sort ~compare:String.compare (List.map logons ~f:(fun x -> x.Logon.user))

(* By using @@deriving fields, it generates a lot of helper functions *)
let get_users_v2 logons =
  List.dedup_and_sort ~compare:String.compare
    ((* notice the lambda function is replaced with getter function  *)
     List.map logons ~f:Logon.user)
(* when used in utop, need: #require "ppx_jane";; *)

let show_field field to_string record =
  let name = Field.name field in
  let field_string = to_string (Field.get field record) in
  name ^ ": " ^ field_string

let logon =
  {
    Logon.session_id = "26685";
    time = Time_ns.of_string_with_utc_offset "2017-07-21 10:11:45Z";
    user = "yminsky";
    credentials = "Xy2d9W";
  }

let _ = show_field Logon.Fields.user Fn.id logon

(* Use Logon.Fields.iter and show_field to print out all the fields of a Logon record  *)

let print_logon logon =
  let print to_string field = printf "%s\n" (show_field field to_string logon) in
  Logon.Fields.iter ~session_id:(print Fn.id) ~time:(print Time_ns.to_string_utc)
    ~user:(print Fn.id) ~credentials:(print Fn.id)

let _ = print_logon logon
