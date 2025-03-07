open Core

module Log_entry = struct
  type t = { important : bool; message : string }
end

module Heartbeat = struct
  type t = { status_message : string }
end

module Logon = struct
  type t = { user : string; credentials : string }
end

type details = Logon of Logon.t | Heartbeat of Heartbeat.t | Log_entry of Log_entry.t

(* we need a record that contains the fields that are common across all messages: *)
module Common = struct
  type t = { session_id : string; time : Time_ns.t }
end

let messages_for_user user (messages : (Common.t * details) list) =
  let user_messages, _ =
    List.fold messages
      ~init:([], Set.empty (module String))
      ~f:(fun ((messages, user_sessions) as acc) ((common, details) as message) ->
        match details with
        | Logon m ->
            if String.( = ) m.user user then
              (message :: messages, Set.add user_sessions common.session_id)
            else acc
        | Heartbeat _ | Log_entry _ ->
            if Set.mem user_sessions common.session_id then (message :: messages, user_sessions)
            else acc)
  in
  List.rev user_messages

(* In addition, this design allows us to grab the specific message and dispatch code to handle just that message type. *)
(* let handle_message server_state ((common : Common.t), details) =
  match details with
  | Log_entry m -> handle_log_entry server_state (common, m)
  | Logon m -> handle_logon server_state (common, m)
  | Heartbeat m -> handle_heartbeat server_state (common, m) *)
