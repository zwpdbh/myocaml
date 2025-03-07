open Core

(* Combining Records and Variants *)
module Time_ns = Core.Time_ns

module Log_entry = struct
  type t = { session_id : string; time : Time_ns.t; important : bool; message : string }
end

module Heartbeat = struct
  type t = { session_id : string; time : Time_ns.t; status_message : string }
end

module Logon = struct
  type t = { session_id : string; time : Time_ns.t; user : string; credentials : string }
end
