open Core
open Async

(*  Deferred.both, which lets you wait until two deferreds of different types have returned, returning both values as a tuple *)
let string_and_float =
  Deferred.both
    (let%map () = after (sec 0.5) in
     "A")
    (let%map () = after (sec 0.25) in
     32.33)

(* Timeouts, Cancellation, and Choices *)
let any_one_of_them_is_determined =
  Deferred.any
    [
      (let%map () = after (sec 0.5) in
       "half a second");
      (let%map () = after (sec 1.0) in
       "one second");
      (let%map () = after (sec 4.0) in
       "four seconds");
    ]

let command =
  Command.group ~summary:"Ch016: Concurrent Programming"
    [
      ("demo01", Simple_async.command);
      ("demo02", Simple_async.command_for_let_binding);
      ("demo03", Echo_server.command);
      ("demo04", Query_duckduckgo.command);
      ("demo05", Query_duckduckgo.command_show_async_exception);
    ]

let () = Command_unix.run ~version:"1.0" ~build_info:"" command
