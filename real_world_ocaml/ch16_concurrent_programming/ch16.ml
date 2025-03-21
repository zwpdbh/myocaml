open Core
open Async

let command =
  Command.group ~summary:"Ch016: Concurrent Programming"
    [
      ("demo01", Simple_async.command);
      ("demo02", Simple_async.command_for_let_binding);
      ("demo03", Echo_server.command);
      ("demo04", Query_duckduckgo.command);
    ]

let () = Command_unix.run ~version:"1.0" ~build_info:"" command
