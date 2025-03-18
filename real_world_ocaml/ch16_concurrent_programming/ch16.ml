open Core
open Async

let command =
  Command.group ~summary:"Ch016: Concurrent Programming" [ ("demo01", Simple_async.command) ]

let () = Command_unix.run ~version:"1.0" ~build_info:"" command
