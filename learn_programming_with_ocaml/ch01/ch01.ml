open Core
open Async

let command =
  Command.group ~summary:"Chapter summary"
    [ ("demo01", Leap_year.command); ("demo02", Monte_carlo.command) ]

let () = Command_unix.run ~version:"1.0" ~build_info:"" command
