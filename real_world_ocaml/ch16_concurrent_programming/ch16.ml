open Async
open Reader
open Core
open Scheduler

let demo01 =
  let uppercase_file filename =
    Reader.file_contents filename >>= fun text ->
    print_endline (String.uppercase text);
    shutdown 0;
    return ()
  in
  uppercase_file "./README.md"

let () =
  don't_wait_for demo01;
  never_returns (Scheduler.go ())
