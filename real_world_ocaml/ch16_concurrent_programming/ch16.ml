open Async
open Reader
open Core
open Scheduler

let simple_async_read_example =
  let uppercase_file filename =
    Reader.file_contents filename >>= fun text ->
    print_endline (String.uppercase text);
    shutdown 0;
    return ()
  in
  uppercase_file "./README.md"

let run async_f =
  don't_wait_for (async_f ());
  never_returns (Scheduler.go ())

(* let demo01 = run (fun () -> simple_async_read_example) *)
let demo01 name = printf "hello_world, %s" name

let demo01_command =
  Command.basic ~summary:"run demo01"
    ~readme:(fun
        (* dune exec real_world_ocaml/ch16_concurrent_programming/ch16.exe demo01 -- -help *)
          ()
      -> "show simple async read example")
    (let%map_open.Command name = anon ("name" %: string) in
     fun () -> demo01 name)

let demo02_command =
  Command.basic ~summary:"run demo02"
    ~readme:(fun
        (* dune exec real_world_ocaml/ch16_concurrent_programming/ch16.exe demo01 -- -help *)
          ()
      -> "show simple async read example")
    (let%map_open.Command name = anon ("name" %: string) in
     fun () -> demo01 name)

let command =
  Command.group ~summary:"Ch016: Concurrent Programming"
    [ ("demo01", demo01_command); ("demo02", demo02_command) ]

let () = Command_unix.run ~version:"1.0" ~build_info:"" command
