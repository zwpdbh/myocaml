open Core
open Async
open Reader

let process_file filename uppercase =
  Reader.file_contents filename >>= fun text ->
  let processed_text = if uppercase then String.uppercase text else String.lowercase text in
  print_endline processed_text;
  shutdown 0;
  return ()

(* 
 dune exec real_world_ocaml/ch16_concurrent_programming/ch16.exe demo01 -- -help 
 dune exec real_world_ocaml/ch16_concurrent_programming/ch16.exe demo01 -- -f "README.md" 
 or 
 dune exec real_world_ocaml/ch16_concurrent_programming/ch16.exe demo01 -- -f "README.md" -u
*)

let command =
  let params =
    let%map_open.Command file_name = flag "-f" (required string) ~doc:"file The filename to read"
    and uppercase = flag "-u" no_arg ~doc:"Convert to uppercase (default is lowercase)" in
    fun () -> process_file file_name uppercase
  in
  let readme () =
    "This command demonstrates asynchronous file reading in OCaml. It reads the contents of a \
     specified file and prints them to stdout. You can optionally convert the text to uppercase."
  in
  Command.async ~summary:"Demo async read file" ~readme params
