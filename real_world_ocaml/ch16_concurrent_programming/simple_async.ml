open Core
open Async
open Reader

(* Deferred.bind d ~f causes f to be called after the value of d has been determined. *)
(* Together, bind and return form a design pattern in functional programming known as a monad. *)
let process_file_v1 filename uppercase =
  Deferred.bind (Reader.file_contents filename) ~f:(fun text ->
      let processed_text = if uppercase then String.uppercase text else String.lowercase text in
      print_endline processed_text;
      shutdown 0;
      return ())

(*  Infix operator >>= is a shorthand for Deferred.bind. *)
let process_file_v2 filename uppercase =
  Reader.file_contents filename >>= fun text ->
  let processed_text = if uppercase then String.uppercase text else String.lowercase text in
  print_endline processed_text;
  shutdown 0;
  (* return, it is a function provided by Async that takes an ordinary value and wrap it up in a deferred. *)
  return ()

(* #require "ppx_let";; 
  Opening Async also implicitly opens Deferred.Let_syntax
*)
let process_file_v3 filename uppercase =
  let%bind text = Reader.file_contents filename in
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
    fun () -> process_file_v3 file_name uppercase
  in
  let readme () =
    "This command demonstrates asynchronous file reading in OCaml. It reads the contents of a \
     specified file and prints them to stdout. You can optionally convert the text to uppercase."
  in
  Command.async ~summary:"Demo async read file" ~readme params
