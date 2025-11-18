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

(* how bind works behind the scene *)
let my_bind d ~f =
  let i = Ivar.create () in
  upon d (fun x -> upon (f x) (fun y -> Ivar.fill_exn i y));
  Ivar.read i

(* Here’s roughly what happens when you write let d' = Deferred.bind d ~f.
A new ivar i is created to hold the final result of the computation. The corresponding deferred is returned
A function is registered to be called when the deferred d becomes determined.
That function, once run, calls f with the value that was determined for d.
Another function is registered to be called when the deferred returned by f becomes determined.
When that function is called, it uses it to fill i, causing the corresponding deferred it to become determined. *)

(* The differences between let%bind and let%map *)
let example1 () =
  let%bind x = Deferred.return 1 in
  let%bind y = Deferred.return (x + 1) in
  Deferred.return (y + 1)

let example2 () =
  let%bind x = Deferred.return 1 in
  let%map y = Deferred.return (x + 1) in
  y + 1

let run () =
  let%bind result1 = example1 () in
  let%bind result2 = example2 () in
  printf "Result 1: %d\n" result1;
  printf "Result 2: %d\n" result2;
  Deferred.unit

let command_for_let_binding =
  let params = Command.Param.return (fun () -> run ()) in
  let readme () =
    "This command demonstrates the difference between let%bind and let%map in OCaml's Async. It \
     runs two example functions and prints their results."
  in
  Command.async ~summary:"Demo let%bind vs let%map" ~readme params
