open Base
open Core

(* Anonymous Functions *)
let _ = List.map ~f:(fun x -> x + 1) [ 1; 2; 3 ]
let () = print_endline "Hello, World!"
