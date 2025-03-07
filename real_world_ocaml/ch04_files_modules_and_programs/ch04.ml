(* https://dev.realworldocaml.org/files-modules-and-programs.html#multi-file-programs-and-modules *)
(* grep -Eo '[[:alpha:]]+' freq.ml | dune exec real_world_ocaml/ch04/ch04.exe *)

let _ = Freq.demo01 ()

(* Demo open module *)
let print_median m =
  (* Prefer Local Opens *)
  let open Core in
  (* Using Module Shortcuts Instead *)
  let module C = Counter_v3 in
  match m with
  | C.Median string -> printf "True median:\n   %s\n" string
  | C.Before_and_after (before, after) ->
      printf "Before and after median:\n   %s\n   %s\n" before after

let _ =
  let open Base in
  let open Import in
  (* Then, by opening Import, we can shadow Base’s Option module with our extension. *)
  let _lookup_and_apply map key x = Option.apply (Map.find map key) x in
  3 + 4
