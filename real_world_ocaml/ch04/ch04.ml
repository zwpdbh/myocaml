(* grep -Eo '[[:alpha:]]+' freq.ml | dune exec real_world_ocaml/ch04/ch04.exe *)
open Base
open Stdio

(* a utility that reads lines from stdin, computes a frequency count of the lines, and prints out the ten most frequent lines *)

let build_counts () = In_channel.fold_lines In_channel.stdin ~init:[] ~f:Counter.touch

let () =
  build_counts ()
  |> List.sort ~compare:(fun (_, x) (_, y) -> Int.descending x y)
  |> (fun l -> List.take l 10)
  |> List.iter ~f:(fun (line, count) -> printf "%3d: %s\n" count line)
