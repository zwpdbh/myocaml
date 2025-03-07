open Base
open Stdio

(* it depends on the fact that the empty set of frequency counts is represented as an empty list *)
let build_counts_v1 () = In_channel.fold_lines In_channel.stdin ~init:[] ~f:Counter_v1.touch

let demo01 () =
  build_counts_v1 ()
  |> List.sort ~compare:(fun (_, x) (_, y) -> Int.descending x y)
  |> (fun l -> List.take l 10)
  |> List.iter ~f:(fun (line, count) -> printf "%3d: %s\n" count line)

(* See how Counter_v2 hidde the implementation *)
(* It doesn't rely on the ~init:[] must be a list *)
let build_counts_v2 () =
  In_channel.fold_lines In_channel.stdin ~init:Counter_v2.empty ~f:Counter_v2.touch

let demo02 () =
  build_counts_v2 () |> Counter_v2.to_list
  |> List.sort ~compare:(fun (_, x) (_, y) -> Int.descending x y)
  |> (fun l -> List.take l 10)
  |> List.iter ~f:(fun (line, count) -> printf "%3d: %s\n" count line)
