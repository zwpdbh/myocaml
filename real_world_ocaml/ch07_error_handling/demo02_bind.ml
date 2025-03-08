(* Consider the compute_bounds function below, which takes a list and a comparison function 
and returns upper and lower bounds for the list by finding the smallest and largest element on the list.*)
open Base

let compute_bounds_v1 ~compare list =
  let sorted = List.sort ~compare list in
  match (List.hd sorted, List.last sorted) with
  | None, _ | _, None -> None
  | Some x, Some y -> Some (x, y)

(* bind can be used as a way of sequencing together error-producing functions 
so that the first one to produce an error terminates the computation. *)
let compute_bounds_v2 ~compare list =
  let sorted = List.sort ~compare list in
  Option.bind (List.hd sorted) ~f:(fun first ->
      Option.bind (List.last sorted) ~f:(fun last -> Some (first, last)))

(* use Option.Monad_infix *)
let compute_bounds_v3 ~compare list =
  let open Option.Monad_infix in
  let sorted = List.sort ~compare list in
  List.hd sorted >>= fun first ->
  List.last sorted >>= fun last -> Some (first, last)

(* use Let_syntax, #require "ppx_let" *)
let compute_bounds_v4 ~compare list =
  let open Option.Let_syntax in
  let sorted = List.sort ~compare list in
  let%bind first = List.hd sorted in
  let%bind last = List.last sorted in
  Some (first, last)
