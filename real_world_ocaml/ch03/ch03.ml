open Base
open Core
open Core_bench
(* Lists and Patterns *)
(* https://dev.realworldocaml.org/lists-and-patterns.html *)

let _ =
  let rec drop_value l to_drop =
    match l with
    | [] -> []
    | hd :: tl ->
        let new_tl = drop_value tl to_drop in
        if hd = to_drop then new_tl else hd :: new_tl
  in
  drop_value [ 1; 2; 3; 4 ] 2

let _ =
  let rec drop_value l to_drop =
    match l with
    | [] -> []
    | hd :: tl -> if hd = to_drop then drop_value tl to_drop else hd :: drop_value tl to_drop
  in
  drop_value [ 1; 2; 3; 4 ] 2

(* Using the List Module Effectively *)
(* Example: render_table that, given a list of column headers and a list of rows, prints them out in a well-formatted text table. *)

(* compute the maximum width of each column of data *)
let max_widths headers rows =
  let lengths l = List.map ~f:String.length l in
  List.fold rows ~init:(lengths headers) ~f:(fun acc row ->
      List.map2_exn ~f:Int.max acc (lengths row))

(* Now that we know how to compute column widths, we can write the code to 
generate the line that separates the header from the rest of the text table. *)
let render_separator widths =
  let pieces = List.map widths ~f:(fun w -> String.make w '-') in
  "|-" ^ String.concat ~sep:"-+-" pieces ^ "-|"

(* Now we need code for rendering a row with data in it. *)
let pad s length = s ^ String.make (length - String.length s) ' '

(* render a row of data *)
let render_row row widths =
  let padded = List.map2_exn row widths ~f:pad in
  "|" ^ String.concat ~sep:"|" padded ^ "|"

let render_table header rows =
  let widths = max_widths header rows in
  String.concat ~sep:"\n"
    (render_row header widths :: render_separator widths
    :: List.map rows ~f:(fun row -> render_row row widths))

let _ =
  Stdio.print_endline
    (render_table
       [ "language"; "architect"; "first release" ]
       [
         [ "Lisp"; "John McCarthy"; "1958" ];
         [ "C"; "Dennis Ritchie"; "1969" ];
         [ "ML"; "Robin Milner"; "1973" ];
         [ "OCaml"; "Xavier Leroy"; "1996" ];
       ])

(* Combining List Elements with List.reduce *)
let _ = List.reduce ~f:( + ) [ 1; 2; 3; 4; 5 ]

(* filter and filter map *)
let _ = List.filter ~f:(fun x -> x mod 2 = 0) [ 1; 2; 3; 4; 5 ]
let _ = List.filter_map ~f:(fun x -> if x mod 2 = 0 then Some x else None) [ 1; 2; 3; 4; 5 ]
