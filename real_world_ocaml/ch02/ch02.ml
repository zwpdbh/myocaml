open Base
open Core

(* Anonymous Functions *)
let _ = List.map ~f:(fun x -> x + 1) [ 1; 2; 3 ]
let () = print_endline "Hello, World!"

(* Recursive Function  *)
(* example01  *)
let rec find_first_repeat list =
  match list with
  | [] | [ _ ] ->
      (* only zero or one elements, so no repeats *)
      None
  | x :: y :: tl -> if x = y then Some x else find_first_repeat (y :: tl)

(* example 02:  *)
let rec is_even x = if x = 0 then true else is_odd (x - 1)
and is_odd x = if x = 0 then false else is_even (x - 1)

(* Prefix and Infix Operator *)
(* prefix  *)
let _ = Int.max 3 4

(* infix  *)
let _ = 3 + 4

(* infix can turn to prefix  *)
let _ = 3 + 4

(* let (|>) x f = f x;; reverse application operator *)
let _ = [ 1; 2; 3; 4 ] |> List.map ~f:(fun x -> x + 1)

(* (@@);; The Application Operator, This one is useful for cases where you want to avoid many layers of parentheses 
when applying functions to complex expressions. 
In particular, you can replace f (g (h x)) with f @@ g @@ h x. 
Note that, just as we needed |> to be left associative, we need @@ to be right associative. 
In particular, you can replace f (g (h x)) with f @@ g @@ h x.
*)

(* https://dev.realworldocaml.org/variables-and-functions.html *)
(* Labeled arguments are marked by a leading tilde, and a label (followed by a colon) *)
let _ =
  let ratio ~num ~denom = Float.of_int num /. Float.of_int denom in
  let num = 3 in
  let denom = 4 in
  (* The position is flexiable *)
  ratio ~num:3 ~denom:4 +. ratio ~denom:2 ~num:5
  (* and you get to drop the text after the colon if the name of the label and the name of the variable being used are the same. *)
  +. ratio ~num ~denom

(* https://dev.realworldocaml.org/variables-and-functions.html *)
(* Optional Arguments *)
let example01 =
  let concat ?sep x y =
    let sep = match sep with None -> "" | Some s -> s in
    x ^ sep ^ y
  in
  (concat "hello" "world", concat ~sep:"+" "hello" "world")

let example02 =
  (* provide a default optional argument *)
  let concat ?(sep = "") x y = x ^ sep ^ y in
  (* explicitly passing an optional argument *)
  (concat ~sep:":" "foo" "bar", concat ?sep:(Some ":") "foo" "bar")

let example02 =
  (* calling concat without specifying sep.*)
  let concat ?(sep = "XXX") x y = x ^ sep ^ y in
  let uppercase_concat ?sep a b = concat ?sep (String.uppercase a) b in
  (uppercase_concat "foo" "bar", concat ?sep:None "foo" "bar")
