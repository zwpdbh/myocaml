(* https://dev.realworldocaml.org/error-handling.html *)

open Base

(* How to define your own exception.*)
exception Key_not_found of string

let _ =
  let lookup key map =
    match Map.find map key with Some value -> value | None -> raise (Key_not_found key)
  in
  lookup "keyx" (Map.of_alist_exn (module String) [ ("key1", "value1"); ("key2", "value2") ])

let rec find_exn alist key =
  match alist with
  | [] -> raise (Key_not_found key)
  | (key', data) :: tl -> if String.( = ) key key' then data else find_exn tl key

(* Exceptions are ordinary values. 
Exceptions are all of the same type, exn, which is itself something of a special case in the OCaml type system *)
let _ =
  let exceptions = [ Division_by_zero; Key_not_found "b" ] in
  List.filter exceptions ~f:(function Key_not_found _ -> true | _ -> false)

(* How to handle exceptions *)

(* Excpetion handler *)
(* an exception handler is declared using a try/with expression *)

(* try <expr> with
| <pat1> -> <expr1>
| <pat2> -> <expr2> *)

(* Exn.protect *)
(* Exn.protect function, which takes two arguments: 
a thunk f, which is the main body of the computation to be run; 
and a thunk finally, which is to be called when f exits, whether it exits normally or with an exception *)
let load filename =
  let open Stdio in
  let parse_line line = String.split_on_chars ~on:[ ',' ] line |> List.map ~f:Float.of_string in
  let inc = In_channel.create filename in
  Exn.protect
    ~f:(fun () -> In_channel.input_lines inc |> List.map ~f:parse_line)
    ~finally:(fun () -> In_channel.close inc)

(* From Exception to Error-Aware Types and Back *)

(* can capture that exception into an option  *)
let x =
  let find alist key = Option.try_with (fun () -> find_exn alist key) in
  find [] "key1"

(* Result and Or_error have similar try_with functions *)
let x =
  let find alist key =
    (* Exception to Result *)
    Or_error.try_with (fun () -> find_exn alist key)
  in
  find [ ("a", 1); ("b", 2) ] "c"
  (* Result to exception *)
  |> Or_error.ok_exn
