open Core

(* use Yojson.Basic.t type to construct json, its type is:
type json = [
  | `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of json list
  | `Null
  | `String of string
]
*)

(* construct a json value and print it as json string *)
let demo01 =
  let person = `Assoc [ ("name", `String "Anil") ] in
  (* got a string *)
  Yojson.Basic.pretty_to_string person

let demo02 =
  let person = `Assoc [ ("name", `String "Anil") ] in
  Yojson.Basic.pretty_to_channel stdout person
