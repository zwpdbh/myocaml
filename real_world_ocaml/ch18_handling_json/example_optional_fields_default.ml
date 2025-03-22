open Core

type person = {
  person_id : int; [@key "id"]
  person_name : string; [@key "name"]
  description : string option; [@default None]
}
[@@deriving yojson]

let json_string = {|
{
  "id": 42,
  "name": "Test"
}
|}

let yojson = Yojson.Safe.from_string json_string

let person_value =
  match person_of_yojson yojson with
  | Ok value -> value
  | Error err -> failwith ("JSON parse error: " ^ err)

let () =
  match person_value.description with
  | Some desc -> Format.printf "Description: %s\n" desc
  | None -> Format.printf "No description provided\n"
