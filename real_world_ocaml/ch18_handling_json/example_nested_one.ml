open Core

(* Define the nested contact record *)
type contact = { email : string; phone : string } [@@deriving yojson]

(* Define the user record containing a contact field *)
type user = { id : int; name : string; contact : contact } [@@deriving yojson]

(* Define the top-level record *)
type root = { user : user; active : bool } [@@deriving yojson]

let json_string =
  {|
{
  "user": {
    "id": 123,
    "name": "Alice",
    "contact": {
      "email": "alice@example.com",
      "phone": "123456789"
    }
  },
  "active": true
}
|}

(* Parse the JSON string into a Yojson.Safe.t value *)
let yojson = Yojson.Safe.from_string json_string

(* Convert Yojson.Safe.t into our OCaml type *)
let root_value =
  match root_of_yojson yojson with
  | Ok value -> value
  | Error err -> failwith ("JSON parse error: " ^ err)

let () =
  Format.printf "User %s is %s\n" root_value.user.name
    (if root_value.active then "active" else "inactive")
