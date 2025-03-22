open Core

let json_string =
  {|
{
  "user": {
    "id": 123,
    "name": "Alice",
    "contact": {
      "email": "alice@example.com",
      "phone": "123456789"
    },
    "books": [
      {
        "title": "OCaml programming","author": "someone01", "isbn": "xxxx01"
      },
      {
        "title": "OCaml practise","author": "someone02", "isbn": "xxxx02"
      }
    ]
  },
  "active": true
}
|}

type book = { title : string; author : string; id : string [@key "isbn"] } [@@deriving yojson]
type contact = { email : string; phone : string } [@@deriving yojson]
type user = { id : int; name : string; contact : contact; books : book list } [@@deriving yojson]
type root = { user : user; active : bool } [@@deriving yojson]

let print_user_info root =
  let user = root.user in
  Printf.printf "User: %s (ID: %d)\n" user.name user.id;
  Printf.printf "Email: %s\n" user.contact.email;
  Printf.printf "Phone: %s\n" user.contact.phone;
  Printf.printf "Books:\n";
  List.iter
    ~f:(fun book -> Printf.printf "  - %s by %s (ID: %s)\n" book.title book.author book.id)
    user.books

let demo03 () =
  try
    let yojson = Yojson.Safe.from_string json_string in
    let root_value =
      match root_of_yojson yojson with
      | Ok root -> root
      | Error msg -> failwith ("JSON parsing error: " ^ msg)
    in
    print_user_info root_value
  with
  | Yojson.Json_error msg ->
      Printf.eprintf "->> Error parsing JSON: %s\n" msg;
      Printf.eprintf "Please check your JSON string for syntax errors.\n"
  | exn -> Printf.eprintf "->> An unexpected error occurred: %s\n" (Exn.to_string exn)

(* dune exec real_world_ocaml/ch18_handling_json/ch18.exe demo03 *)
let command_for_demo_json =
  let params = Command.Param.return (fun () -> demo03 ()) in
  let readme () = "use ppx_deriving_yojson to parse json" in
  Command.basic ~summary:"use ppx_deriving_yojson to parse json" ~readme params

let command =
  Command.group ~summary:"Chapter 18 process json"
    [
      ("demo01", Basic.command_sync);
      ("demo02", Basic.command_async);
      ("demo03", command_for_demo_json);
    ]

let () = Command_unix.run ~version:"1.0" ~build_info:"" command
