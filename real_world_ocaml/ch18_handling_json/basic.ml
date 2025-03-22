open Core

let parse_json file_name =
  let buf = In_channel.read_all file_name in
  let json1 = Yojson.Basic.from_string buf in
  let json2 = Yojson.Basic.from_file file_name in
  print_endline (if Yojson.Basic.equal json1 json2 then "OK" else "FAIL")

(* dune exec real_world_ocaml/ch18_handling_json/ch18.exe demo01 -- -f "real_world_ocaml/ch18_handling_json/book.json" *)
let command_sync =
  (* 1. create Command.Param *)
  let params =
    let%map_open.Command file_name = flag "-f" (required string) ~doc:"Parse Json from file_name" in
    fun () -> parse_json file_name
  in
  (* 2. create readme *)
  let readme () = "Parse json by reading from json file" in
  Command.basic ~summary:"Command summary" ~readme params

let parse_json_async file_name =
  let open Async in
  let%bind buf = Reader.file_contents file_name in
  let json1 = Yojson.Basic.from_string buf in
  let%bind json2 =
    Reader.with_file file_name ~f:(fun reader ->
        let%map contents = Reader.contents reader in
        Yojson.Basic.from_string contents)
  in
  let result = if Yojson.Basic.equal json1 json2 then "OK" else "FAIL" in
  printf "%s\n" result;
  return ()

(* dune exec real_world_ocaml/ch18_handling_json/ch18.exe demo02 -- -f "real_world_ocaml/ch18_handling_json/book.json" *)
let command_async =
  let open Async in
  (* 1. create Command.Param *)
  let params =
    let%map_open.Command file_name = flag "-f" (required string) ~doc:"Parse Json from file_name" in
    fun () -> parse_json_async file_name
  in
  (* 2. create readme *)
  let readme () = "Parse json by reading from json file" in
  Command.async ~summary:"Command summary" ~readme params

(* Selecting values from JSON structure *)
let demo_selecting_value () =
  (* Read the JSON file *)
  let json = Yojson.Basic.from_file "real_world_ocaml/ch18_handling_json/book.json" in

  (* Locally open the JSON manipulation functions *)
  let open Yojson.Basic.Util in
  let title = json |> member "title" |> to_string in
  (* First, we convert the JSON List to an OCaml list of JSON values and then filter out the String values as an OCaml string list. *)
  let tags = json |> member "tags" |> to_list |> filter_string in
  let pages = json |> member "pages" |> to_int in
  let is_online = json |> member "is_online" |> to_bool_option in
  let is_translated = json |> member "is_translated" |> to_bool_option in
  let authors = json |> member "authors" |> to_list in
  let names = List.map authors ~f:(fun json -> member "name" json |> to_string) in

  (* Print the results of the parsing *)
  printf "Title: %s (%d)\n" title pages;
  printf "Authors: %s\n" (String.concat ~sep:", " names);
  printf "Tags: %s\n" (String.concat ~sep:", " tags);
  let string_of_bool_option = function
    | None -> "<unknown>"
    | Some true -> "yes"
    | Some false -> "no"
  in
  printf "Online: %s\n" (string_of_bool_option is_online);
  printf "Translated: %s\n" (string_of_bool_option is_translated)
