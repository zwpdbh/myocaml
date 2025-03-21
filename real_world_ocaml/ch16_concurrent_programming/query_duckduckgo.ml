open Core
open Async

(*a small command-line utility for querying DuckDuckGo to extract definitions for a collection of terms. 
It relies on textwrap, uri, yojson, cohttp*)

(* Generate a DuckDuckGo search URI from a query string *)
let query_uri query =
  let base_uri = Uri.of_string "https://api.duckduckgo.com/?format=json" in
  Uri.add_query_param base_uri ("q", [ query ])

(* Extract the "Definition" or "Abstract" field from the DuckDuckGo
   results *)
let get_definition_fromn_json json =
  match Yojson.Safe.from_string json with
  | `Assoc kv_list -> (
      let find key =
        match List.Assoc.find ~equal:String.equal kv_list key with
        | None | Some (`String "") -> None
        | Some s -> Some (Yojson.Safe.to_string s)
      in
      match find "Abstract" with Some _ as x -> x | None -> find "Definition")
  | _ -> None

(* Execute the DuckDuckGo search *)
let get_definition word =
  let%bind _, body = Cohttp_async.Client.get (query_uri word) in
  let%map string = Cohttp_async.Body.to_string body in
  (word, get_definition_fromn_json string)

let get_definition word =
  let%bind _, body = Cohttp_async.Client.get (query_uri word) in
  let%map string = Cohttp_async.Body.to_string body in
  let the_definition = get_definition_fromn_json string in
  (word, the_definition)

(* Print out a word/definition pair *)
let print_result (word, definition) =
  printf "%s\n %s\n \n %s\n \n" word
    (String.init (String.length word) ~f:(fun _ -> '-'))
    (match definition with
    | None -> "No definition found"
    | Some def -> String.concat ~sep:"\n" (Wrapper.wrap (Wrapper.make 70) def))

(* Run many searches in parallel, printing out the results after
   they're all done. *)
let search_and_print words =
  let%map results = Deferred.all (List.map words ~f:get_definition) in
  List.iter results ~f:print_result

let search_and_printv2 words =
  Deferred.all_unit (List.map words ~f:(fun word -> get_definition word >>| print_result))

(* dune exec real_world_ocaml/ch16_concurrent_programming/ch16.exe demo04 -- -search "ocaml functional","programming" *)
let command =
  (* 1. create Command.Param *)
  let params =
    let%map_open.Command words =
      flag "search"
        (required (Arg_type.comma_separated string))
        ~doc:"WORDS Comma-separated list of words to search for"
    in
    fun () -> search_and_printv2 words
  in
  (* 2. create readme *)
  let readme () = "Search for definitions of words using DuckDuckGo API." in
  Command.async ~summary:"Search for word definitions" ~readme params
