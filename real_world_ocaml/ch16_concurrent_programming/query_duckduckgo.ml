open Core
open Async

(*a small command-line utility for querying DuckDuckGo to extract definitions for a collection of terms. 
It relies on textwrap, uri, yojson, cohttp*)

(* Generate a DuckDuckGo search URI from a query string *)
let query_uri query =
  let base_uri = Uri.of_string "http://api.duckduckgo.com/?format=json" in
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
