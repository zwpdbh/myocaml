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
let get_definition_from_json json =
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

let get_definition ~interrupt word =
  match%map
    try_with (fun () ->
        let%bind _, body = Cohttp_async.Client.get ~interrupt (query_uri word) in
        let%map string = Cohttp_async.Body.to_string body in
        let the_definition = get_definition_from_json string in
        (word, the_definition))
  with
  | Ok (word, definition) -> (word, Ok definition)
  | Error _ -> (word, Error "unexpected failure")

let get_definition_with_timeout_v1 word =
  let timeout_duration = sec 5.0 in
  match%map get_definition ~interrupt:(after timeout_duration) word with
  | word, Error _ -> (word, Error "Unexpected failure")
  | word, (Ok _ as x) -> (word, x)

(* Improved from v1 that now we could distinguish between exception error and timeout error *)
(* Use Ivar is important:
In essence, the Ivar here acts as a one-time, asynchronous flag. It's a way to send a signal from one part of the asynchronous computation (the timeout logic) to another (the get_definition function) without requiring synchronous communication or polling. This pattern allows for efficient, non-blocking cancellation of asynchronous operations in OCaml's Async library.
*)
let get_definition_with_timeout_v2 word =
  let timeout_duration = sec 5.0 in
  let interrupt = Ivar.create () in
  choose
    [
      choice (after timeout_duration) (fun () ->
          (* when timeout, interrupt is filled, become determined *)
          Ivar.fill interrupt ();
          (word, Error "Timed out"));
      choice
        (* If the timeout occurs first, the interrupt Ivar is filled, which then allows the interrupt deferred in get_definition to complete, triggering an early return with an error. *)
        (get_definition ~interrupt:(Ivar.read interrupt) word)
        (fun (word, result) ->
          let result' =
            match result with Ok _ as x -> x | Error _ -> Error "Unexpected failure"
          in
          (word, result'));
    ]

(* Print out a word/definition pair *)
let print_result (word, definition) =
  printf "%s\n%s\n\n%s\n\n" word
    (String.init (String.length word) ~f:(fun _ -> '-'))
    (match definition with
    | Error s -> "DuckDuckGo query failed: " ^ s
    | Ok None -> "No definition found"
    | Ok (Some def) -> String.concat ~sep:"\n" (Wrapper.wrap (Wrapper.make 70) def))

(* Run many searches in parallel, printing out the results after
   they're all done. *)
let search_and_print words =
  let%map results = Deferred.all (List.map words ~f:get_definition_with_timeout_v1) in
  List.iter results ~f:print_result

let search_and_printv2 words =
  Deferred.all_unit
    (List.map words ~f:(fun word -> get_definition_with_timeout_v1 word >>| print_result))

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

let handle_error () =
  match%map
    try_with (fun () ->
        let%map () = after (Time_float.Span.of_sec 0.1) in
        raise Exit)
  with
  | Ok () -> printf "succeed"
  | Error _ -> printf "error"

(* *)
let command_show_async_exception =
  let params = Command.Param.return (fun () -> handle_error ()) in
  let readme () = "Demo exception handling in async" in
  Command.async ~summary:"async exception handling" ~readme params
