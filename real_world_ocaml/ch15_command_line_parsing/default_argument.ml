open Core
open Core_unix
open Common

let get_contents = function
  | "-" -> In_channel.input_all In_channel.stdin
  | filename -> In_channel.read_all filename

let do_hash filename = get_contents filename |> Md5.digest_string |> Md5.to_hex |> print_endline

(* A more realistic md5 binary could also read from the standard input if a filename isn’t specified *)
let command =
  Command.basic ~summary:"Demo the usage of default argument command"
    ~readme:(fun () -> "Generate an MD5 hash of the input data")
    (let%map_open.Command filename =
       anon (maybe_with_default "-" ("filename" %: Filename_unix.arg_type))
     in
     fun () -> do_hash filename)
