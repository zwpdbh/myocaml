open Core

(* The do_hash function accepts a filename parameter and prints the human-readable MD5 string to the console standard output. *)
let do_hash file = Md5.digest_file_blocking file |> Md5.to_hex |> print_endline

(* After define a specification, put it to work by create a command-line interface *)
let command =
  Command.basic ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (Command.Param.map Args.filename_param ~f:(fun filename () -> do_hash filename))
