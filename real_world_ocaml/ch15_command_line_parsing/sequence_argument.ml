open Core

let get_contents = function
  | "-" -> In_channel.input_all In_channel.stdin
  | filename -> In_channel.read_all filename

let do_hash filename =
  get_contents filename |> Md5.digest_string |> Md5.to_hex |> fun md5 ->
  printf "MD5 (%s) = %s\n" filename md5

let command =
  Command.basic ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command files = anon (sequence ("filename" %: Filename_unix.arg_type)) in
     fun () -> match files with [] -> do_hash "-" | _ -> List.iter files ~f:do_hash)

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
