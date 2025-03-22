open Core

let checksum_from_string buf = Md5.digest_string buf |> Md5.to_hex |> print_endline

let checksum_from_file filename =
  let contents =
    match filename with
    | "-" -> In_channel.input_all In_channel.stdin
    | filename -> In_channel.read_all filename
  in
  Md5.digest_string contents |> Md5.to_hex |> print_endline

(* 
  dune exec real_world_ocaml/ch15_command_line_parsing/ch15.exe demo06 -- -help
  dune exec real_world_ocaml/ch15_command_line_parsing/ch15.exe demo06 -- -s "zhaowei"
  _build/default/real_world_ocaml/ch15_command_line_parsing/ch15.exe demo06 -s "zhaowei"
*)
let command =
  let params =
    let%map_open.Command use_string =
      (* The doc string is formatted so that the first word is the short name that appears in the usage text, with the remainder being the full help text. *)
      flag "-s" (optional string) ~doc:"string Checksum the given string"
    and trial = flag "-t" no_arg ~doc:" run a built-in time trial"
    and filename = anon (maybe_with_default "-" ("filename" %: Filename_unix.arg_type)) in
    fun () ->
      if trial then printf "Running time trial\n"
      else
        match use_string with
        | Some buf -> checksum_from_string buf
        | None -> checksum_from_file filename
  in
  let readme () =
    "This command demonstrates the usage of labeled arguments in OCaml. It can calculate the MD5 \
     checksum of a given string or a file. If no string is provided, it reads from a file or \
     stdin. It also includes a time trial option for performance testing."
  in
  Command.basic ~summary:"Demo the usage of labeled argument" ~readme params
