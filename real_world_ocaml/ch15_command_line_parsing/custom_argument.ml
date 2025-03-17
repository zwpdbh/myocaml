open Core
open Core_unix
open Common
(* Define an argument type such that it could ensures that the input file isn't a character device or some other odd unix file type that can't be fully read *)

let regular_file =
  Command.Arg_type.create (fun filename ->
      match Sys_unix.is_file filename with
      | `Yes -> filename
      | `No -> failwith "not a regular file"
      | `Unknown -> failwith "could not determine if the file was a regular file")

let command =
  Command.basic ~summary:"Demo the usage of custom argument command"
    ~readme:(fun () -> "Generate an MD5 hash of the input data")
    (let%map_open.Command filename = anon ("filename" %: regular_file) in
     fun () -> do_hash_v1 filename)

(* let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command *)
