open Core
open Core_unix
open Common

(* Define an Annonymous Argument *)
let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

(* After define a specification, put it to work by create a command-line interface *)
let command_deprecate_v1 =
  Command.basic ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (Command.Param.map filename_param ~f:(fun filename () -> do_hash_v1 filename))

let command_deprecate_v2 =
  Command.basic ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    Command.Param.(
      map
        (both (anon ("hash_length" %: int)) (anon ("filename" %: string)))
        ~f:(fun (hash_length, filename) () -> do_hash_v2 hash_length filename))

(* An expanded version from above command  *)
let command_deprecate_v3 =
  let open Command.Param in
  (* Define the individual parameters *)
  let hash_length_param = anon ("hash_length" %: int) in
  let filename_param = anon ("filename" %: string) in

  (* Combine the parameters *)
  let combined_params = both hash_length_param filename_param in

  (* Define the function that will be called with the parsed arguments *)
  let execute_function (hash_length, filename) () = do_hash_v2 hash_length filename in

  (* Map the combined parameters to the execute function *)
  let param = map combined_params ~f:execute_function in

  (* Create the command *)
  Command.basic ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    param

(* let-syntax’s support for parallel let bindings *)
let command_deprecate_v4 =
  Command.basic ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map hash_length = anon ("hash_length" %: int) and filename = anon ("filename" %: string) in
     fun () -> do_hash_v2 hash_length filename)

(*  use let%map_open to automatically open Command.Let_syntax and Command.Param *)
let command =
  (* ch15_command_line_parsing/ch15.exe demo01 -- -help *)
  Command.basic ~summary:"Demo the usage of annonymous argument"
    ~readme:(fun () -> "Generate an MD5 hash of the input data")
    (let%map_open.Command hash_length = anon ("hash_length" %: int)
     and filename = anon ("filename" %: string) in
     fun () -> do_hash_v2 hash_length filename)

let command =
  Command.group ~summary:"Show different ways to build command"
    [
      ("demo01", command); ("demo02", Custom_argument.command); ("demo03", Default_argument.command);
    ]

let () = Command_unix.run ~version:"1.0" ~build_info:"" command
