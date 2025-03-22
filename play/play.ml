open Utils

let run_command cmd =
  match Cmd.run_cmd cmd with
  | Ok output ->
      print_endline "Command executed successfully:";
      print_endline output
  | Error e ->
      let error_msg = Cmd.error_to_string e in
      print_endline ("Error: " ^ error_msg)

let () = run_command "ls -zzz"
