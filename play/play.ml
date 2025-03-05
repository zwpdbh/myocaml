open Utils.Error
open Utils.Cmd

let handle_command cmd =
  match run_cmd cmd with
  | Ok output -> 
      print_endline "Command executed successfully:";
      print_endline output
  | Error (CommandError (ExecutionError msg)) ->
      prerr_endline ("Execution error: " ^ msg)
  | Error (CommandError (SignalError msg)) ->
      prerr_endline ("Signal error: " ^ msg)
  | Error (CommandError (CommandUnknownError msg)) ->
      prerr_endline ("Unknown command error: " ^ msg)
  | Error other_error ->
      prerr_endline ("Other error: " ^ to_string (Error other_error))

let () =
  handle_command "ls -l"