open Error 

(* run shell command from ocaml *)
let run_cmd cmd =
  let channel = Unix.open_process_in cmd in
  let output = ref "" in
  try
    while true do
      output := !output ^ input_line channel ^ "\n"
    done;
    Ok !output
  with End_of_file ->
    let status = Unix.close_process_in channel in
    match status with
    | Unix.WEXITED 0 -> Ok !output
   | Unix.WEXITED code -> 
        Error (command_error_to_error (ExecutionError ("Command exited with code " ^ string_of_int code)))
    | Unix.WSIGNALED code -> 
        Error (command_error_to_error (SignalError ("Command killed by signal " ^ string_of_int code)))
    | Unix.WSTOPPED code -> 
        Error (command_error_to_error (CommandUnknownError ("Command stopped by signal " ^ string_of_int code)))
