open Result

type error = ExecutionError of string | SignalError of string | CommandUnknownError of string
type 'a result = ('a, error) Result.t

let error_to_string = function
  | ExecutionError msg -> "ExecutionError: " ^ msg
  | SignalError msg -> "SignalError: " ^ msg
  | CommandUnknownError msg -> "CommandUnknownError: " ^ msg

(* run shell command from ocaml *)
let run_cmd cmd : string result =
  let channel = Unix.open_process_in cmd in
  let output = ref "" in
  try
    while true do
      output := !output ^ input_line channel ^ "\n"
    done;
    Ok !output
  with End_of_file -> (
    let status = Unix.close_process_in channel in
    match status with
    | Unix.WEXITED 0 -> Ok !output
    | Unix.WEXITED code -> Error (ExecutionError ("Command exited with code " ^ string_of_int code))
    | Unix.WSIGNALED code -> Error (SignalError ("Command killed by signal " ^ string_of_int code))
    | Unix.WSTOPPED code ->
        Error (CommandUnknownError ("Command stopped by signal " ^ string_of_int code)))
