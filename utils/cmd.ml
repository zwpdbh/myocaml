(* run shell command from ocaml *)
let run_command cmd =
  let channel = Unix.open_process_in cmd in
  let output = ref "" in
  try
    while true do
      output := !output ^ input_line channel ^ "\n"
    done;
    !output
  with End_of_file ->
    let status = Unix.close_process_in channel in
    match status with
    | Unix.WEXITED 0 -> !output
    | Unix.WEXITED code -> failwith ("Command exited with code " ^ string_of_int code)
    | Unix.WSIGNALED code -> failwith ("Command killed by signal " ^ string_of_int code)
    | Unix.WSTOPPED code -> failwith ("Command stopped by signal " ^ string_of_int code)

let run cmd =
  try
    let result = run_command cmd in
    print_endline result;
    result
  with Failure msg ->
    prerr_endline ("Error: " ^ msg);
    ""