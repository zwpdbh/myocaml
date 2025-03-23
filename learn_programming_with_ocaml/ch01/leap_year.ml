open Core
open Async

let read_int_async () =
  match%map Reader.read_line (force Reader.stdin) with
  | `Ok line -> (
      try Ok (Int.of_string line)
      with _ -> Or_error.error_string "Invalid input. Please enter a valid integer.")
  | `Eof -> Or_error.error_string "No input"

let is_leap_year year = (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0

let run () =
  let result =
    let open Deferred.Result.Let_syntax in
    let%map year = read_int_async () in
    let leap = is_leap_year year in
    let msg = if leap then "is" else "is not" in
    print_endline ("Year " ^ Int.to_string year ^ " " ^ msg ^ " a leap year.")
  in
  match%map result with Ok () -> () | Error e -> print_endline ("Error: " ^ Error.to_string_hum e)

(* dune exec learn_programming_with_ocaml/ch01/ch01.exe demo01 *)
let command =
  let params = Command.Param.return (fun () -> run ()) in
  let readme () = "Show leap year" in
  Command.async ~summary:"leap year" ~readme params
