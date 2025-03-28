open Core

let add =
  Command.basic ~summary:"Add [days] to the [base] date"
    Command.Let_syntax.(
      let%map_open base = anon ("base" %: date) and days = anon ("days" %: int) in
      fun () -> Date.add_days base days |> Date.to_string |> print_endline)

let diff =
  Command.basic ~summary:"Show days between [date1] and [date2]"
    (let%map_open.Command date1 = anon ("date1" %: date) and date2 = anon ("date2" %: date) in
     fun () -> Date.diff date1 date2 |> printf "%d days\n")

(* a calendar tool that does a few operations over dates from the command line. *)
let command = Command.group ~summary:"Manipulate dates" [ ("add", add); ("diff", diff) ]
