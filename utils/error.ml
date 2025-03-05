open Result

type error = NetworkError of string | CmdError of string
type 'a result = ('a, error) Result.t

let to_string = function
  | Ok v -> "Ok: " ^ v
  | Error e -> (
      match e with
      | NetworkError msg -> "Network Error: " ^ msg
      | CmdError msg -> "CmdError: " ^ msg)
