open Result

type error =
  | NetworkError of string
  | DatabaseError of string
  | ValidationError of string
  | ApplicationError of string
  | UnknownError of string
  | CommandError of command_error

and command_error =
  | ExecutionError of string
  | SignalError of string
  | CommandUnknownError of string

type 'a result = ('a, error) Result.t

let to_string = function
  | Ok v -> "Ok: " ^ v
  | Error e ->
    match e with
    | NetworkError msg -> "Network Error: " ^ msg
    | DatabaseError msg -> "Database Error: " ^ msg
    | ValidationError msg -> "Validation Error: " ^ msg
    | ApplicationError msg -> "Application Error: " ^ msg
    | UnknownError msg -> "Unknown Error: " ^ msg
    | CommandError ce ->
      match ce with
      | ExecutionError msg -> "Command Execution Error: " ^ msg
      | SignalError msg -> "Command Signal Error: " ^ msg
      | CommandUnknownError msg -> "Command Unknown Error: " ^ msg

let command_error_to_error = function
  | ExecutionError msg -> CommandError (ExecutionError msg)
  | SignalError msg -> CommandError (SignalError msg)
  | CommandUnknownError msg -> CommandError (CommandUnknownError msg)