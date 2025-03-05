open Result

type error =
  | NetworkError of string
  | DatabaseError of string
  | ValidationError of string
  | ApplicationError of string
  | UnknownError of string

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