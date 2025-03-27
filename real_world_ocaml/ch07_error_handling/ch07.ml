open Core

type myerror = NetworkError of string | CmdError of string

let devide x y = if y = 0 then Error (CmdError "Divided by zero") else Ok (x / y)
let add_one x = Ok (x + 1)
let subtract_10 x = Ok (x - 10)
let increment x = Ok (x + 1)

let demo01 =
  let x =
    let open Result.Let_syntax in
    let%bind a = devide 10 0 in
    let%bind b = add_one a in
    let%bind c = subtract_10 b in
    increment c
  in
  match x with
  | Ok v -> v
  | Error x -> (
      match x with NetworkError _msg -> failwith "no network" | CmdError msg -> failwith msg)

let demo02 =
  let ( >>= ) r f = match r with Ok v -> f v | Error msg -> Error msg in
  let return x = Ok x in
  return 11 >>= subtract_10 >>= add_one >>= increment >>= devide 10

let demo03 =
  let ( >>= ) r f = match r with Ok v -> f v | Error msg -> Error msg in
  let return x = Ok x in
  return 11 >>= subtract_10 >>= add_one >>= increment >>= devide 10
