open Base

type value = Int of int | Bool of bool
type expr = Value of value | Eq of expr * expr | Plus of expr * expr | If of expr * expr * expr

exception Ill_typed

let rec eval expr =
  match expr with
  | Value v -> v
  | If (c, t, e) -> (
      match eval c with Bool b -> if b then eval t else eval e | Int _ -> raise Ill_typed)
  | Eq (x, y) -> (
      match (eval x, eval y) with
      | Bool _, _ | _, Bool _ -> raise Ill_typed
      | Int f1, Int f2 -> Bool (f1 = f2))
  | Plus (x, y) -> (
      match (eval x, eval y) with
      | Bool _, _ | _, Bool _ -> raise Ill_typed
      | Int f1, Int f2 -> Int (f1 + f2))

(* it’s entirely possible to create an ill-typed expression which will trip these checks. *)
let _ = Plus (Value (Int 3), Value (Bool false))
