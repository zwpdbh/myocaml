(* GADTs to the Rescue *)

(* This `_` represents a type variable that can be different for each constructor of the GADT. It allows the type system to assign specific types to each constructor, which is one of the key features of GADTs. *)
type _ value =
  (* means that when you use the Int constructor, the type parameter becomes int *)
  | Int : int -> int value
  (* means that when you use the Bool constructor, the type parameter becomes bool *)
  | Bool : bool -> bool value

type _ expr =
  (* The colon to the right of each tag is what tells you that this is a GADT. *)
  (* The left-hand side of the arrow states the types of the arguments to the constructor, 
  and the right-hand side determines the type of the constructed value *)
  | Value : 'a value -> 'a expr
  | Eq : int expr * int expr -> bool expr
  | Plus : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr

(* In the definition of each tag in a GADT, the right-hand side of the arrow is an instance of the type of the overall GADT, with independent choices for the type parameter in each case. *)
(* Importantly, the type parameter can depend both on the tag and on the type of the arguments. *)

let i x = Value (Int x)
and b x = Value (Bool x)
and ( +: ) x y = Plus (x, y)

(* These type-safety rules apply not just when constructing an expression, but also when deconstructing one, 
which means we can write a simpler and more concise evaluator that doesn’t need any type-safety checks. *)

(* When use GADTs, type  annotation is needed *)
let eval_value : type a. a value -> a = fun v -> match v with Int x -> x | Bool x -> x

(* When use GADTs, type  annotation is needed *)
let rec eval : type a. a expr -> a =
 fun expr ->
  match expr with
  | Value v -> eval_value v
  | Eq (x, y) -> eval x = eval y
  | Plus (x, y) -> eval x + eval y
  | If (c, t, e) -> if eval c then eval t else eval e
