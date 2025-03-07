(* https://dev.realworldocaml.org/variants.html#combining-records-and-variants *)
(* Variants and Recursive Data Structures *)

(* a simple Boolean expression language 
Such a language can be useful anywhere you need to specify filters, 
which are used in everything from packet analyzers to mail clients. *)

type 'a expr =
  (* The Base tag is what allows you to tie the expr to your application, 
by letting you specify an element of some base predicate type, 
whose truth or falsehood is determined by your application. *)
  | Base of 'a
  | Const of bool
  | And of 'a expr list
  | Or of 'a expr list
  | Not of 'a expr

(* If you were writing a filter language for an email processor, 
your base predicates might specify the tests you would run against an email, as in the following example: *)

type mail_field = To | From | CC | Date | Subject
type mail_predicate = { field : mail_field; contains : string }

(* construct a simple expression with mail_predicate as its base predicate: *)

let test field contains = Base { field; contains }
let _some_predicate = And [ Or [ test To "doligez"; test CC "doligez" ]; test Subject "runtime" ]

(* Being able to construct such expressions isn’t enough; we also need to be able to evaluate them. *)

let rec eval expr base_eval =
  (* a shortcut, so we don't need to repeatedly pass [base_eval]
     explicitly to [eval] *)
  let eval' expr = eval expr base_eval in
  match expr with
  | Base base -> base_eval base
  | Const bool -> bool
  | And exprs -> List.for_all exprs ~f:eval'
  | Or exprs -> List.exists exprs ~f:eval'
  | Not expr -> not (eval' expr)
