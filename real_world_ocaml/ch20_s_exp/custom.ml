open Core

(* S-Expression Converters for New Types*)
type t = { foo : int; bar : float }

let sexp_of_t t =
  let a x = Sexp.Atom x and l x = Sexp.List x in
  l [ l [ a "foo"; Int.sexp_of_t t.foo ]; l [ a "bar"; Float.sexp_of_t t.bar ] ]
;;

sexp_of_t { foo = 3; bar = -5.5 }

(* Use ppx_sexp_conv from ppx_jane *)
type t = { foo : int; bar : float } [@@deriving sexp];;

t_of_sexp (Sexp.of_string "((bar 35) (foo 3))")

(* [@@deriving sexp] can be attached to the declaration of an exception to improve the quality of errors printed by OCaml’s top-level exception handler.  *)
exception Ordinary_exn of string list
exception Exn_with_sexp of string list [@@deriving sexp]

let x =
  let some_fun () = raise (Exn_with_sexp [ "hello"; "world" ]) in
  try some_fun () with Exn_with_sexp x -> x

let x =
  let some_fun () = raise (Ordinary_exn [ "hello"; "world" ]) in
  try some_fun () with Ordinary_exn x -> x
