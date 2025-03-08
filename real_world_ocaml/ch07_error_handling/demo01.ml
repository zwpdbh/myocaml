open Base

(* Result type in OCaml *)
let x = [ Ok 3; Error "abject failure"; Ok 4 ]

(* An Or_error.t is simply a Result.t with the error case specialized to the Error.t type *)
(* The Or_error module provides a bunch of useful operators for constructing errors *)
(* Shows how to convert except except into Or_error *)
let float_of_string s = Or_error.try_with (fun () -> Float.of_string s)
let x = float_of_string "xxx"
let x = float_of_string "3.14"

(* The most common way to create Error.t is using s-expressions , #require "ppx_jane"*)
let x =
  Error.t_of_sexp [%sexp (("List is too long", [ 1; 2; 3 ]) : string * int list)]
  |> Error.to_string_hum

(* transform error  *)
(* Error.tag is useful to augment an error with information about the context of the error*)
(* Error.of_list is useful to combine error *)
let x =
  [ Error.of_string "Your first error"; Error.of_string "Your second error" ]
  |> Error.of_list |> Error.tag ~tag:"total error" |> Error.to_string_hum

(* Common way of generating error is %message syntax extension *)
let x =
  let a = "foo" in
  let b = ("foo", [ 3; 4 ]) in
  Or_error.error_s [%message "a is not b" (a : string) (b : string * int list)]
