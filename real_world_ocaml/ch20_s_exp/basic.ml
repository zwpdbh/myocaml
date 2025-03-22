open Core

let x =
  let s =
    Sexp.List
      [
        Sexp.Atom "this";
        Sexp.List [ Sexp.Atom "is"; Sexp.Atom "an" ];
        Sexp.List [ Sexp.Atom "s"; Sexp.Atom "expression" ];
      ]
  in
  Sexp.to_string_hum s

(* Convert ordinary types into s-exp *)
let _x = Int.sexp_of_t 3
let _x = String.sexp_of_t "hello"
let _x = Exn.sexp_of_t (Invalid_argument "hello")
let _x = List.sexp_of_t Int.sexp_of_t [ 1; 2; 3 ]

(* Convert s-exp into ordinary types *)
let _x = Int.t_of_sexp (Sexp.Atom "3")
let _x = Int.t_of_sexp (Sexp.List [ Sexp.Atom "3" ])
let _x = List.t_of_sexp Int.t_of_sexp (Sexp.of_string "(1 2 3)")
let _exception = List.t_of_sexp Int.t_of_sexp (Sexp.of_string "(1 2 three)")
