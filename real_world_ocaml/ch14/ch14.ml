open Base

(* Create a custom module and use functor to make it satisfy the Comparator.S *)
module Book = struct
  module T = struct
    type t = { title : string; isbn : string }

    let compare t1 t2 =
      let cmp_title = String.compare t1.title t2.title in
      if cmp_title <> 0 then cmp_title else String.compare t1.isbn t2.isbn

    let sexp_of_t t : Sexp.t = List [ Atom t.title; Atom t.isbn ]
  end

  (* we use functor by 
  1. Create a submodule, called T containing the basic functionality for the type,  Book 
  2. and then include both that module and the result of applying a functor to that module.*)
  include T
  include Comparator.Make (T)
end

(*With this module in hand, we can now build a set of Book.t’s  *)
let some_programming_books =
  Set.of_list
    (module Book)
    [
      { title = "Real World OCaml"; isbn = "978-1449323912" };
      { title = "Structure and Interpretation of Computer Programs"; isbn = "978-0262510875" };
      { title = "The C Programming Language"; isbn = "978-0131101630" };
    ]
