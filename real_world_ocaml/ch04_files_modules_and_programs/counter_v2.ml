(* For counter v2, we want to abstract the implementation detail of a module *)
(* The implementation details of a module can be hidden by attaching an interface. 
(Note that in the context of OCaml, the terms interface, signature, and module type are all used interchangeably.) 
A module defined by a file filename.ml can be constrained by a signature placed in a file called filename.mli.   *)

open Base

(* for representing a collection of frequency counts. *)
type t = int Map.M(String).t

let empty = Map.empty (module String)
let to_list t = Map.to_alist t

let touch t s =
  let count = match Map.find t s with None -> 0 | Some x -> x in
  Map.set t ~key:s ~data:(count + 1)
