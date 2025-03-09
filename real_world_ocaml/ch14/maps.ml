open Base
(* association list as map *)

let digit_alist =
  [
    (0, "zero");
    (1, "one");
    (2, "two");
    (3, "three");
    (4, "four");
    (5, "five");
    (6, "six");
    (7, "seven");
    (8, "eight");
    (9, "nine");
  ]

let _demo_association_list =
  let _ = List.Assoc.find ~equal:Int.equal digit_alist 3 in
  let _ = List.Assoc.find ~equal:Int.equal digit_alist 22 in
  let _ = List.Assoc.add ~equal:Int.equal digit_alist 0 "zilch" in
  digit_alist

(* A module for keeping frequency count on a set of string   *)
module Counter : sig
  type t
  (** A collection of string frequency counts *)

  val empty : t
  (** The empty set of frequency counts *)

  val touch : t -> string -> t
  (** Bump the frequency count for the given string. *)

  val to_list : t -> (string * int) list
  (** Converts the set of frequency counts to an association list. Every string in the list will
      show up at most once, and the integers will be at least 1. *)
end = struct
  (* Notice how we represent Counter using Map *)
  type t = (string, int, String.comparator_witness) Map.t

  (* There, the first-class module is used *)
  let empty = Map.empty (module String)
  let to_list t = Map.to_alist t

  let touch t s =
    let count = match Map.find t s with None -> 0 | Some x -> x in
    Map.set t ~key:s ~data:(count + 1)
end

(* Create map from association list *)
let digit_map = Map.of_alist_exn (module Int) digit_alist
let x = Map.find digit_map 3
