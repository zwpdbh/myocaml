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

(* Define a new type alias for some map, 
This creates a map where the keys are strings and the values are integers. *)
type string_int_map = int Map.M(String).t [@@deriving sexp]

let _demo_create_type_alias_for_map =
  let my_map : string_int_map = Map.empty (module String) in
  let updated_map = Map.set my_map ~key:"hello" ~data:42 in
  Map.iter updated_map ~f:(fun ~key ~data -> Stdio.printf "%s: %d\n" key data)

let _demo_map01 =
  (* Create an empty map with int keys and string values *)
  let _empty_map = Map.empty (module Int) in

  (* Add some key-value pairs to the map *)
  let map_with_values = Map.of_alist_exn (module Int) [ (1, "One"); (2, "Two"); (3, "Three") ] in

  (* Add a single key-value pair to an existing map *)
  let updated_map = Map.set map_with_values ~key:4 ~data:"Four" in

  (* Retrieve a value from the map *)
  let value = Map.find updated_map 2 in

  (* Print the value *)
  match value with
  | Some v -> Stdio.printf "Value for key 2: %s\n" v
  | None -> Stdio.printf "Key 2 not found in the map\n"

let _demo_map02 =
  (* Create a map with an initial key-value pair *)
  let initial_map = Map.singleton (module Int) 1 "one" in

  (* Print the value for key 1 *)
  match Map.find initial_map 1 with
  | Some value -> Stdio.printf "Value for key 1: %s\n" value
  | None ->
      Stdio.printf "Key 1 not found in the map\n";
      (* Add more key-value pairs *)
      let updated_map = initial_map |> Map.set ~key:2 ~data:"two" |> Map.set ~key:3 ~data:"three" in
      (* Print the size of the updated map *)
      Stdio.printf "Size of updated map: %d\n" (Map.length updated_map)
