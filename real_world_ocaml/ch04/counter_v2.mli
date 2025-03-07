open Base
(* val declarations are used to specify values in a signature. *)

(* start by writing down an interface that describes what’s currently available *)
(* val touch : (string * int) list -> string -> (string * int) list *)
(** Bump the frequency count for the given string. *)

(* To hide the fact that frequency counts are represented as association lists, we’ll need to make the type of frequency counts abstract *)
(* A type is abstract if its name is exposed in the interface, but its definition is not. *)

type t
(** A collection of string frequency counts *)

val empty : t
(** The empty set of frequency counts *)

val touch : t -> string -> t
(** Bump the frequency count for the given string. *)

val to_list : t -> (string * int) list
(** Converts the set of frequency counts to an association list. A string shows up at most once, and
    the counts are >= 1. *)
