open Base

module type Query_handler = sig
  type config
  (** Configuration for a query handler *)

  val sexp_of_config : config -> Sexp.t
  val config_of_sexp : Sexp.t -> config

  val name : string
  (** The name of the query-handling service *)

  type t
  (** The state of the query handler *)

  val create : config -> t
  (** Creates a new query handler from a config *)

  val eval : t -> Sexp.t -> Sexp.t Or_error.t
  (** Evaluate a given query, where both input and output are s-expressions *)
end

(* Now we can construct an example of a query handler that satisfies the Query_handler interface *)
