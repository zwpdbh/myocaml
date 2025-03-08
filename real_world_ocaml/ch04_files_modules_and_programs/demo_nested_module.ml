open Base

(* module <name> : <signature> = <implementation> *)
(* module Username : sig ... end = struct ... end *)
(* This syntax is used when you're defining a module and its implementation simultaneously.  *)
module Username : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end = struct
  type t = string

  let of_string x = x
  let to_string x = x
  let ( = ) = String.( = )
end
