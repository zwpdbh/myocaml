(* dome03 compare with demo02 solves the problem of serilalization *)
(* We need change from demo02 such that 
modify Make_interval to use the Sexpable.S interface, for both its input and its output *)
open Core

module type Comparable = sig
  type t

  val compare : t -> t -> int
end

module type Interval_intf = sig
  type t
  type endpoint

  val create : endpoint -> endpoint -> t
  val is_empty : t -> bool
  val contains : t -> endpoint -> bool
  val intersect : t -> t -> t
end

(* First, let’s create an extended version of the Interval_intf interface that includes the functions from the Sexpable.S interface.*)
module type Interval_intf_with_sexp = sig
  (* we can define a type t within our new module, and apply destructive substitutions to all of the included interfaces, Interval_intf included 
  This is somewhat cleaner when combining multiple interfaces, since it correctly reflects that all of the signatures are being handled equivalently
  *)
  type t

  include Interval_intf with type t := t
  include Sexpable.S with type t := t
end

(* Second, create the functor*)
module Make_interval (Endpoint : sig
  type t

  include Comparable with type t := t
  include Sexpable.S with type t := t
end) : Interval_intf_with_sexp with type endpoint := Endpoint.t = struct
  type t = Interval of Endpoint.t * Endpoint.t | Empty [@@deriving sexp]

  (** [create low high] creates a new interval from [low] to [high]. If [low > high], then the
      interval is empty *)
  let create low high = if Endpoint.compare low high > 0 then Empty else Interval (low, high)

  (** Returns true iff the interval is empty *)
  let is_empty = function Empty -> true | Interval _ -> false

  (** [contains t x] returns true iff [x] is contained in the interval [t] *)
  let contains t x =
    match t with
    | Empty -> false
    | Interval (l, h) -> Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  (** [intersect t1 t2] returns the intersection of the two input intervals *)
  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match (t1, t2) with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) -> create (max l1 l2) (min h1 h2)
end

module type Interval_intf_with_sexp = sig
  type t

  include Interval_intf with type t := t
  include Sexpable.S with type t := t
end

(* Finally, we can use that sexp converter in the ordinary way: *)
module Int_interval = Make_interval (Int)

let _demo_use_sexp_converter = Int_interval.sexp_of_t (Int_interval.create 3 4)
