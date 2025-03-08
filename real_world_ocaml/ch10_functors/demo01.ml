open Core

(* Let’s consider a more realistic example of how to use functors: a library for computing with intervals. *)
module type Comparable = sig
  type t

  val compare : t -> t -> int
end

(* The functor for creating the interval module follows *)
module Make_interval (Endpoint : Comparable) = struct
  type t = Interval of Endpoint.t * Endpoint.t | Empty

  (** [create low high] creates a new interval from [low] to [high]. If [low > high], then the
      interval is empty *)
  let create low high = if Endpoint.compare low high > 0 then Empty else Interval (low, high)

  (** Returns true iff the interval is empty *)
  let is_empty = function Empty -> true | _ -> false

  (** [contains t x] returns true iff [x] is contained in the interval [t] *)
  let contains t x =
    match t with
    | Empty -> false
    | Interval (low, high) -> Endpoint.compare x low >= 0 && Endpoint.compare x high <= 0

  (** [intersect t1 t2] returns the intersection of the two input intervals *)
  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match (t1, t2) with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) -> create (max l1 l2) (min h1 h2)
end

(* option 01: We can instantiate the functor by applying it to a module with the right signature. *)
module Int_interval_v1 = Make_interval (struct
  (*rather than name the module first and then call the functor, we provide the functor input as an anonymous module:  *)
  type t = int

  let compare = Int.compare
end)

(* option 02: If the input interface for your functor is aligned with the standards of the libraries you use, then you don’t need to construct a custom module to feed to the functor.*)
module Int_interval = Make_interval (Int)
module String_interval = Make_interval (String)

let _demo_int_interval_module =
  let i1 = Int_interval.create 3 8 in
  let i2 = Int_interval.create 4 10 in
  Int_interval.intersect i1 i2

(* The ability of functors to mint new types is a useful trick that comes up a lot. *)
module Rev_int_interval = Make_interval (struct
  type t = int

  let compare x y = Int.compare y x
end)
(* Rev_int_interval.t is a different type than Int_interval.t, even though its physical representation is the same. *)
