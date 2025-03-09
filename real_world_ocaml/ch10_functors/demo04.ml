(* Extending Modules: Another common use of functors is to generate type-specific functionality for a given module in a standardized way. *)

module Fqueue : sig
  type 'a t

  val empty : 'a t

  val enqueue : 'a t -> 'a -> 'a t
  (** [enqueue q el] adds [el] to the back of [q] *)

  val dequeue : 'a t -> ('a * 'a t) option
  (** [dequeue q] returns None if the [q] is empty, otherwise returns the first element of the queue
      and the remainder of the queue *)

  val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
  (** Folds over the queue, from front to back *)
end = struct
  open Base

  type 'a t = 'a list * 'a list

  let empty = ([], [])
  let enqueue (in_list, out_list) x = (x :: in_list, out_list)

  let dequeue (in_list, out_list) =
    match out_list with
    | hd :: tl -> Some (hd, (in_list, tl))
    | [] -> ( match List.rev in_list with [] -> None | hd :: tl -> Some (hd, ([], tl)))

  let fold (in_list, out_list) ~init ~f =
    let after_out = List.fold ~init ~f out_list in
    List.fold_right ~init:after_out ~f:(fun x acc -> f acc x) in_list
end

(* Now, create a new module, Foldable, that automates the process of adding helper functions to a fold-supporting container*)

module type S = sig
  type 'a t

  val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
end

module type Extension = sig
  type 'a t

  val iter : 'a t -> f:('a -> unit) -> unit
  val length : 'a t -> int
  val count : 'a t -> f:('a -> bool) -> int
  val for_all : 'a t -> f:('a -> bool) -> bool
  val exists : 'a t -> f:('a -> bool) -> bool
end

(* A functor Extend that allows one to extend any module that matches Foldable.S 
The extended result will have module signature as Extension*)
module Extend (Arg : S) : Extension with type 'a t := 'a Arg.t = struct
  open Arg

  let iter t ~f = fold t ~init:() ~f:(fun () a -> f a)
  let length t = fold t ~init:0 ~f:(fun acc _ -> acc + 1)
  let count t ~f = fold t ~init:0 ~f:(fun count x -> count + if f x then 1 else 0)

  exception Short_circuit

  let for_all c ~f =
    try
      iter c ~f:(fun x -> if not (f x) then raise Short_circuit);
      true
    with Short_circuit -> false

  let exists c ~f =
    try
      iter c ~f:(fun x -> if f x then raise Short_circuit);
      false
    with Short_circuit -> true
end

(* Apply the Extend functor to Fqueue *)
module Extended_Fqueue = Extend (Fqueue)

(* Now you can use the extended functionality *)
let test_extended_fqueue () =
  let q = Fqueue.empty in
  let q = Fqueue.enqueue q 1 in
  let q = Fqueue.enqueue q 2 in
  let q = Fqueue.enqueue q 3 in

  Printf.printf "Length: %d\n" (Extended_Fqueue.length q);
  Printf.printf "Count of even numbers: %d\n" (Extended_Fqueue.count q ~f:(fun x -> x mod 2 = 0));
  Printf.printf "All greater than 0: %b\n" (Extended_Fqueue.for_all q ~f:(fun x -> x > 0));
  Printf.printf "Exists number greater than 2: %b\n" (Extended_Fqueue.exists q ~f:(fun x -> x > 2));

  Extended_Fqueue.iter q ~f:(fun x -> Printf.printf "%d " x);
  Printf.printf "\n"

(* Run the test *)
let () = test_extended_fqueue ()
