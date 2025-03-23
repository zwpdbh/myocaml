open Core

(* Module Angle will provide the type of angles *)
module type Angle = sig
  (* An abstract type to represent angle *)
  type t

  (* convert an angle expressed in degress to a value of type t *)
  val of_degrees : float -> t

  (* Add to angles *)
  val add : t -> t -> t

  (* calculate the cos of angle *)
  val cos : t -> float

  (* calculate the sine of angle  *)
  val sin : t -> float
end

(* As a functor, parameterized by the representation of angles *)
module Turtle (A : Angle) = struct
  let draw = ref true
  let pen_down () = draw := true
  let pen_up () = draw := false

  (* current direction of the turtle *)
  let angle = ref (A.of_degrees 0.)

  (* make the turtle turn d degrees to the left  *)
  let rotate_left d = angle := A.add !angle (A.of_degrees d)
  let rotate_right d = rotate_left (-.d)

  open Graphics

  let tx = ref 400.
  let ty = ref 300.

  let () =
    open_graph " 800x600";
    set_line_width 2;
    moveto (Float.iround_towards_zero_exn !tx) (Float.iround_towards_zero_exn !ty)

  let advance d =
    (tx := Float.(!tx + (d * A.cos !angle)));
    (ty := Float.(!ty + (d * A.sin !angle)));
    if !draw then lineto (Float.iround_towards_zero_exn !tx) (Float.iround_towards_zero_exn !ty)
    else moveto (Float.iround_towards_zero_exn !tx) (Float.iround_towards_zero_exn !ty)
end

(* create reate an implementation of this module *)
module RadianAngle : Angle = struct
  type t = float

  let of_degrees deg = deg *. Float.pi /. 180.0
  let add a1 a2 = a1 +. a2
  let cos = Float.cos
  let sin = Float.sin
end

module T = Turtle (RadianAngle)

let run () =
  let square d =
    for k = 1 to 4 do
      T.advance d;
      T.rotate_left 90.
    done
  in
  let squares d a =
    for k = 1 to Float.iround_towards_zero_exn (360. /. a) do
      square d;
      T.rotate_left a
    done
  in
  squares 100. 20.
