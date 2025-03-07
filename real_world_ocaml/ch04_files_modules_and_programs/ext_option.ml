(* Imagine you wanted to build an extended version of the Option module, 
where you’ve added some functionality not present in the module as distributed in Base. *)

open Base

(* The full contents of the option module *)
include Option

(* The new function we're going to add *)
let apply f_opt x = match f_opt with None -> None | Some f -> Some (f x)
