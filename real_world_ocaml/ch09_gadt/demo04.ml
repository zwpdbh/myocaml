(* Abstracting Computational Machines *)
(* A common idiom in OCaml is to combine small components into larger computational machines, using a collection of component-combining functions, or combinators.  

GADTs can be helpful for writing such combinators. *)

(* beyond basic execution of the pipeline, e.g.:

Profiling, so that when you run a pipeline, you get a report of how long each step of the pipeline took.
Control over execution, like allowing users to pause the pipeline mid-execution, and restart it later.
Custom error handling, so, for example, you could build a pipeline that kept track of where it failed, and offered the possibility of restarting it. *)

module type Pipeline = sig
  type ('input, 'output) t

  val ( @> ) : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  val empty : ('a, 'a) t
end
