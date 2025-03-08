(* Abstracting Computational Machines *)
(* A common idiom in OCaml is to combine small components into larger computational machines, using a collection of component-combining functions, or combinators.  

GADTs can be helpful for writing such combinators. *)

(* beyond basic execution of the pipeline, e.g.:

Profiling, so that when you run a pipeline, you get a report of how long each step of the pipeline took.
Control over execution, like allowing users to pause the pipeline mid-execution, and restart it later.
Custom error handling, so, for example, you could build a pipeline that kept track of where it failed, and offered the possibility of restarting it. *)
open Core

type (_, _) pipeline =
  | Step : ('a -> 'b) * ('b, 'c) pipeline -> ('a, 'c) pipeline
  | Empty : ('a, 'a) pipeline

let ( @> ) f pipeline = Step (f, pipeline)
let empty = Empty

(* do a no-frills pipeline execution *)
let rec exec : type a b. (a, b) pipeline -> a -> b =
 fun pipeline input -> match pipeline with Empty -> input | Step (f, tail) -> exec tail (f input)

(* executes a pipeline and produces a profile showing how long each step of a pipeline took *)
let exec_with_profile pipeline input =
  let rec loop : type a b. (a, b) pipeline -> a -> Time_ns.Span.t list -> b * Time_ns.Span.t list =
   fun pipeline input rev_profile ->
    match pipeline with
    | Empty -> (input, rev_profile)
    | Step (f, tail) ->
        let start = Time_ns.now () in
        let output = f input in
        let elapsed = Time_ns.diff (Time_ns.now ()) start in
        loop tail output (elapsed :: rev_profile)
  in
  let output, rev_profile = loop pipeline input [] in
  (output, List.rev rev_profile)

(* Define some simple functions to use in our pipelines *)
let add_one x = x + 1
let double x = x * 2
let to_string x = Int.to_string x
let to_uppercase s = String.uppercase s

(* Example 1: Simple integer pipeline *)
let int_pipeline = add_one @> double @> add_one @> empty

(* Example 2: Pipeline that converts int to uppercase string *)
let string_pipeline = add_one @> double @> to_string @> to_uppercase @> empty

(* Example 3: Empty pipeline *)
let empty_pipeline = empty

let () =
  (* Using exec *)
  let result1 = exec int_pipeline 5 in
  printf "Int pipeline result: %d\n" result1;

  let result2 = exec string_pipeline 10 in
  printf "String pipeline result: %s\n" result2;

  let result3 = exec empty_pipeline "Hello" in
  printf "Empty pipeline result: %s\n" result3

let () =
  (* Using exec_with_profile *)
  let result4, profile4 = exec_with_profile int_pipeline 5 in
  printf "Int pipeline result with profile: %d\n" result4;
  List.iteri profile4 ~f:(fun i span ->
      printf "Step %d took: %s\n" (i + 1) (Time_ns.Span.to_string span));

  let result5, profile5 = exec_with_profile string_pipeline 10 in
  printf "String pipeline result with profile: %s\n" result5;
  List.iteri profile5 ~f:(fun i span ->
      printf "Step %d took: %s\n" (i + 1) (Time_ns.Span.to_string span));

  let result6, profile6 = exec_with_profile empty_pipeline "Hello" in
  printf "Empty pipeline result with profile: %s\n" result6;
  List.iteri profile6 ~f:(fun i span ->
      printf "Step %d took: %s\n" (i + 1) (Time_ns.Span.to_string span))
