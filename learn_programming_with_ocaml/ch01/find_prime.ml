open Core

(* demo 
arrays 
while loop
begin-end block
*)
let find_prime max =
  let prime = Array.create ~len:(max + 1) true in
  prime.(0) <- false;
  prime.(1) <- false;
  let limit = Float.iround_towards_zero_exn (sqrt (float max)) in
  for n = 2 to limit do
    if prime.(n) then
      let m = ref (n * n) in
      while !m < max do
        prime.(!m) <- false;
        m := !m + n
      done
  done;
  for n = 2 to max do
    if prime.(n) then printf "%d\n" n
  done

open Core
open Async

let command =
  (* 1. create Command.Param *)
  let params =
    let%map_open.Command n = flag "-n" (required int) ~doc:"description" in
    fun () -> find_prime n
  in
  (* 2. create readme *)
  let readme () = "compute primes from 1 to n" in
  Command.basic ~summary:"compute primes from 1 to n" ~readme params
