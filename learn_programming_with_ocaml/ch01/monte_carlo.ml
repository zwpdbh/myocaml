open Core

let approx n =
  let p = ref 0 in
  (* For an expression
 sequence (indicated by `;`) to be accepted by OCaml, the expression before the semicolon must
 not return a value, that is, it must only perform side effects. *)
  for k = 1 to n do
    let x = Random.float 1.0 in
    let y = Random.float 1.0 in
    if Float.((x * x) + (y * y) <= 1.0) then p := !p + 1
  done;
  let pi = 4.0 *. float !p /. float n in
  Printf.printf "%f\n" pi

(* dune exec learn_programming_with_ocaml/ch01/ch01.exe demo02 -- -n 1000 *)
let command =
  (* 1. create Command.Param *)
  let params =
    let%map_open.Command n = flag "-n" (required int) ~doc:"description" in
    fun () -> approx n
  in
  (* 2. create readme *)
  let readme () = "Use mote carlo to approx pi, use n to specify approx times " in
  Command.basic ~summary:"Use mote carlo to approx pi" ~readme params
