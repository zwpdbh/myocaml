(* open Core *)

(* demo pairs, n-tuples, first-class, higher order*)

let read_pair () =
  let x = read_int () in
  let y = read_int () in
  (x, y)

open Graphics

let run n =
  let data = Array.init n (fun i -> read_pair ()) in
  Array.sort compare data;
  open_graph " 200x200";
  set_line_width 3;
  let x0, y0 = data.(0) in
  moveto x0 y0;
  for i = 1 to n - 1 do
    let x, y = data.(i) in
    lineto x y
  done;
  ignore (read_key ())

let command =
  let open Core in
  (* 1. create Command.Param *)
  let params =
    let%map_open.Command n = flag "-n" (required int) ~doc:"description" in
    fun () -> run n
  in
  (* 2. create readme *)
  let readme () = "Command description. Additional details." in
  Command.basic ~summary:"Command summary" ~readme params
