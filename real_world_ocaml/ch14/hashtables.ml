open Core
open Base
open Core_bench

let x =
  let table = Hashtbl.create (module String) in
  Hashtbl.set table ~key:"foo" ~data:1;
  Hashtbl.find table "foo"

(* TBC: How to make  *)
module Book = struct
  type t = { title : string; isbn : string } [@@deriving compare, sexp_of, hash]
end

let table = Hashtbl.create (module Book)

(* A bench mark for comparing hashtbl and map *)
let map_iter ~num_keys ~iterations =
  let rec loop i map =
    if i <= 0 then ()
    else
      loop (i - 1)
        (Map.change map (i % num_keys) ~f:(fun current ->
             Some (1 + Option.value ~default:0 current)))
  in
  loop iterations (Map.empty (module Int))

let table_iter ~num_keys ~iterations =
  let table = Hashtbl.create (module Int) ~size:num_keys in
  let rec loop i =
    if i <= 0 then ()
    else (
      Hashtbl.change table (i % num_keys) ~f:(fun current ->
          Some (1 + Option.value ~default:0 current));
      loop (i - 1))
  in
  loop iterations

let tests ~num_keys ~iterations =
  let t name f = Bench.Test.create f ~name in
  [
    t "table" (fun () -> table_iter ~num_keys ~iterations);
    t "map" (fun () -> map_iter ~num_keys ~iterations);
  ]

let () = tests ~num_keys:1000 ~iterations:100_000 |> Bench.make_command |> Command_unix.run
