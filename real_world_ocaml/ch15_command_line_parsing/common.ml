open Core

(* The do_hash function accepts a filename parameter and prints the human-readable MD5 string to the console standard output. *)
let do_hash_v1 file = Md5.digest_file_blocking file |> Md5.to_hex |> print_endline

let do_hash_v2 hash_length filename =
  Md5.digest_file_blocking filename |> Md5.to_hex
  |> (fun s -> String.prefix s hash_length)
  |> print_endline
