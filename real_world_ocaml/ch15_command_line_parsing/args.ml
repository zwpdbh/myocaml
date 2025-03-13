open Core

(* Define an Annonymous Argument *)
let filename_param =
  let open Command.Param in
  anon ("filename" %: string)
