open Core

type service_info = {
  service_name : string;
  port : int;
  protocol : string;
  comment : string option;
}

let service_info_of_string line =
  let matches =
    (* Notice the usage of regex *)
    let pat = "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)" in
    Re.exec (Re.Posix.compile_pat pat) line
  in
  {
    service_name = Re.Group.get matches 1;
    port = Int.of_string (Re.Group.get matches 2);
    protocol = Re.Group.get matches 3;
    comment = None;
  }

(* A polymorphic record *)
type 'a with_line_num = { item : 'a; line_num : int }

let parse_lines parse file_contents =
  let lines = String.split ~on:'\n' file_contents in
  List.mapi lines ~f:(fun line_num line -> { item = parse line; line_num = line_num + 1 })

let x =
  parse_lines service_info_of_string
    "rtmp              1/ddp     # Routing Table Maintenance Protocol\n\
    \     tcpmux            1/udp     # TCP Port Service Multiplexer\n\
    \     tcpmux            1/tcp     # TCP Port Service Multiplexer"

(* The polymorphism lets us use the same function when parsing a different format, 
like this function for parsing a file containing an integer on every line. *)
let y = parse_lines Int.of_string "1\n10\n100\n1000"

let _ =
  (* demo the field punning *)
  let service_info_to_string { service_name = name; port; protocol = prot; _ } =
    sprintf "%s %i/%s" name port prot
  in
  printf "FTP service: %s\n"
    (service_info_to_string
       {
         service_name = "ftp";
         port = 21;
         protocol = "tcp";
         comment = Some "File Transfer Protocol";
       })

let _ =
  let service_info_of_string line =
    (* first, split off any comment *)
    let line, comment =
      match String.rsplit2 line ~on:'#' with
      | None -> (line, None)
      | Some (ordinary, comment) -> (ordinary, Some comment)
    in
    (* now, use a regular expression to break up the
     service definition *)
    let matches = Re.exec (Re.Posix.compile_pat "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)") line in
    let service_name = Re.Group.get matches 1 in
    let port = Int.of_string (Re.Group.get matches 2) in
    let protocol = Re.Group.get matches 3 in
    (* Field punning can also be used to construct a record *)
    { service_name; port; protocol; comment }
  in
  service_info_of_string "ftp 21/tcp # File Transfer Protocol"
