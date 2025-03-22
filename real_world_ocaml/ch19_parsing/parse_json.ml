(* Use menhir to parse json *)

type value =
  [ `Assoc of (string * value) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of value list
  | `Null
  | `String of string ]

(* 
`Assoc
  ["title", `String "Cities";
   "cities", `List
     [`Assoc ["name", `String "Chicago"; "zips", `List [`Int 60601]];
      `Assoc ["name", `String "New York"; "zips", `List [`Int 10004]]]]  
  *)
let json_string =
  {|
{
  "title": "Cities",
  "cities": [
    { "name": "Chicago",  "zips": [60601] },
    { "name": "New York", "zips": [10004] }
  ]
}
|}
