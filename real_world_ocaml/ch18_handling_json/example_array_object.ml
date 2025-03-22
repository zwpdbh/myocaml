open Core

(* Define an item type *)
type item = { name : string; price : float } [@@deriving yojson]

(* Define a store type which contains a list of items *)
type store = { items : item list } [@@deriving yojson]

(* Define the top-level type *)
type root = { store : store } [@@deriving yojson]

let json_string =
  {|
{
  "store": {
    "items": [
      { "name": "item1", "price": 10.5 },
      { "name": "item2", "price": 20.0 }
    ]
  }
}
|}

let yojson = Yojson.Safe.from_string json_string

let root_value =
  match root_of_yojson yojson with
  | Ok value -> value
  | Error err -> failwith ("JSON parse error: " ^ err)

let () =
  List.iter
    ~f:(fun item -> Format.printf "Item: %s, Price: %.2f\n" item.name item.price)
    root_value.store.items
