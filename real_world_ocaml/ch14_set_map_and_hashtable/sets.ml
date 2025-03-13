open Base

let x = Set.of_list (module Int) [ 1; 2; 3 ] |> Set.to_list

let x =
  Set.union (Set.of_list (module Int) [ 1; 2; 3; 2 ]) (Set.of_list (module Int) [ 3; 5; 1 ])
  |> Set.to_list
