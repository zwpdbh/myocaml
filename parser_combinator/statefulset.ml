open Angstrom

type statefulset = {
  name : string;
  ready : string;
  status : string;
  restarts : string;
  age : string;
  ip : string;
  node : string;
  nominated_node : string;
  readiness_gates : string;
}

let header_row =
  {|
NAME                                                            READY   STATUS                   RESTARTS           AGE     IP               NODE                                NOMINATED NODE   READINESS GATES
|}

let input =
  {|
NAME                                                            READY   STATUS                   RESTARTS           AGE     IP               NODE                                NOMINATED NODE   READINESS GATES
azurecontainerstorage-agent-core-7584594559-mb65s               3/3     Running                  0                  7d1h    10.244.2.157     aks-nodepool1-30816469-vmss000001   <none>           <none>
azurecontainerstorage-agent-core-7584594559-mfdfr               3/3     Running                  0                  7d1h    10.244.68.12     aks-pool01-97838753-vmss00001p      <none>           <none>
azurecontainerstorage-api-rest-5bc7b97bf5-crf9j                 0/1     Running                  0                  7d1h    10.244.3.210     aks-nodepool1-30816469-vmss000002   <none>           <none>
azurecontainerstorage-api-rest-6d68d45d69-6rcqh                 0/1     Running                  1 (7d11h ago)      7d23h   10.244.90.165    aks-pool01-97838753-vmss00002b      <none>           <none> 
|}

let whitespace = take_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)
let word = take_while1 (function ' ' | '\t' | '\n' | '\r' -> false | _ -> true)

let column_names =
  let* _ = whitespace in
  sep_by1 whitespace word

let demo_01 =
  match parse_string ~consume:All column_names header_row with
  | Ok names ->
      ( List.nth names 0,
        List.nth names 1,
        List.nth names 2,
        List.nth names 3,
        List.nth names 4,
        List.nth names 5,
        List.nth names 6,
        List.nth names 7,
        List.nth names 8 )
  | Error msg -> failwith ("Failed to parse header row: " ^ msg)
