open Utils
open Base
open Stdio
open Printf

let run_command cmd =
  match Cmd.run_cmd cmd with
  | Ok output ->
      print_endline "Command executed successfully:";
      print_endline output
  | Error e ->
      let error_msg = Cmd.error_to_string e in
      print_endline ("Error: " ^ error_msg)

(* let create_aks_cluster ~resource_group ~name ~nodepool_name ?(node_count = 3)
    ?(enable_addons = [ "monitoring" ]) ?(generate_ssh_keys = true) ?(node_vm_size = "Standard_NC6")
    ?(kubernetes_version = "1.23.7") () =
  let base_cmd = sprintf "az aks create --resource-group %s --name %s" resource_group name in
  let options =
    [
      sprintf "--nodepool-name %s" nodepool_name;
      sprintf "--node-count %d" node_count;
      sprintf "--node-vm-size %s" node_vm_size;
      sprintf "--kubernetes-version %s" kubernetes_version;
      (if List.length enable_addons > 0 then
         sprintf "--enable-addons %s" (String.concat ~sep:"," enable_addons)
       else "");
      (if generate_ssh_keys then "--generate-ssh-keys" else "");
    ]
    |> List.filter ~f:(fun s -> s <> "")
  in

  let full_cmd = String.concat ~sep:" \\\n  " (base_cmd :: options) in
  run_command full_cmd

let () =
  create_aks_cluster ~resource_group:"myResourceGroup" ~name:"myAKSCluster"
    ~nodepool_name:"myNodePool" () (* Don't forget the unit argument at the end *)
  |> ignore;

  (* Or with custom options: *)
  create_aks_cluster ~resource_group:"myCustomResourceGroup" ~name:"myCustomAKSCluster"
    ~nodepool_name:"myCustomNodePool" ~node_count:5
    ~enable_addons:[ "monitoring"; "http_application_routing" ]
    ~node_vm_size:"Standard_NC12" ~kubernetes_version:"1.24.0" ()
  |> ignore *)
