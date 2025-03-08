(* https://dev.realworldocaml.org/gadts.html *)
(* Narrowing the Possibilities *)

(* One context where this can be useful is when managing complex application state, where the available data changes over time. 
Let’s consider a simple example, where we’re writing code to handle a logon request from a user, and we want to check if the user in question is authorized to logon. *)

(* With GADTs, we can track the state of the request in a type parameter, and have that parameter be used to narrow the set of available cases, without duplicating the type. *)

(* 1. start by creating an option type that is sensitive to whether our request is in a complete or incomplete state. *)

type incomplete = Incomplete
type complete = Complete

(* 2. mint a completeness-sensitive option type. *)
(* the first indicates the type of the contents of the option *)
(* the second indicates whether this is being used in an incomplete state *)
type (_, _) coption = Absent : (_, incomplete) coption | Present : 'a -> ('a, _) coption

(* We can use coption to define a completion-sensitive version of logon_request. *)
module User_name = struct
  type t = string
end

module User_id = struct
  type t = int
end

module Permissions = struct
  type t = string

  let check _permissions _user_id = true
end

type 'c logon_request = {
  user_name : User_name.t;
  user_id : (User_id.t, 'c) coption;
  permissions : (Permissions.t, 'c) coption;
}

let set_user_id request x = { request with user_id = Present x }
let set_permissions request x = { request with permissions = Present x }

let check_completeness request : complete logon_request option =
  match (request.user_id, request.permissions) with
  | Absent, _ | _, Absent -> None
  | (Present _ as user_id), (Present _ as permissions) -> Some { request with user_id; permissions }

let authorized (request : complete logon_request) =
  let { user_id = Present user_id; permissions = Present permissions; _ } = request in
  Permissions.check permissions user_id
