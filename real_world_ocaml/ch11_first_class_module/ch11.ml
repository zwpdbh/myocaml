open Base

module type X_int = sig
  val x : int
end

(* create module that matches this signature *)
module Three : X_int = struct
  let x = 3
end

(* convert Three into a first-class module as follows *)
let three = (module Three : X_int)

(* Inference: we don't need to specify the module type X_int  *)
module Four = struct
  let x = 4
end

let numbers = [ three; (module Four) ]

(* Create a first-class module from an anonymous module *)
let numbers =
  [
    three;
    (module struct
      let x = 4
    end);
  ]

(* To access the contents of a first-class module, you need to unpack it into an ordinary module *)
module New_three = (val three : X_int)

let _ = New_three.x
