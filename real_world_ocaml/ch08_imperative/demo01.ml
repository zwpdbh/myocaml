(* bytes AND strinS *)
open Base

let x =
  let b = Bytes.of_string "foobar" in
  Bytes.set b 0 (Char.uppercase (Bytes.get b 0));
  b

(* For *)
open Stdio

let x =
  for i = 0 to 3 do
    printf "i = %d\n" i
  done

let x =
  for i = 3 downto 0 do
    printf "i = %d\n" i
  done

let x =
  let nums = [| 1; 2; 3; 4; 5 |] in
  (* reversing an array in place *)
  let rec_inplace ar =
    let i = ref 0 in
    let j = ref (Array.length ar - 1) in
    while !i < !j do
      let tmp = ar.(!i) in
      ar.(!i) <- ar.(!j);
      ar.(!j) <- tmp;
      Int.incr i;
      Int.decr j
    done
  in
  rec_inplace nums;
  nums
