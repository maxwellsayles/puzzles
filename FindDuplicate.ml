(* An implementation of Brent's cycle finding -- the teleporting tortoise.

In particular, this is used to find a duplicate element in an array of N
elements of values [0..N-1].
*)

(* Construct an array of `len` random elements from 0 to n-1. *)
let make_list len =
  let arr = Array.make len 0 in
  let rec loop i =
    if i = len then arr
    else begin
      Array.set arr i (Random.int len);
      loop (i + 1)
    end
  in
  loop 0

(* Detects the duplicate value. The algorithm works by planting a tortoise and
   advancing a hare. After `power` number of steps, if the hare hasn't caught the
   tortoise, the tortoise is teleported to catch the hare and `power` is doubled.
   This way, eventually the hare takes a number of steps more than the cycle length
   and the sum of all the steps taken in 2n-1. *)
let detect_duplicate arr =
  let n = Array.length arr in
  let next idx = (idx + 1) mod n in

  let rec loop hare tortoise steps power =
    let x = Array.get arr tortoise in
    let y = Array.get arr hare in
    if x = y && tortoise <> hare then x
    else if steps = power then loop hare hare 0 (power * 2)
    else loop (next hare) tortoise (steps + 1) power in

  loop 0 0 0 1

let _ =
  Random.self_init();

  let n = 100 in
  let arr = make_list n in

  Printf.printf "Input array: %s\n"
    (String.concat " " (List.map string_of_int (Array.to_list arr)));

  Printf.printf "Duplicate value: %d\n" (detect_duplicate arr);
  0
