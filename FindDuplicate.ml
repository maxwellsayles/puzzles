(* An implementation of the tortoise and the hare used to find a duplicate in
   an array of N elements of values [0..N-2] guaranteed to have a duplicate. *)

(* Construct an array of `len` random elements from 0 to n-2. *)
let make_list len =
  Array.map (fun _ -> Random.int (len - 1)) (Array.make len 0)

let detect_duplicate arr =
  let n = Array.length arr in
  let next i = Array.get arr i in

  (* Step a tortoise and a hare until they meet *)
  let rec meet i j =
    let i2 = next i in
    let j2 = next (next j) in
    if i2 == j2 then i2 else meet i2 j2
  in

  (* Step both at the same speed until they meet.
     Return the value they refer to *)
  let rec duplicate i j =
    let i2 = next i in
    let j2 = next j in
    if i2 == j2 then Array.get arr i else duplicate i2 j2
  in

  let start = n - 1 in
  duplicate start (meet start start)

let _ =
  Random.self_init();

  let arr = Array.of_list [6; 8; 0; 1; 0; 2; 4; 3; 7; 5;] in
  let res = detect_duplicate arr in
  assert (res == 0);

  let n = 10 in
  let arr = make_list n in

  Printf.printf "Input array: %s\n"
    (String.concat " " (List.map string_of_int (Array.to_list arr)));

  Printf.printf "Duplicate value: %d\n" (detect_duplicate arr);
  0
