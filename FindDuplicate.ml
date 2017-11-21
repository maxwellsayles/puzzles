(*
In particular, this is used to find a duplicate element in an array of N
elements of values [0..N-2].
*)

(* Construct an array of `len` random elements from 0 to n-2. *)
let make_list len =
  Array.map (fun _ -> Random.int (len - 1)) (Array.make len 0)

let detect_duplicate arr =
  let n = Array.length arr in
  let next i = Array.get arr i in

  (* Step j forward c times *)
  let rec jump j c =
    if c == 0 then j else jump (next j) (c - 1)
  in

  (* Find an index that is definitely on the loop starting from index n - 1 *)
  let start = jump (n - 1) (n - 1) in

  (* Step until we determine the cycle length *)
  let length =
    let rec fnc j acc =
      if j == start then acc else fnc (next j) (acc + 1)
    in fnc (next start) 1
  in
  Printf.printf "length: %d\n" length;

  (* Find the first point before the cycle; this is the duplicate element *)
  let j = jump (n - 1) (n - length) in
  Array.get arr j

let _ =
  Random.self_init();

  let n = 10 in
  let arr = Array.of_list [6; 8; 0; 1; 0; 2; 4; 3; 7; 5;] in

(*  let arr = make_list n in*)


  (* 6 8 0 0 0 2 4 5 7 5 *)

  Printf.printf "Input array: %s\n"
    (String.concat " " (List.map string_of_int (Array.to_list arr)));

  Printf.printf "Duplicate value: %d\n" (detect_duplicate arr);
  0
