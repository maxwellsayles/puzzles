(* An implementation of Brent's cycle finding -- the teleporting tortoise. *)

type node = { mutable next: node option }

(* Construct a list of `len` elements. *)
let make_list len =
  let rec loop len acc =
    match len with
    | 0 -> acc
    | _ -> loop (len - 1) { next = Some acc } in
  loop len { next = None }

(* Given a node, return the next node. *)
let next_node { next = next } =
  match next with
  | None -> failwith "No next"
  | Some n -> n

(* Drop the first `len` elements from the list. *)
let rec drop len node =
  match len with
  | 0 -> node
  | _ -> drop (len - 1) (next_node node)

(* Construct a list with the given tail and cycle length. *)
let make_cycle cycle_length tail_length =
  let total_length = cycle_length + tail_length in
  let xs = make_list total_length in
  let last_node = drop (total_length - 1) xs in
  let nth_node = drop tail_length xs in
  last_node.next <- Some nth_node;
  xs

(* Applies f to x n times, i.e. f(f(f(...(f(x))))). *)
let rec iterate f x n =
  match n with
  | 0 -> x
  | _ -> iterate f (f x) (n - 1)

(* Detects the length of a cycle given a start element x and a function f used to
   advance to the next element.  The algorithm works by planting a tortoise and
   advancing a hare.  After `power` number of steps, if the hare hasn't caught the
   tortoise, the tortoise is teleported to catch the hare and `power` is doubled.
   This way, eventually the hare takes a number of steps more than the cycle length
   and the sum of all the steps taken in 2n-1. *)
let detect_cycle_length f x =
  let rec loop hare tortoise steps power =
    if tortoise == hare && steps > 0 then steps
    else if steps = power then loop hare hare 0 (power * 2)
    else loop (f hare) tortoise (steps + 1) power in
  loop x x 0 1

(* Given the cycle length, this will detect the tail length.  It works by
   advancing a cursor `u` steps.  Assume the tail has length `t`.  Since the cursor
   had to crawl the tail, that means the cursor is now `t` steps from completing the
   first cycle and returning to the end of the tail.  Now, we advance two cursors
   (the original cursor and now another cursor from the first element) until these
   two cursors meet. The number of steps taken in the second phase is the length of
   the tail. *)
let detect_tail_length f x u =
  let y = iterate f x u in
  let rec together x y steps =
    if x == y then steps else together (f x) (f y) (steps + 1) in
  together x y 0

(* Construct a linked list with a cycle of random length and then run the
   teleporting tortoise detection algorithm and verify the results. *)
let run_test() =
  let cycle_length = 1 + Random.int 1000 in
  let tail_length = Random.int 1000 in

  Printf.printf "Cycle length %d\n" cycle_length;
  Printf.printf "Tail length %d\n" tail_length;

  let xs = make_cycle cycle_length tail_length in
  
  let c = detect_cycle_length next_node xs in
  Printf.printf "Detected a cycle length of %d\n" c;

  let t = detect_tail_length next_node xs c in
  Printf.printf "Detected a tail length of %d\n" t;

  if c = cycle_length && t = tail_length
  then Printf.printf "Everything is okay!\n\n"
  else failwith "Something went horrible wrong!"

let _ =
  Random.self_init();
  iterate run_test () 10000;
  0


