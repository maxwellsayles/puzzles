(**
 * Given a singly linked list where each node has a second pointer to the list,
 * duplicate the list. Note, the second pointer may point to a node earlier in
 * the list.
 *
 * This temporarily mutates the input (but then restores it) and uses 3 passes
 * to use a constant amount of memory plus the new list, and time linear to
 * the input.
 *
 * The first pass interleaves a copy of each node.
 * The second pass moves the second node pointer forward 1 node.
 * The third pass de-interleaves each list, and restores the input.
 *
 * This may not have been the best language for the solution, since it deals
 * with mutation so much, but
 *)
open System.Collections.Generic

[<ReferenceEquality>]
type 'a node = { mutable x: 'a node option;
                 mutable y: 'a node option;
                 mutable v: 'a }

let ToList(hd: 'a node option): 'a node list =
    let rec f acc = function
        | None -> List.rev acc
        | Some n -> f (n :: acc) n.x
    f [] hd

// Given a list of indices from the head, create a list of nodes with a second
// link to the node at the index from the head.
let FromList(hd: int option list): int node option =
    match hd with
    | [] -> None
    | _ ->
        // Generate a list of the appropriate number of elements.
        let rec makeList acc next i =
            match i with
            | 0 -> acc
            | _ ->
                let n = Some { x = next; y = None; v = i }
                makeList (n :: acc) n (i - 1)
        let l = makeList [] None (List.length hd)

        // Mutate the y pointer to point to the correct node.
        let mutateY(t, n) =
            match t, n with
            | None, _ -> ()
            | Some i, Some n' -> n'.y <- List.nth l i
            | _, _ -> failwith "WTF"
        List.iter mutateY <| List.zip hd l
        List.head l

// Are two 2-tailed lists isomorphic?
let Same (a: 'a node option) (b: 'a node option) : bool =
    let a' = ToList a
    let b' = ToList b

    // NOTE: We use a function to defer computation because the lists
    // might not be the same length.
    let iso() =
        // Build a set pairing elements from the two lists
        let m = new HashSet<'a node * 'a node>()
        List.iter (ignore << m.Add) <| List.zip a' b'

        // Check that `y` nodes are isomorphic
        let f(a, b) =
            match a, b with
            | None, None -> true
            | Some u, Some v -> m.Contains(u, v)
            | _, _ -> false
        let ay = List.map (fun n -> n.y) a'
        let by = List.map (fun n -> n.y) b'
        List.forall f <| List.zip ay by

    let equalValue() =
        List.forall (fun (a, b) -> a.v = b.v) <| List.zip a' b'

    List.length a' <> List.length b' && equalValue() && iso()

let DuplicateImp(hd: 'a node option): 'a node option =    
    // Interleave a copy.
    let mutable h = hd
    while h.IsSome do
        let cur = h.Value
        h <- cur.x
        cur.x <- Some { x = cur.x; y = cur.y; v = cur.v }

    // Bump the copy's `y` link forward one.
    let mutable h = hd
    while h.IsSome do
        let cur = h.Value
        let next = cur.x.Value
        if next.y.IsSome then
            next.y <- next.y.Value.x
        h <- next.x

    // Uninterleave the copy and return the head of the new copy.
    let mutable h = hd
    let res = if h.IsSome then h.Value.x else None
    while h.IsSome do
        let cur = h.Value
        let next = cur.x.Value
        h <- next.x
        cur.x <- next.x
        if next.x.IsSome then
            next.x <- next.x.Value.x
    res

[<EntryPoint>]
let main argv =
    let test n =
        let n' = FromList n
        let d = DuplicateImp n'
        assert(d <> n')
        assert(Same d n')

    test []
    test [None]
    test [Some 0]
    test [None; None]
    test [Some 0; None]
    test [None; Some 0]
    test [Some 0; Some 1]
    test [Some 1; Some 0]
    test [Some 0; Some 0]
    test [Some 1; Some 1]
    test [None; None; None]
    test [None; None; Some 0]
    test [None; Some 0; Some 0]
    test [Some 0; Some 0; Some 0]
    test [Some 1; Some 2; Some 0]
    test [Some 0; Some 1; Some 2; Some 3]
    test [Some 1; Some 2; Some 3; Some 0]

    printfn "All Pass"
    0
