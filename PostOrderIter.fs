(**
 * Given a binary tree, provide a post order iterator.
 *
 * Here I provide three functional solutions.  The first uses a sequence to lazily
 * yield the values in post order, the second uses a left fold in post order, and
 * the third uses continuations.
 *)

type Tree<'T> = Empty | Node of Tree<'T> * 'T * Tree<'T>

let empty = Empty
let singleton v = Node(empty, v, empty)
let tree l v r = Node(l, v, r)

let rec seqPostOrder t =
    match t with
    | Empty -> Seq.empty
    | Node(l, v, r) -> seq { yield! seqPostOrder l; yield! seqPostOrder r; yield v; }

let foldPostOrder f z0 t =
    let rec helper t acc =
        match t with
        | Empty -> acc
        | Node(l, v, r) -> acc |> helper l |> helper r |> (fun acc -> f acc v)
    helper t z0

let contPostOrder f t =
    let rec helper t k =
        match t with
        | Empty -> k()
        | Node(l, v, r) ->
            helper l (fun () ->
                helper r (fun () ->
                    f(v)
                    k()
                )
            )
    helper t id

[<EntryPoint>]
let main argv =
    let t = tree (tree (singleton 4) 2 (singleton 5)) 1 (tree (singleton 6) 3 (singleton 7))
    
    seqPostOrder t |> Seq.iter (printf "%d")
    printfn ""

    foldPostOrder (fun acc x -> x :: acc) [] t |> List.rev |> Seq.iter (printf "%d")
    printfn ""

    contPostOrder (printf "%d") t
    printfn ""

    0
