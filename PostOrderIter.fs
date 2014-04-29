(**
 * Given a binary tree, provide a post order iterator.
 *
 * Here I provide four solutions, one imperative as a stateful iterator, and
 * three functional.  The first uses a stateful class to iterate over the sequence.
 * The second, uses a sequence to lazily yield the values in post order, the third
 * uses a left fold in post order, and the last uses continuations.
 *
 * NOTE: This is a solution to an interview question that expects a stateful iterator.
 * In practice, I would prefer the the second solution using a sequence to lazily
 * yield values.  If this feature was not available in the language, then I would
 * prefer continuations, and finally a left fold.
 *)

type Tree<'t> = Empty | Node of Tree<'t> * 't * Tree<'t>

let empty = Empty
let singleton v = Node(empty, v, empty)
let tree l v r = Node(l, v, r)

type Visit = LeftVisit | RightVisit | FinishedVisit

type PostOrderIterator<'T>(n: Tree<'T>) =
    let mutable stack = [(n, LeftVisit)]

    member this.Next() =
        match stack with
        | [] -> None
        | (topNode, topVisit) :: rest ->
            match topNode with
            // If the top of the stack is a leaf, continue with the next element.
            | Empty ->
                stack <- rest
                this.Next()

            // Either traverse the left child, the right child,
            // or return the current
            | Node(l, v, r) ->
                match topVisit with
                | LeftVisit ->
                    stack <- (l, LeftVisit) :: (topNode, RightVisit) :: rest
                    this.Next()
                | RightVisit ->
                    stack <- (r, LeftVisit) :: (topNode, FinishedVisit) :: rest
                    this.Next()
                | FinishedVisit ->
                    stack <- rest
                    Some(v)

let rec iteratePostOrder<'T> (f: 'T -> unit) (iter: PostOrderIterator<'T>) =
    match iter.Next() with
    | None -> ()
    | Some(v) ->
        f(v)
        iteratePostOrder f iter

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
    
    iteratePostOrder (printf "%d") (new PostOrderIterator<int>(t))
    printfn ""

    seqPostOrder t |> Seq.iter (printf "%d")
    printfn ""

    foldPostOrder (fun acc x -> x :: acc) [] t |> List.rev |> Seq.iter (printf "%d")
    printfn ""

    contPostOrder (printf "%d") t
    printfn ""

    0
