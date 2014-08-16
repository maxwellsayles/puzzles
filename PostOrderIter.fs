(* This is another implementation of a stateful post order binary tree iterator.
   In this one, we build a stack of lambdas where each lambda represents the next
   operation, and returns the next value.  See `PushNode` for the core functionality.
*)
type Tree<'a> = Empty | Node of Tree<'a> * 'a * Tree<'a>

type PostOrderIter<'a>(root: Tree<'a>) as this =
    let mutable ops : List<unit -> 'a> = []
    do this.PushNode root

    member this.HasNext: bool =
        not <| List.isEmpty ops

    member this.Next(): 'a =
        match ops with
        | op :: opsTail ->
            ops <- opsTail
            op()
        | _ -> failwith "no more ops"

    member private this.OpNode (t: Tree<'a>) =
        this.PushNode t
        this.Next()

    member private this.PushNode(t: Tree<'a>) =
        match t with
        | Node(l, v, r) ->
            ops <- (fun () -> v) :: ops
            ops <- (fun () -> this.OpNode r) :: ops
            ops <- (fun () -> this.OpNode l) :: ops
        | Empty -> ()

let DoTree (t: Tree<'a>) =
    let iter = new PostOrderIter<'a>(t)
    while iter.HasNext do
        printf "%A " <| iter.Next()
    printfn ""

let Singleton(v: 'a) =
    Node(Empty, v, Empty)

let Node3(l: 'a, v: 'a, r: 'a) =
    Node(Singleton(l), v, Singleton(r))

[<EntryPoint>]
let main argv = 
    DoTree Empty
    DoTree <| Singleton 1
    DoTree <| Node3(1, 2, 3)
    DoTree <| Node(Node3(1, 2, 3), 4, Node3(5, 6, 7))
    
    0 // return an integer exit code
