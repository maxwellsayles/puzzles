(*
Given an arbitrary BST, return a BST on the same values
such that no node has a left child.

This is done by recursively rotating right on a node until
there is no left child of a node, and then recursing on the
right child.

This is O(n) runtime and O(n) memory.
*)
    
open System

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec RandomBst(l: int, h: int, r: System.Random): int tree =
    match l, h with
    | _ when l > h -> Leaf
    | _ when l = h -> Node (l, Leaf, Leaf)
    | _ -> let d = h - l + 1
           let v = (r.Next() % d) + l
           Node(v, RandomBst(l, v-1, r), RandomBst(v+1, h, r))

let rec TreeToString(t: int tree): string =
    match t with
    | Leaf -> ""
    | Node(v, Leaf, Leaf) -> String.Format("{0}", v)
    | Node(v, l, Leaf) -> String.Format("({0} {1})", TreeToString(l), v)
    | Node(v, Leaf, r) -> String.Format("({0} {1})", v, TreeToString(r))
    | Node(v, l, r) -> String.Format("({0} {1} {2})", TreeToString(l), v, TreeToString(r))


let RotateRight = function
    | Leaf -> Leaf
    | Node(v, Leaf, r) -> Node(v, Leaf, r)
    | Node(v, Node(lv, ll, lr), r) -> Node(lv, ll, Node(v, lr, r))

let rec LeanRight t =
    match t with
    | Leaf -> Leaf
    | Node(v, Leaf, r) -> Node(v, Leaf, LeanRight r)
    | Node(v, l, r) -> LeanRight(RotateRight t)

let r = System.Random()
let t = RandomBst(0, 10, r)
printfn "%s" <| TreeToString t
printfn "%s" << TreeToString <| LeanRight t

