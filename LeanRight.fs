(*
Given an arbitrary BST, return a BST on the same values
such that no node has a left child.

This is done by recursively rotating right on a node until
there is no left child, and then recursing on the right child.

This is O(n) runtime and O(n) memory.
*)
    
open System

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

// Generate a random BST on the range [l..h] inclusive.
let rec RandomBst(l: int, h: int, r: System.Random): int tree =
    if   l > h then Leaf
    elif l = h then Node(l, Leaf, Leaf)
    else let d = h - l + 1
         let v = (r.Next() % d) + l
         Node(v, RandomBst(l, v-1, r), RandomBst(v+1, h, r))

// Human readable tree.
let rec TreeToString(t: 'a tree): string =
    match t with
    | Leaf -> ""
    | Node(v, Leaf, Leaf) ->
        String.Format("{0}", v)
    | Node(v, l, Leaf) ->
        String.Format("({0} {1})", TreeToString(l), v)
    | Node(v, Leaf, r) ->
        String.Format("({0} {1})", v, TreeToString(r))
    | Node(v, l, r) ->
        String.Format("({0} {1} {2})", TreeToString(l), v, TreeToString(r))

// Perform a right rotation of the subtree rooted at the input node
// and return the new root (i.e. the left child of the input subtree).
let RotateRight = function
    | Leaf                         -> Leaf
    | Node(v, Leaf, r)             -> Node(v, Leaf, r)
    | Node(v, Node(lv, ll, lr), r) -> Node(lv, ll, Node(v, lr, r))

// Convert a binary tree into a right leaning tree.
// The idea is to perform right rotations on the current node, until
// it does not have a left child, then recurse on the right child.
//
// The function implemented is the continuation passing style of
// the following function:
//
// let rec LeanRight t =
//     match t with
//     | Leaf             -> Leaf
//     | Node(v, Leaf, r) -> Node(v, Leaf, LeanRight r)
//     | Node(v, l, r)    -> LeanRight(RotateRight t)
let rec LeanRight'(t, k) =
    match t with
    | Leaf             -> k Leaf
    | Node(v, Leaf, r) -> LeanRight'(r, fun r' -> k (Node(v, Leaf, r')))
    | Node(v, l, r)    -> LeanRight'(RotateRight t, k)
let LeanRight t = LeanRight'(t, fun x -> x)

// Create a random binary search tree on the elements 0 throuugh 50
// and then perform rotations on the tree until it is entirely right leaning.
let r = System.Random()
let t = RandomBst(0, 50, r)
printfn "%s" <| TreeToString t
printfn "%s" << TreeToString <| LeanRight t


