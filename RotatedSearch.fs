/// Search a sorted but rotated array for an element
/// The solution is to sample each endpoint of the subarray and a midpoint.
/// We then case out the conditions under which the element is either
/// in the left half or the right half.

let RotatedSearch (arr: int[]) (t: int) =
    let rec f i j =
        let k = (i + j) / 2
        let x, y, z = arr.[i], arr.[j], arr.[k]
        if i = j || i + 1 = j then
            if x = t then Some i elif y = t then Some j else None
        elif t >= x && t <= z then f i k
        elif t >= z && t <= y then f k j
        elif x > z && (t >= x || t <= z) then f i k
        else f k j
    let n = Seq.length arr
    if n = 0 then None else f 0 (n - 1)

[<EntryPoint>]
let main argv =
    assert(RotatedSearch [| |] 0 = None)
    
    assert(RotatedSearch [| 1 |] 0 = None)
    assert(RotatedSearch [| 1 |] 1 = Some 0)
    assert(RotatedSearch [| 1 |] 2 = None)

    assert(RotatedSearch [| 1; 2 |] 0 = None)
    assert(RotatedSearch [| 1; 2 |] 1 = Some 0)
    assert(RotatedSearch [| 1; 2 |] 2 = Some 1)
    assert(RotatedSearch [| 1; 2 |] 3 = None)
    assert(RotatedSearch [| 2; 1 |] 0 = None)
    assert(RotatedSearch [| 2; 1 |] 1 = Some 1)
    assert(RotatedSearch [| 2; 1 |] 2 = Some 0)
    assert(RotatedSearch [| 2; 1 |] 3 = None)

    assert(RotatedSearch [| 1; 2; 3 |] 0 = None)
    assert(RotatedSearch [| 1; 2; 3 |] 1 = Some 0)
    assert(RotatedSearch [| 1; 2; 3 |] 2 = Some 1)
    assert(RotatedSearch [| 1; 2; 3 |] 3 = Some 2)
    assert(RotatedSearch [| 1; 2; 3 |] 4 = None)
    assert(RotatedSearch [| 2; 3; 1 |] 0 = None)
    assert(RotatedSearch [| 2; 3; 1 |] 1 = Some 2)
    assert(RotatedSearch [| 2; 3; 1 |] 2 = Some 0)
    assert(RotatedSearch [| 2; 3; 1 |] 3 = Some 1)
    assert(RotatedSearch [| 2; 3; 1 |] 4 = None)
    assert(RotatedSearch [| 3; 1; 2 |] 0 = None)
    assert(RotatedSearch [| 3; 1; 2 |] 1 = Some 1)
    assert(RotatedSearch [| 3; 1; 2 |] 2 = Some 2)
    assert(RotatedSearch [| 3; 1; 2 |] 3 = Some 0)
    assert(RotatedSearch [| 3; 1; 2 |] 4 = None)

    let n = 1489  // This is prime
    for j in 0..n-1 do
        let arr = [| for k in 0..n-1 -> (k + j) % n |]
        for i in 0..n-1 do    
            let res = RotatedSearch arr i
            assert(res = Some((i - j + n) % n))

    0
