// description:
// consider the following set of blocks (each block is an X)
//
//     8|
//     7|             X X 
//     6|             X X X
//     5|   X         X X X
//     4|   X       X X X X
//     3|   X     X X X X X
//     2| X X   X X X X X X
//     1| X X X X X X X X X
//      +------------------
// Imagine it rains and the water fills up in the blocks
// e.g. the following puddle will be formed (W is a block of water)
//
//     8|
//     7|             X X 
//     6|             X X X
//     5|   X W W W W X X X
//     4|   X W W W X X X X
//     3|   X W W X X X X X
//     2| X X W X X X X X X
//     1| X X X X X X X X X
//      +------------------
// How many blocks of water are there?

// Solution 1.

// scan left and right for max, find min water height, subtract from height, then sum differences
let scan folder = 
    function 
    | [] -> Seq.empty
    | x :: xs -> Seq.scan folder x xs

let scanBack folder list = 
    match List.rev list with
    | [] -> Seq.empty
    | x :: xs -> Seq.scanBack folder (Seq.rev xs) x

let calculateWater1 blocks = 
    Seq.zip (Seq.zip (scan max blocks) (scanBack max blocks) |> Seq.map (fun (a, b) -> min a b)) blocks
    |> Seq.map (fun (a, b) -> a - b)
    |> Seq.sum

// Soltion 2.

// create a tree with the cached max values for each column, then traverse the tree top to bottom passing down the max to left and right
let split list = 
    match List.splitInto 2 list with
    | [ x ] -> (x, [])
    | x :: y :: [] -> (x, y)
    | _ -> failwith "Wat?"

type CachedTree<'a> = 
    | NullNode
    | LeafNode of 'a
    | PairNode of 'a * CachedTree<'a> * CachedTree<'a>
    member x.Value = 
        match x with
        | NullNode -> failwith "NullNode has no value"
        | LeafNode x -> x
        | PairNode(x, _, _) -> x

let rec createMaxCachedTree = 
    function 
    | [] -> NullNode
    | [ x ] -> LeafNode x
    | list -> 
        let (xs, ys) = split list
        let (left, right) = (createMaxCachedTree xs, createMaxCachedTree ys)
        PairNode(max left.Value right.Value, left, right)

let rec calculateWaterTree tree maxToLeft maxToRight = 
    match tree with
    | PairNode(_, left, right) -> 
        calculateWaterTree left maxToLeft (max right.Value maxToRight) 
        + calculateWaterTree right (max maxToLeft left.Value) maxToRight
    | LeafNode x -> max (min maxToLeft maxToRight) x - x
    | NullNode -> 0

let calculateWater2 blocks = calculateWaterTree (createMaxCachedTree blocks) 0 0

let timed fn = 
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let result = fn()
    sw.Stop()
    (result, sw.ElapsedMilliseconds)

[<EntryPoint>]
let main argv = 
    let rnd = System.Random()
    let blocks = List.init 1000000 (fun _ -> rnd.Next(1, 10))
    //let blocks = [ 6; 3; 2; 4; 1; 3; 4; 1 ] // = 7
    timed (fun () -> calculateWater1 blocks) |> printfn "%A"
    timed (fun () -> calculateWater2 blocks) |> printfn "%A"
    0