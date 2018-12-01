module SudokuSolver

let toCoordinate index = (index % 9, index / 9)

let toMap sudoku = 
    sudoku
    |> Seq.concat
    |> Seq.mapi (fun index item -> (toCoordinate index, item))
    |> Seq.filter (fun (_, item) -> item <> 0)
    |> Map.ofSeq

let toGrid sudoku = 
    seq { 
        for row in 0..8 do
            yield seq { 
                      for col in 0..8 do
                          yield Map.find (col, row) sudoku
                  }
                  |> Seq.toList
    }
    |> Seq.toList

let canAdd number (col, row) sudoku = 
    let canAddWith filter = 
        sudoku
        |> Map.filter (fun coord _ -> filter coord)
        |> Map.forall (fun _ n -> n <> number)
    
    let colFilter (c, _) = c = col
    let rowFilter (_, r) = r = row
    let squareFilter (c, r) = c / 3 = col / 3 && r / 3 = row / 3
    (canAddWith colFilter) && (canAddWith rowFilter) && (canAddWith squareFilter)

let rec solveAt index sudoku = 
    if index = 81 then Some sudoku
    else 
        let coordinate = toCoordinate index
        if Map.containsKey coordinate sudoku then solveAt (index + 1) sudoku
        else 
            let trySolveUsing number = 
                if canAdd number coordinate sudoku then solveAt (index + 1) (Map.add coordinate number sudoku)
                else None
            List.tryPick trySolveUsing [ 1..9 ]

let solve (sudoku : int list list) = 
    match sudoku
          |> toMap
          |> solveAt 0 with
    | Some solution -> toGrid solution
    | None -> invalidArg "sudoku" "Unsolvable Sudoku"
