module SudokuSolver

let toMap (sudoku : int list list) = 
    sudoku
    |> Seq.concat
    |> Seq.mapi (fun idx item -> ((idx % 9, idx / 9), item))
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

let canAddNumber (col, row) number sudoku = 
    let canAddToCol = 
        lazy (sudoku
              |> Map.filter (fun (c, _) _ -> c = col)
              |> Map.forall (fun _ n -> n <> number))
    
    let canAddToRow = 
        lazy (sudoku
              |> Map.filter (fun (_, r) _ -> r = row)
              |> Map.forall (fun _ n -> n <> number))
    
    let canAddToSquare = 
        lazy (sudoku
              |> Map.filter (fun (c, r) _ -> c / 3 = col / 3 && r / 3 = row / 3)
              |> Map.forall (fun _ n -> n <> number))
    
    canAddToCol.Value && canAddToRow.Value && canAddToSquare.Value

let rec solveAt idx sudoku = 
    let col = idx % 9
    let row = idx / 9
    if idx = 81 then Some sudoku
    else if Map.containsKey (col, row) sudoku then solveAt (idx + 1) sudoku
    else 
        let trySolveWith num = 
            if canAddNumber (col, row) num sudoku then solveAt (idx + 1) (Map.add (col, row) num sudoku)
            else None
        seq { 1..9 } |> Seq.tryPick trySolveWith

let solve (sudoku : int list list) = 
    let solved = solveAt 0 (toMap sudoku)
    match solved with
    | Some solution -> toGrid solution
    | None -> failwith "Unsolvable Sudoku"
