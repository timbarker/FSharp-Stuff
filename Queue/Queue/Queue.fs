module Queue

type queue<'a> = 
    | EmptyQueue
    | Queue of 'a list * 'a list

let empty = EmptyQueue

let enqueue queue x = 
    match queue with
    | EmptyQueue -> Queue([ x ], [])
    | Queue(xs, ys) -> Queue(x :: xs, ys)

let rec dequeue = 
    function 
    | EmptyQueue -> EmptyQueue, None
    | Queue([], y :: []) -> EmptyQueue, Some y
    | Queue(xs, y :: ys) -> Queue(xs, ys), Some y
    | Queue(xs, []) -> dequeue (Queue([], List.rev xs))

let length = 
    function 
    | EmptyQueue -> 0
    | Queue(xs, ys) -> List.length xs + List.length ys

let ofList = 
    function 
    | [] -> EmptyQueue
    | xs -> Queue([], List.rev xs)

let toList = 
    function 
    | EmptyQueue -> []
    | Queue(xs, ys) -> xs @ List.rev ys

let isEmpty = 
    function 
    | EmptyQueue -> true
    | _ -> false

let peek = 
    function 
    | EmptyQueue -> None
    | Queue(_, y :: _) -> Some y
    | Queue(xs, []) -> 
        xs
        |> List.reduce (fun _ -> id)
        |> Some

let tryFind predicate queue = 
    match queue with
    | EmptyQueue -> None
    | Queue(xs, ys) -> 
        seq { 
            yield! xs
            yield! ys
        }
        |> Seq.tryFind predicate

let find predicate queue = 
    match tryFind predicate queue with
    | Some value -> value
    | _ -> failwith "No value in queue that matches the predicate specified"
