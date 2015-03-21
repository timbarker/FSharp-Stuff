module Result

type Result<'TSuccess, 'TFailure> = 
| Success of 'TSuccess
| Failure of 'TFailure

let fold folder state result = 
    match result with
    | Success s -> folder state s
    | Failure _ -> state

let bind binder result = 
    match result with
    | Success s -> binder s
    | Failure f -> Failure f

let map mapping result = 
    match result with
    | Success s -> Success(mapping s)
    | Failure f -> Failure f

let iter result action = fold (fun _ -> action) () result
let isSuccess result = fold (fun _ _ -> true) false result
let isFailure result = fold (fun _ _ -> false) true result

[<Sealed>]
type ResultBuilder() = 
    member this.Bind(result, fn) = bind fn result
    member this.Return(v) = Success v

let result = ResultBuilder()

