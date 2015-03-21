module Result

type Result<'TSuccess, 'TFailure> = 
    | Success of 'TSuccess
    | Failure of 'TFailure

[<Sealed>]
type ResultBuilder = 
    member Bind : Result<'a, 'b> * ('a -> Result<'c, 'b>) -> Result<'c, 'b>
    member Return : 'a -> Result<'a, 'b>

val result : ResultBuilder
val fold : ('state -> 'a -> 'state) -> 'state -> Result<'a, 'b> -> 'state
val bind : ('a -> Result<'c, 'b>) ->  Result<'a, 'b> -> Result<'c, 'b>
val map : ('a -> 'b) -> Result<'a, 'c> -> Result<'b, 'c>
val iter : Result<'a, 'b> -> ('a -> unit) -> unit
val isSuccess: Result<_,_> -> bool
val isFailure: Result<_,_> -> bool