module ComputationExpressionTests

open Xunit
open FsUnit.Xunit
open Result

let div n d = 
    if d = 0 then Failure "Divide by zero"
    else Success(n / d)

let modulo n d = 
    if d = 0 then Failure "Modulo by zero"
    else Success(n % d)

let computation a b = result { let! r1 = div a b
                               let! r2 = modulo a r1
                               return r2 }

type ``Computation Expression Tests``() =
    [<Fact>]
    member x.``Computation divde by zero test``() =
        match computation 10 0 with
        | Failure "Divide by zero" -> ()
        | _ -> true |> should be False
         
    [<Fact>]
    member x.``Computation modulo by zero test``() =
        match computation 10 20 with
        | Failure "Modulo by zero" -> ()
        | _ -> true |> should be False

    [<Fact>]
    member x.``Computation result test``() =
        match computation 10 10 with
        | Success 0 -> ()
        | _ -> true |> should be False
        