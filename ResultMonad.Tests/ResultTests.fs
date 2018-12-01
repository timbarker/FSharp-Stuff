module ResultMonad.Tests

open Xunit
open FsUnit.Xunit
open Result

let testBinder s = Success (s + ", World!")
let testMapper s = s + ", World!"
let testFolder s v = String.length v + s
let success = Success "Hello"
let failure = Failure "Oh No!"

type ``A Success Result``() = 

    [<Fact>]
    member x.``Should bind correctly``() =          
        let result = Result.bind testBinder success
        match result with
        | Success("Hello, World!") -> ()
        | _ -> true |> should be False

    [<Fact>]
    member x.``Should fold correctly``() =
        let result = Result.fold testFolder 1 success
        result |> should equal 6

    [<Fact>]
    member x.``Should map correctly``() =
        let result = Result.map testMapper success
        match result with
        | Success("Hello, World!") -> ()
        | _ -> true |> should be False

    [<Fact>]
    member x.``Should iter correctly``() =
        let result = ref ""
        Result.iter success (fun v -> result := v)
        !result |> should equal "Hello"

    [<Fact>]
    member x.``isSuccess should be true``() =
        Result.isSuccess success |> should be True

    [<Fact>]
    member x.``isFailure should be false``() =
        Result.isFailure success |> should be False

type ``A Failure Result``() = 

    [<Fact>]
    member x.``Should bind correctly``() =          
        let result = Result.bind testBinder failure
        match result with
        | Failure("Oh No!") -> ()
        | _ -> true |> should be False

    [<Fact>]
    member x.``Should fold correctly``() =
        let result = Result.fold testFolder 1 failure
        result |> should equal 1

    [<Fact>]
    member x.``Should map correctly``() =
        let result = Result.map testMapper failure
        match result with
        | Failure("Oh No!") -> ()
        | _ -> true |> should be False

    [<Fact>]
    member x.``Should iter correctly``() =
        let result = ref ""
        Result.iter failure (fun v -> result := v)
        !result |> should equal ""

    [<Fact>]
    member x.``isSuccess should be false``() =
        Result.isSuccess failure |> should be False

    [<Fact>]
    member x.``isFailure should be true``() =
        Result.isFailure failure |> should be True