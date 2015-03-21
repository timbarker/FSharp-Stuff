module ResultMonad.Tests

open NUnit.Framework
open FsUnit
open Result

let testBinder s = Success (s + ", World!")
let testMapper s = s + ", World!"
let testFolder s v = String.length v + s
let success = Success "Hello"
let failure = Failure "Oh No!"

[<TestFixture>]
type ``A Success Result``() = 

    [<Test>]
    member x.``Should bind correctly``() =          
        let result = Result.bind testBinder success
        match result with
        | Success("Hello, World!") -> Assert.Pass()
        | _ -> Assert.Fail()

    [<Test>]
    member x.``Should fold correctly``() =
        let result = Result.fold testFolder 1 success
        result |> should equal 6

    [<Test>]
    member x.``Should map correctly``() =
        let result = Result.map testMapper success
        match result with
        | Success("Hello, World!") -> Assert.Pass()
        | _ -> Assert.Fail()

    [<Test>]
    member x.``Should iter correctly``() =
        let result = ref ""
        Result.iter success (fun v -> result := v)
        !result |> should equal "Hello"

    [<Test>]
    member x.``isSuccess should be true``() =
        Result.isSuccess success |> should be True

    [<Test>]
    member x.``isFailure should be false``() =
        Result.isFailure success |> should be False

[<TestFixture>]
type ``A Failure Result``() = 

    [<Test>]
    member x.``Should bind correctly``() =          
        let result = Result.bind testBinder failure
        match result with
        | Failure("Oh No!") -> Assert.Pass()
        | _ -> Assert.Fail()

    [<Test>]
    member x.``Should fold correctly``() =
        let result = Result.fold testFolder 1 failure
        result |> should equal 1

    [<Test>]
    member x.``Should map correctly``() =
        let result = Result.map testMapper failure
        match result with
        | Failure("Oh No!") -> Assert.Pass()
        | _ -> Assert.Fail()

    [<Test>]
    member x.``Should iter correctly``() =
        let result = ref ""
        Result.iter failure (fun v -> result := v)
        !result |> should equal ""

    [<Test>]
    member x.``isSuccess should be false``() =
        Result.isSuccess failure |> should be False

    [<Test>]
    member x.``isFailure should be true``() =
        Result.isFailure failure |> should be True