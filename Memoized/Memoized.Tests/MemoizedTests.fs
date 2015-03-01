namespace Memoized.Tests

open NUnit.Framework
open FsUnit

[<TestFixture>]
type ``An empty cache``() = 
    let mutable calls = []
    
    let fn arg = 
        calls <- arg :: calls
        arg * 2
    
    let emptyCache = Memoized.memoized 100 fn
    
    [<SetUp>]
    member x.Setup() = calls <- []
    
    [<Test>]
    member x.``Should always call cached function``() = 
        emptyCache 42 |> ignore
        calls |> should equal [ 42 ]
    
    [<Test>]
    member x.``Should return the cached function result``() = emptyCache 42 |> should equal 84

[<TestFixture>]
type ``A Non Empty Cache``() = 
    let mutable calls = []
    
    let fn arg = 
        calls <- arg :: calls
        arg * 2
    
    let cache = Memoized.memoized 100 fn
    
    [<SetUp>]
    member x.Setup() = 
        cache 42 |> ignore
        calls <- []
    
    [<Test>]
    member x.``Should not call cached function on cache hit``() = 
        cache 42 |> ignore
        calls |> should equal []
    
    [<Test>]
    member x.``Should call cached function on cache miss``() = 
        cache 99 |> ignore
        calls |> should equal [ 99 ]
    
    [<Test>]
    member x.``Should return the cached result``() = cache 42 |> should equal 84

type ``A Size Limited Cache``() = 
    let mutable calls = []
    
    let fn arg = 
        calls <- arg :: calls
        arg * 2
    
    let cache = Memoized.memoized 1 fn
    
    [<SetUp>]
    member x.Setup() = 
        cache 42 |> ignore
        calls <- []
    
    [<Test>]
    member x.``Should only cache the last n calls``() = 
        cache 99 |> ignore
        cache 42 |> ignore
        calls |> should equal [ 42; 99 ]
