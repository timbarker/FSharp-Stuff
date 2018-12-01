namespace Memoized.Tests

open Xunit
open FsUnit.Xunit

type ``An empty cache``() = 
    let mutable calls = []
    
    let fn arg = 
        calls <- arg :: calls
        arg * 2
    
    let emptyCache = Memoized.memoized 100 fn
    
    [<Fact>]
    member x.``Should always call cached function``() = 
        emptyCache 42 |> ignore
        calls |> should equal [ 42 ]
    
    [<Fact>]
    member x.``Should return the cached function result``() = emptyCache 42 |> should equal 84

type ``A Non Empty Cache``() = 
    let mutable calls = []
    
    let fn arg = 
        calls <- arg :: calls
        arg * 2
    
    let cache = Memoized.memoized 100 fn
    do cache 42 |> ignore
    do calls <- []
    
    [<Fact>]
    member x.``Should not call cached function on cache hit``() = 
        cache 42 |> ignore
        calls |> should haveLength 0
    
    [<Fact>]
    member x.``Should call cached function on cache miss``() = 
        cache 99 |> ignore
        calls |> should equal [ 99 ]
    
    [<Fact>]
    member x.``Should return the cached result``() = cache 42 |> should equal 84

type ``A Size Limited Cache``() = 
    let mutable calls = []
    
    let fn arg = 
        calls <- arg :: calls
        arg * 2
    
    let cache = Memoized.memoized 1 fn
    
    do cache 42 |> ignore
    do calls <- []
    
    [<Fact>]
    member x.``Should only cache the last n calls``() = 
        cache 99 |> ignore
        cache 42 |> ignore
        calls |> should equal [ 42; 99 ]
