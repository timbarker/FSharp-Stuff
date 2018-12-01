module KeyValueStoreTests

open Xunit
open FsUnit.Xunit
open System

type KeyValueStoreTests() = 
    let store = new KeyValueStore.KeyValueStore<_, _>()
    
    interface IDisposable with

        member x.Dispose() = (store :> System.IDisposable).Dispose()
    
    [<Fact>]
    member x.``adding item to store shound increment count``() = 
        async { 
            let! beforeInsertCount = store.Count
            store.Set "Test" 1
            let! afterInsertCount = store.Count
            afterInsertCount |> should equal (beforeInsertCount + 1)
        }
        |> Async.RunSynchronously
    
    [<Fact>]
    member x.``adding duplicate item to store shound not change count``() = 
        async { 
            store.Set "Test" 1
            let! beforeInsertCount = store.Count
            store.Set "Test" 1
            let! afterInsertCount = store.Count
            afterInsertCount |> should equal (beforeInsertCount)
        }
        |> Async.RunSynchronously
    
    [<Fact>]
    member x.``getting a value that does not exist results in none``() = 
        async { let! value = store.Get "invalid"
                value |> should be (equal None) } |> Async.RunSynchronously
    
    [<Fact>]
    member x.``getting a value that does exist results in some``() = 
        async { 
            store.Set "Test" 1
            let! value = store.Get "Test"
            value |> should be (equal (Some 1))
        }
        |> Async.RunSynchronously
    
    [<Fact>]
    member x.``deleting key removes it from store``() = 
        async { 
            store.Set "Test" 1
            store.Delete "Test"
            let! value = store.Get "Test"
            value |> should be (equal None)
        }
        |> Async.RunSynchronously
