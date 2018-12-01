namespace HelloFs.Tests

open Xunit
open FsUnit.Xunit

type ``An Empty Queue``() = 
    let emptyQueue = Queue.empty
    
    [<Fact>]
    member x.``Should return None item On Dequeue``() = 
        let _, item = Queue.dequeue emptyQueue
        item |> should equal None
    
    [<Fact>]
    member x.``Should returns empty queue On Deqeue``() = 
        let queue, _ = Queue.dequeue emptyQueue
        Queue.isEmpty queue |> should be True
    
    [<Fact>]
    member x.``Should returns an empty list on toList``() = Queue.toList emptyQueue |> should be Empty
    
    [<Fact>]
    member x.``Should have zero length``() = Queue.length emptyQueue |> should equal 0
    
    [<Fact>]
    member x.``Should be empty``() = Queue.isEmpty emptyQueue |> should be True
    
    [<Fact>]
    member x.``Should allow item to be queued``() = 
        let nonEmptyQueue = Queue.enqueue 42 emptyQueue
        Queue.length nonEmptyQueue |> should equal 1
    
    [<Fact>]
    member x.``Should returns None on tryFind``() = 
        let result = Queue.tryFind (fun _ -> true) emptyQueue
        result |> should equal None
    
    [<Fact>]
    member x.``Should throw an exepction on find``() = 
        (fun () -> Queue.find (fun _ -> true) emptyQueue |> ignore) 
            |> should (throwWithMessage "No value in queue that matches the predicate specified") typeof<System.Exception>
    
    [<Fact>]
    member x.``Should return None on peek``() = Queue.peek emptyQueue |> should equal None

type ``A Non Empty Queue``() = 
    let nonEmptyQueue = Queue.ofList [ 1; 2; 3; 4; 5 ]
    
    [<Fact>]
    member x.``Should have non zero length``() = Queue.length nonEmptyQueue |> should equal 5
    
    [<Fact>]
    member x.``Should not be empty``() = Queue.isEmpty nonEmptyQueue |> should be False
    
    [<Fact>]
    member x.``Should return top item on dequeue``() = 
        let _, item = Queue.dequeue nonEmptyQueue
        item |> should equal (Some 5)
    
    [<Fact>]
    member x.``Should return queue without top item on dequeue``() = 
        let queue, _ = Queue.dequeue nonEmptyQueue
        Queue.length queue |> should equal 4
    
    [<Fact>]
    member x.``Should allow item to be queued``() = 
        let queue = Queue.enqueue 42 nonEmptyQueue
        Queue.length queue |> should equal 6
    
    [<Fact>]
    member x.``Should return non empty list on toList``() = Queue.toList nonEmptyQueue |> should equal [ 1; 2; 3; 4; 5 ]
    
    [<Fact>]
    member x.``Once all items have been dequeed should be empty``() = 
        let emptyQueue = 
            nonEmptyQueue
            |> Queue.dequeue
            |> fst
            |> Queue.dequeue
            |> fst
            |> Queue.dequeue
            |> fst
            |> Queue.dequeue
            |> fst
            |> Queue.dequeue
            |> fst
        Queue.isEmpty emptyQueue |> should be True
    
    [<Fact>]
    member x.``Should returns Some on tryFind with valid predicate``() = 
        let result = Queue.tryFind (fun x -> x = 3) nonEmptyQueue
        result |> should equal (Some 3)
    
    [<Fact>]
    member x.``Should returns None on tryFind with invalid predicate``() = 
        let result = Queue.tryFind (fun _ -> false) nonEmptyQueue
        result |> should equal None
    
    [<Fact>]
    member x.``Should return value on find with valid predicate``() = 
        Queue.find (fun x -> x = 3) nonEmptyQueue |> should equal 3
    
    [<Fact>]
    member x.``Should throw an exepction on find with invalid predicate``() = 
        (fun () -> Queue.find (fun _ -> false) nonEmptyQueue |> ignore) 
            |> should (throwWithMessage "No value in queue that matches the predicate specified") typeof<System.Exception>
    
    [<Fact>]
    member x.``Should return Some on peek``() = Queue.peek nonEmptyQueue |> should equal (Some 5)
