namespace HelloFs.Tests

open NUnit.Framework
open FsUnit

[<TestFixture>]
type ``An Empty Queue``() = 
    let emptyQueue = Queue.empty
    
    [<Test>]
    member x.``Should return None item On Dequeue``() = 
        let _, item = Queue.dequeue emptyQueue
        item |> should equal None
    
    [<Test>]
    member x.``Should returns empty queue On Deqeue``() = 
        let queue, _ = Queue.dequeue emptyQueue
        Queue.isEmpty queue |> should be True
    
    [<Test>]
    member x.``Should returns an empty list on toList``() = Queue.toList emptyQueue |> should be Empty
    
    [<Test>]
    member x.``Should have zero length``() = Queue.length emptyQueue |> should equal 0
    
    [<Test>]
    member x.``Should be empty``() = Queue.isEmpty emptyQueue |> should be True
    
    [<Test>]
    member x.``Should allow item to be queued``() = 
        let nonEmptyQueue = Queue.enqueue emptyQueue 42
        Queue.length nonEmptyQueue |> should equal 1
    
    [<Test>]
    member x.``Should returns None on tryFind``() = 
        let result = Queue.tryFind (fun _ -> true) emptyQueue
        result |> should equal None
    
    [<Test>]
    [<ExpectedException(ExpectedMessage = "No value in queue that matches the predicate specified")>]
    member x.``Should throw an exepction on find``() = Queue.find (fun _ -> true) emptyQueue |> ignore
    
    [<Test>]
    member x.``Should return None on peek``() = Queue.peek emptyQueue |> should equal None

[<TestFixture>]
type ``A Non Empty Queue``() = 
    let nonEmptyQueue = Queue.ofList [ 1; 2; 3; 4; 5 ]
    
    [<Test>]
    member x.``Should have non zero length``() = Queue.length nonEmptyQueue |> should equal 5
    
    [<Test>]
    member x.``Should not be empty``() = Queue.isEmpty nonEmptyQueue |> should be False
    
    [<Test>]
    member x.``Should return top item on dequeue``() = 
        let _, item = Queue.dequeue nonEmptyQueue
        item |> should equal (Some 5)
    
    [<Test>]
    member x.``Should return queue without top item on dequeue``() = 
        let queue, _ = Queue.dequeue nonEmptyQueue
        Queue.length queue |> should equal 4
    
    [<Test>]
    member x.``Should allow item to be queued``() = 
        let queue = Queue.enqueue nonEmptyQueue 42
        Queue.length queue |> should equal 6
    
    [<Test>]
    member x.``Should return non empty list on toList``() = Queue.toList nonEmptyQueue |> should equal [ 1; 2; 3; 4; 5 ]
    
    [<Test>]
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
    
    [<Test>]
    member x.``Should returns Some on tryFind with valid predicate``() = 
        let result = Queue.tryFind (fun x -> x = 3) nonEmptyQueue
        result |> should equal (Some 3)
    
    [<Test>]
    member x.``Should returns None on tryFind with invalid predicate``() = 
        let result = Queue.tryFind (fun _ -> false) nonEmptyQueue
        result |> should equal None
    
    [<Test>]
    member x.``Should return value on find with valid predicate``() = 
        Queue.find (fun x -> x = 3) nonEmptyQueue |> should equal 3
    
    [<Test>]
    [<ExpectedException(ExpectedMessage = "No value in queue that matches the predicate specified")>]
    member x.``Should throw an exepction on find with invalid predicate``() = 
        Queue.find (fun _ -> false) nonEmptyQueue |> ignore
    
    [<Test>]
    member x.``Should return Some on peek``() = Queue.peek nonEmptyQueue |> should equal (Some 5)
