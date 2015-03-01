module Memoized

let memoized maxSize fn = 
    let cache = ref Map.empty
    let previousCalls = ref Queue.empty
    
    let updateCache arg result = 
        if Queue.length !previousCalls = maxSize then 
            let updatedPreviousCalls, oldestCall = Queue.dequeue !previousCalls
            previousCalls := updatedPreviousCalls
            cache := Map.remove oldestCall.Value !cache
        cache := Map.add arg result !cache
        previousCalls := Queue.enqueue arg !previousCalls
    
    let findCachedResult arg = Map.tryFind arg !cache
    
    let memoizedFn arg = 
        match findCachedResult arg with
        | Some result -> result
        | None -> 
            let result = fn arg
            updateCache arg result
            result
    memoizedFn
