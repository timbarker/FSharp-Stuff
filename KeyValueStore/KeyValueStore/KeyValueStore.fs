module KeyValueStore

type StoreOperation<'K, 'V when 'K : comparison> = 
    | Set of 'K * 'V
    | Get of 'K * 'V option AsyncReplyChannel
    | Delete of 'K
    | Count of int AsyncReplyChannel
    | Clear

[<Sealed>]
type KeyValueStore<'K, 'V when 'K : comparison>() = 
    let cts = new System.Threading.CancellationTokenSource()
    
    let store = 
        MailboxProcessor.Start((fun inbox -> 
                               let rec loop table = 
                                   async { 
                                       let! operation = inbox.Receive()
                                       match operation with
                                       | Set(key, value) -> return! loop (Map.add key value table)
                                       | Get(key, chan) -> 
                                           chan.Reply(Map.tryFind key table)
                                           return! loop table
                                       | Delete key -> return! loop (Map.remove key table)
                                       | Clear -> return! loop Map.empty
                                       | Count chan -> 
                                           chan.Reply(table.Count)
                                           return! loop table
                                   }
                               loop Map.empty), cts.Token)
    
    interface System.IDisposable with
        member this.Dispose() = cts.Cancel()
    
    member this.Set (key : 'K) (value : 'V) = store.Post(Set(key, value))
    member this.Get key = store.PostAndAsyncReply(fun chan -> Get(key, chan))
    member this.Delete key = store.Post(Delete key)
    member this.Clear() = store.Post Clear
    member this.Count = store.PostAndAsyncReply Count
