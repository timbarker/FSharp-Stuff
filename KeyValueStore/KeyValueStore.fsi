module KeyValueStore

[<Sealed>]
type KeyValueStore<'K, 'V when 'K : comparison> = 
    interface System.IDisposable
    new : unit -> KeyValueStore<'K, 'V>
    member Set : 'K -> 'V -> unit
    member Get : 'K -> 'V option Async
    member Delete : 'K -> unit
    member Clear : unit -> unit
    member Count : int Async
