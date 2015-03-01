module Queue

type queue<'a>

val empty : 'a queue
val enqueue : 'a -> 'a queue -> 'a queue
val dequeue : 'a queue -> 'a queue * 'a option
val length : 'a queue -> int
val ofList : 'a list -> 'a queue
val toList : 'a queue -> 'a list
val isEmpty : 'a queue -> bool
val peek : 'a queue -> 'a option
val tryFind : ('a -> bool) -> 'a queue -> 'a option
val find : ('a -> bool) -> 'a queue -> 'a
