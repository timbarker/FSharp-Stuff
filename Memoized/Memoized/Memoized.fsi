module Memoized

val memoized : int -> ('a -> 'b) -> ('a -> 'b) when 'a : comparison