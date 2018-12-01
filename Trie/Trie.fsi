namespace Trie

module Trie = 

    type trie<'a when 'a : comparison>

    type key = string

    exception NotFound

    val empty : 'a trie
    val ofSeq : (key * 'a) seq -> 'a trie 
    val find : key -> 'a trie -> 'a
    val tryFind : key -> 'a trie -> 'a option
    val contains : key -> 'a trie -> bool
    val add : key -> 'a -> 'a trie -> 'a trie
    val remove : key -> 'a trie -> 'a trie
    val hasKeyWithPrefix : key -> 'a trie -> bool 
    //val prefixes : key -> 'a trie -> key list //Find all prefixes of a given key
    //val keys : key -> 'a trie -> key list // Find all keys from this trie that starts with a given prefix
    //val items : key -> 'a trie -> (key * 'a) list //Find all items from this trie that starts with a given prefix:

