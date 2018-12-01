namespace Trie.Tests

open Xunit;
open FsUnit.Xunit
open Trie

type Test() = 
    
    [<Fact>]
    member __.``TryTind when element does exit``() = 
        let trie = 
            Trie.ofSeq [ ("4471", "Orange")
                         ("4472", "Vodafone")
                         ("4473", "T-Mobile") ]
        trie |> Trie.tryFind "4472" |> should equal <| Some("Vodafone")
    
    [<Fact>]
    member __.``TryTind when element does not exit``() = 
        let trie = 
            Trie.ofSeq [ ("4471", "Orange")
                         ("4472", "Vodafone")
                         ("4473", "T-Mobile") ]
        trie |> Trie.tryFind "4474" |> should equal <| None
    
    [<Fact>]
    member __.``HasKeyWithPrefix when keys with prefix exist``() = 
        let trie = 
            Trie.ofSeq [ ("4471", "Orange")
                         ("4472", "Vodafone")
                         ("4473", "T-Mobile") ]
        trie |> Trie.hasKeyWithPrefix "447" |> should be True

    [<Fact>]
    member __.``HasKeyWithPrefix when no keys with prefix exist``() = 
        let trie = 
            Trie.ofSeq [ ("4471", "Orange")
                         ("4472", "Vodafone")
                         ("4473", "T-Mobile") ]
        trie |> Trie.hasKeyWithPrefix "336" |> should be False
