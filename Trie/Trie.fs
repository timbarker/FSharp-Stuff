namespace Trie

module Trie = 
    exception NotFound
    
    type key = string
    
    type trie<'a when 'a : comparison> = 
        | Node of 'a option * Map<char, 'a trie>
    
    let empty = Node(None, Map.empty)
    
    let find (key : key) trie = 
        let rec find key trie = 
            match (key, trie) with
            | [], Node(None, _) -> raise NotFound
            | [], Node(Some v, _) -> v
            | x :: xs, Node(_, m) -> 
                match Map.tryFind x m with
                | Some t -> find xs t
                | None -> raise NotFound
        find (List.ofSeq key) trie
    
    let tryFind key trie = 
        try 
            Some(find key trie)
        with NotFound -> None
    
    let contains (key : key) trie = 
        let rec contains key trie = 
            match (key, trie) with
            | [], Node(None, _) -> false
            | [], Node(Some _, _) -> true
            | x :: xs, Node(_, m) -> 
                match Map.tryFind x m with
                | Some t -> contains xs t
                | None -> false
        contains (List.ofSeq key) trie
    
    let add (key : key) value trie = 
        let rec insert = 
            function 
            | [], Node(_, m) -> Node(Some value, m)
            | x :: xs, Node(v, m) -> 
                let subTrie = 
                    match Map.tryFind x m with
                    | Some t -> insert (xs, t)
                    | None -> insert (xs, empty)
                Node(v, Map.add x subTrie m)
        insert ((List.ofSeq key), trie)
    
    let remove (key : key) trie = 
        let rec remove key trie = 
            match (key, trie) with
            | [], Node(_, m) -> Node(None, m)
            | x :: xs, Node(v, m) -> 
                match Map.tryFind x m with
                | Some t -> 
                    let subTrie = remove xs t
                    Node(v, 
                         if subTrie = empty then Map.remove x m
                         else Map.add x subTrie m)
                | None -> trie
        remove (List.ofSeq key) trie
    
    let ofSeq seq = 
        let rec make seq trie = 
            if Seq.isEmpty seq then trie
            else 
                let item = Seq.head seq
                make (Seq.tail seq) (add (fst item) (snd item) trie)
        make seq empty
    
    let hasKeyWithPrefix (key : key) trie = 
        let rec hasKeyWithPrefix key trie = 
            match (key, trie) with
            | [], Node(_, m) -> not (Map.isEmpty m)
            | x :: xs, Node(_, m) -> 
                match Map.tryFind x m with
                | None -> false
                | Some(subTrie) -> hasKeyWithPrefix xs subTrie
        hasKeyWithPrefix (List.ofSeq key) trie
