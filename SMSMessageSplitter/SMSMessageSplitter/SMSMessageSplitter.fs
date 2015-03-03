module SMSMessageSplitter

open System

let getCharacterInfoGSM char = 
    match char with
    | '|' | '^' | '€' | '{' | '}' | '[' | '~' | ']' | '\\' -> (char, 2)
    | _ -> (char, 1)

let getCharacterInfoUnicode char = 
    match char with
    | c when Char.IsHighSurrogate(c) -> (c, 4)
    | c when Char.IsLowSurrogate(c) -> (c, 0)
    | c -> (c, 2)

let splitter partLength (state : (char list * int) list) (char : char * int) = 
    match state, char with
    | (part, partSz) :: parts, (char, charSz) when partSz + charSz <= partLength -> 
        (char :: part, partSz + charSz) :: parts
    | parts, (char, charSz) -> ([ char ], charSz) :: parts

let toString (arr : char array) = new String(arr)

let split getCharInfo partSize singlePartSize (message: string) = 
    let split = 
        message
        |> Seq.map getCharInfo
        |> Seq.fold (splitter partSize) [ ([], 0) ]
        |> List.rev
    
    let effectiveTotalLength = split |> List.sumBy snd
    if effectiveTotalLength = singlePartSize then 
        [ split
          |> Seq.map fst
          |> Seq.map List.rev
          |> Seq.concat
          |> Seq.toArray
          |> toString ]
    else 
        split
        |> Seq.map fst
        |> Seq.map List.rev
        |> Seq.map (Seq.toArray >> toString)
        |> Seq.toList

let splitGSM = split getCharacterInfoGSM 153 160
let splitUnicode = split getCharacterInfoUnicode 134 140

let truncateGSM = split getCharacterInfoGSM 160 160 >> List.head
let truncateUnicode = split getCharacterInfoUnicode 140 140 >> List.head