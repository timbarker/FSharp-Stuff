module SMSMessageSplitter

open System

let getCharacterInfoGSM char = 
    match char with
    | '|' | '^' | '€' | '{' | '}' | '[' | '~' | ']' | '\\' -> (char, 2)
    | _ -> (char, 1)

let getCharacterInfoUnicode char = 
    match char with
    | _ when Char.IsHighSurrogate(char) -> (char, 4)
    | _ when Char.IsLowSurrogate(char) -> (char, 0)
    | _ -> (char, 2)

let splitter partLength state charInfo = 
    match state, charInfo with
    | (part, partSz) :: parts, (char, charSz) when partSz + charSz <= partLength -> 
        (char :: part, partSz + charSz) :: parts
    | parts, (char, charSz) -> ([ char ], charSz) :: parts

let toString (arr : char array) = new String(arr)

let split getCharInfo partSize singlePartSize (message : string) = 
    let split = 
        message
        |> Seq.map getCharInfo
        |> Seq.fold (splitter partSize) [ ([], 0) ]
        |> List.rev
    
    let totalLength = split |> List.sumBy snd
    if totalLength = singlePartSize then 
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
