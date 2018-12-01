module SMSMessageSplitterTests

open Xunit
open FsUnit.Xunit

type ``A single part GSM message when splitting``() = 
    
    [<InlineData(153)>]
    [<InlineData(154)>]
    [<InlineData(160)>]
    [<Theory>]
    member x.``Standard characters should be handled``(len) = 
        let message = String.replicate len "a"
        let splits = SMSMessageSplitter.splitGSM message
        splits |> should haveLength 1
        splits |> should equal [ String.replicate len "a" ]
    
    [<InlineData("|")>]
    [<InlineData("^")>]
    [<InlineData("€")>]
    [<InlineData("{")>]
    [<InlineData("}")>]
    [<InlineData("[")>]
    [<InlineData("~")>]
    [<InlineData("]")>]
    [<InlineData("\\")>]
    [<Theory>]
    member x.``Extended characters should be handled`` (extendedChar) = 
        let message = String.replicate 80 extendedChar
        let splits = SMSMessageSplitter.splitGSM message
        splits |> should haveLength 1
        splits |> should equal [ String.replicate 80 extendedChar ]

type ``A multipart GSM message when splitting``() = 
    
    [<Fact>]
    member x.``Standard characters should be handled``() = 
        let message = String.replicate 161 "a"
        let splits = SMSMessageSplitter.splitGSM message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 153 "a"
                                 String.replicate 8 "a" ]
    
    [<InlineData("|")>]
    [<InlineData("^")>]
    [<InlineData("€")>]
    [<InlineData("{")>]
    [<InlineData("}")>]
    [<InlineData("[")>]
    [<InlineData("~")>]
    [<InlineData("]")>]
    [<InlineData("\\")>]
    [<Theory>]
    member x.``Extended characters should be handled`` (extendedChar) = 
        let message = String.replicate 81 extendedChar
        let splits = SMSMessageSplitter.splitGSM message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 76 extendedChar
                                 String.replicate 5 extendedChar ]
    
    [<Fact>]
    member x.``The last character of a part should never be an escape character``() = 
        let message = (String.replicate 152 "a") + "€" + (String.replicate 10 "a")
        let splits = SMSMessageSplitter.splitGSM message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 152 "a"
                                 "€aaaaaaaaaa" ]

type ``A single part unicode message when splitting``() = 
    
    [<InlineData(67)>]
    [<InlineData(68)>]
    [<InlineData(70)>]
    [<Theory>]  
    member x.``Single code unit characters should be handled``(len) = 
        let message = String.replicate len "á"
        let splits = SMSMessageSplitter.splitUnicode message
        splits |> should haveLength 1
        splits |> should equal [ String.replicate len "á" ]
    
    [<Fact>]
    member x.``Double code unit characters should be handled``() = 
        let message = String.replicate 35 "\uD83D\uDC7D"
        let splits = SMSMessageSplitter.splitUnicode message
        splits |> should haveLength 1
        splits |> should equal [ String.replicate 35 "\uD83D\uDC7D" ]

type ``A multipart Unicode message when splitting``() = 
    
    [<Fact>]
    member x.``Single code unit characters should be handled``() = 
        let message = String.replicate 71 "á"
        let splits = SMSMessageSplitter.splitUnicode message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 67 "á"
                                 String.replicate 4 "á" ]
    
    [<Fact>]
    member x.``Double code unit characters should be handled``() = 
        let message = String.replicate 71 "á"
        let splits = SMSMessageSplitter.splitUnicode message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 67 "á"
                                 String.replicate 4 "á" ]
    
    [<Fact>]
    member x.``The last character of a part should not be first part of surrogate pair``() = 
        let message = (String.replicate 66 "á") + "\uD83D\uDC7D" + (String.replicate 10 "á")
        let splits = SMSMessageSplitter.splitUnicode message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 66 "á"
                                 "\uD83D\uDC7Dáááááááááá" ]

type ``A GSM message when truncating``() = 
    
    [<Fact>]
    member x.``Standard characters should be handled``() = 
        let message = String.replicate 165 "a"
        let truncated = SMSMessageSplitter.truncateGSM message
        truncated |> should haveLength 160
        truncated |> should equal (String.replicate 160 "a")
    
    [<InlineData("|")>]
    [<InlineData("^")>]
    [<InlineData("€")>]
    [<InlineData("{")>]
    [<InlineData("}")>]
    [<InlineData("[")>]
    [<InlineData("~")>]
    [<InlineData("]")>]
    [<InlineData("\\")>]
    [<Theory>]
    member x.``Extended characters should be handled`` (extendedChar) = 
        let message = String.replicate 85 extendedChar
        let truncated = SMSMessageSplitter.truncateGSM message
        truncated |> should haveLength 80
        truncated |> should equal (String.replicate 80 extendedChar)
    
    [<Fact>]
    member x.``The last character should never be an escape character``() = 
        let message = (String.replicate 159 "a") + "€aaaa"
        let truncated = SMSMessageSplitter.truncateGSM message
        truncated |> should haveLength 159
        truncated |> should equal (String.replicate 159 "a")

type ``A Unicode message when truncating``() = 
    
    [<Fact>]
    member x.``Single code unit characters should be handled``() = 
        let message = String.replicate 75 "á"
        let truncated = SMSMessageSplitter.truncateUnicode message
        truncated |> should haveLength 70
        truncated |> should equal (String.replicate 70 "á")
    
    [<Fact>]
    member x.``Double code unit characters should be handled``() = 
        let message = String.replicate 40 "\uD83D\uDC7D"
        let truncated = SMSMessageSplitter.truncateUnicode message
        truncated |> should haveLength 70
        truncated |> should equal (String.replicate 35 "\uD83D\uDC7D")
    
    [<Fact>]
    member x.``The last character should not be first part of surrogate pair``() = 
        let message = String.replicate 69 "á" + "\uD83D\uDC7D"
        let truncated = SMSMessageSplitter.truncateUnicode message
        truncated |> should haveLength 69
        truncated |> should equal (String.replicate 69 "á")
