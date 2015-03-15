module SMSMessageSplitterTests

open NUnit.Framework
open FsUnit

[<TestFixture>]
type ``A single part GSM message when splitting``() = 
    
    [<TestCase(153)>]
    [<TestCase(154)>]
    [<TestCase(160)>]
    member x.``Standard characters should be handled``(len) = 
        let message = String.replicate len "a"
        let splits = SMSMessageSplitter.splitGSM message
        splits |> should haveLength 1
        splits |> should equal [ String.replicate len "a" ]
    
    [<TestCase("|")>]
    [<TestCase("^")>]
    [<TestCase("€")>]
    [<TestCase("{")>]
    [<TestCase("}")>]
    [<TestCase("[")>]
    [<TestCase("~")>]
    [<TestCase("]")>]
    [<TestCase("\\")>]
    member x.``Extended characters should be handled`` (extendedChar) = 
        let message = String.replicate 80 extendedChar
        let splits = SMSMessageSplitter.splitGSM message
        splits |> should haveLength 1
        splits |> should equal [ String.replicate 80 extendedChar ]

[<TestFixture>]
type ``A multipart GSM message when splitting``() = 
    
    [<Test>]
    member x.``Standard characters should be handled``() = 
        let message = String.replicate 161 "a"
        let splits = SMSMessageSplitter.splitGSM message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 153 "a"
                                 String.replicate 8 "a" ]
    
    [<TestCase("|")>]
    [<TestCase("^")>]
    [<TestCase("€")>]
    [<TestCase("{")>]
    [<TestCase("}")>]
    [<TestCase("[")>]
    [<TestCase("~")>]
    [<TestCase("]")>]
    [<TestCase("\\")>]
    member x.``Extended characters should be handled`` (extendedChar) = 
        let message = String.replicate 81 extendedChar
        let splits = SMSMessageSplitter.splitGSM message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 76 extendedChar
                                 String.replicate 5 extendedChar ]
    
    [<Test>]
    member x.``The last character of a part should never be an escape character``() = 
        let message = (String.replicate 152 "a") + "€" + (String.replicate 10 "a")
        let splits = SMSMessageSplitter.splitGSM message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 152 "a"
                                 "€aaaaaaaaaa" ]

[<TestFixture>]
type ``A single part unicode message when splitting``() = 
    
    [<TestCase(67)>]
    [<TestCase(68)>]
    [<TestCase(70)>]    
    member x.``Single code unit characters should be handled``(len) = 
        let message = String.replicate len "á"
        let splits = SMSMessageSplitter.splitUnicode message
        splits |> should haveLength 1
        splits |> should equal [ String.replicate len "á" ]
    
    [<Test>]
    member x.``Double code unit characters should be handled``() = 
        let message = String.replicate 35 "\uD83D\uDC7D"
        let splits = SMSMessageSplitter.splitUnicode message
        splits |> should haveLength 1
        splits |> should equal [ String.replicate 35 "\uD83D\uDC7D" ]

[<TestFixture>]
type ``A multipart Unicode message when splitting``() = 
    
    [<Test>]
    member x.``Single code unit characters should be handled``() = 
        let message = String.replicate 71 "á"
        let splits = SMSMessageSplitter.splitUnicode message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 67 "á"
                                 String.replicate 4 "á" ]
    
    [<Test>]
    member x.``Double code unit characters should be handled``() = 
        let message = String.replicate 71 "á"
        let splits = SMSMessageSplitter.splitUnicode message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 67 "á"
                                 String.replicate 4 "á" ]
    
    [<Test>]
    member x.``The last character of a part should not be first part of surrogate pair``() = 
        let message = (String.replicate 66 "á") + "\uD83D\uDC7D" + (String.replicate 10 "á")
        let splits = SMSMessageSplitter.splitUnicode message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 66 "á"
                                 "\uD83D\uDC7Dáááááááááá" ]

[<TestFixture>]
type ``A GSM message when truncating``() = 
    
    [<Test>]
    member x.``Standard characters should be handled``() = 
        let message = String.replicate 165 "a"
        let truncated = SMSMessageSplitter.truncateGSM message
        truncated |> should haveLength 160
        truncated |> should equal (String.replicate 160 "a")
    
    [<TestCase("|")>]
    [<TestCase("^")>]
    [<TestCase("€")>]
    [<TestCase("{")>]
    [<TestCase("}")>]
    [<TestCase("[")>]
    [<TestCase("~")>]
    [<TestCase("]")>]
    [<TestCase("\\")>]
    member x.``Extended characters should be handled`` (extendedChar) = 
        let message = String.replicate 85 extendedChar
        let truncated = SMSMessageSplitter.truncateGSM message
        truncated |> should haveLength 80
        truncated |> should equal (String.replicate 80 extendedChar)
    
    [<Test>]
    member x.``The last character should never be an escape character``() = 
        let message = (String.replicate 159 "a") + "€aaaa"
        let truncated = SMSMessageSplitter.truncateGSM message
        truncated |> should haveLength 159
        truncated |> should equal (String.replicate 159 "a")

[<TestFixture>]
type ``A Unicode message when truncating``() = 
    
    [<Test>]
    member x.``Single code unit characters should be handled``() = 
        let message = String.replicate 75 "á"
        let truncated = SMSMessageSplitter.truncateUnicode message
        truncated |> should haveLength 70
        truncated |> should equal (String.replicate 70 "á")
    
    [<Test>]
    member x.``Double code unit characters should be handled``() = 
        let message = String.replicate 40 "\uD83D\uDC7D"
        let truncated = SMSMessageSplitter.truncateUnicode message
        truncated |> should haveLength 70
        truncated |> should equal (String.replicate 35 "\uD83D\uDC7D")
    
    [<Test>]
    member x.``The last character should not be first part of surrogate pair``() = 
        let message = String.replicate 69 "á" + "\uD83D\uDC7D"
        let truncated = SMSMessageSplitter.truncateUnicode message
        truncated |> should haveLength 69
        truncated |> should equal (String.replicate 69 "á")
