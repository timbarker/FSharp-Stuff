module SMSMessageSplitterTests

open NUnit.Framework
open FsUnit

[<TestFixture>]
type ``A GSM Single part splitter``() = 
    
    [<TestCase(154)>]
    [<TestCase(160)>]
    member x.``Should handle standard characters``(len) = 
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
    member x.``Should handle extended characters`` (extendedChar) = 
        let message = String.replicate 80 extendedChar
        let splits = SMSMessageSplitter.splitGSM message
        splits |> should haveLength 1
        splits |> should equal [ String.replicate 80 extendedChar ]

[<TestFixture>]
type ``A GSM multipart splitter``() = 
    
    [<Test>]
    member x.``Should handle standard characters``() = 
        let message = String.replicate 154 "a"
        let splits = SMSMessageSplitter.splitGSM message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 153 "a"
                                 "a" ]
    
    [<TestCase("|")>]
    [<TestCase("^")>]
    [<TestCase("€")>]
    [<TestCase("{")>]
    [<TestCase("}")>]
    [<TestCase("[")>]
    [<TestCase("~")>]
    [<TestCase("]")>]
    [<TestCase("\\")>]
    member x.``Should handle extended characters`` (extendedChar) = 
        let message = String.replicate 77 extendedChar
        let splits = SMSMessageSplitter.splitGSM message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 76 extendedChar
                                 extendedChar ]
    
    [<Test>]
    member x.``Should not let last character be escape character``() = 
        let message = (String.replicate 152 "a") + "€" + (String.replicate 10 "a")
        let splits = SMSMessageSplitter.splitGSM message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 152 "a"
                                 "€aaaaaaaaaa" ]

[<TestFixture>]
type ``A Unicode single part splitter``() = 
    
    [<Test>]
    member x.``Should handle single code unit characters``() = 
        let message = String.replicate 70 "á"
        let splits = SMSMessageSplitter.splitUnicode message
        splits |> should haveLength 1
        splits |> should equal [ String.replicate 70 "á" ]
    
    [<Test>]
    member x.``Should handle double code unit characters``() = 
        let message = String.replicate 35 "\uD83D\uDC7D"
        let splits = SMSMessageSplitter.splitUnicode message
        splits |> should haveLength 1
        splits |> should equal [ String.replicate 35 "\uD83D\uDC7D" ]

[<TestFixture>]
type ``A Unicode multi part splitter``() = 
    
    [<Test>]
    member x.``Should handle single code unit characters``() = 
        let message = String.replicate 71 "á"
        let splits = SMSMessageSplitter.splitUnicode message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 67 "á"
                                 String.replicate 4 "á" ]
    
    [<Test>]
    member x.``Should handle double code unit characters``() = 
        let message = String.replicate 71 "á"
        let splits = SMSMessageSplitter.splitUnicode message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 67 "á"
                                 String.replicate 4 "á" ]
    
    [<Test>]
    member x.``Should not split surrogate pair at end of part``() = 
        let message = (String.replicate 66 "á") + "\uD83D\uDC7D" + (String.replicate 10 "á")
        let splits = SMSMessageSplitter.splitUnicode message
        splits |> should haveLength 2
        splits |> should equal [ String.replicate 66 "á"
                                 "\uD83D\uDC7Dáááááááááá" ]

[<TestFixture>]
type ``A GSM truncator``() = 
    
    [<Test>]
    member x.``Should truncate standard chracters correct length``() = 
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
    member x.``Should truncate extended chracters to correct length`` (extendedChar) = 
        let message = String.replicate 85 extendedChar
        let truncated = SMSMessageSplitter.truncateGSM message
        truncated |> should haveLength 80
        truncated |> should equal (String.replicate 80 extendedChar)
    
    [<Test>]
    member x.``Should not let escape character be at end of string``() = 
        let message = (String.replicate 159 "a") + "€aaaa"
        let truncated = SMSMessageSplitter.truncateGSM message
        truncated |> should haveLength 159
        truncated |> should equal (String.replicate 159 "a")

[<TestFixture>]
type ``A Unicode truncator``() = 
    
    [<Test>]
    member x.``Should truncate single code unit chararcters to correct length``() = 
        let message = String.replicate 75 "á"
        let truncated = SMSMessageSplitter.truncateUnicode message
        truncated |> should haveLength 70
        truncated |> should equal (String.replicate 70 "á")
    
    [<Test>]
    member x.``Should truncate double code unit chararcters to correct length``() = 
        let message = String.replicate 40 "\uD83D\uDC7D"
        let truncated = SMSMessageSplitter.truncateUnicode message
        truncated |> should haveLength 70
        truncated |> should equal (String.replicate 35 "\uD83D\uDC7D")
    
    [<Test>]
    member x.``Should not split surrogate pair at end of string``() = 
        let message = String.replicate 69 "á" + "\uD83D\uDC7D"
        let truncated = SMSMessageSplitter.truncateUnicode message
        truncated |> should haveLength 69
        truncated |> should equal (String.replicate 69 "á")
