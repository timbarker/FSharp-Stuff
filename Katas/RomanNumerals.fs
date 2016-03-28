open NUnit.Framework
open FsUnit

let toRomanNumeral number = 
    let mapping = [(1, "I");(4, "IV");(5, "V");(9, "IX");(10, "X");(40, "XL");(50, "L");(90, "XC");(100, "C");(400, "CD");(500, "D");(900, "CM");(1000, "M")]

    let rec calc x acc = 
        if x = 0 then
            acc
        else 
            let foundKey, foundNumeral = List.foldBack (fun (k, v) (sk, sv) -> if sk = 0 && k <= x then (k, v) else (sk, sv)) mapping (0, "")
            calc (x - foundKey) (acc + foundNumeral)

    calc number ""
    
[<TestFixture>]
type Tests() =

    [<TestCase(1, "I")>]
    [<TestCase(2, "II")>]
    [<TestCase(3, "III")>]
    [<TestCase(4, "IV")>]
    [<TestCase(5, "V")>]
    [<TestCase(2008, "MMVIII")>]
    [<TestCase(99, "XCIX")>]
    member __.GivenADecimalThenTheExpectedRomanNumeralIsReturned(number, expected) = toRomanNumeral number |> should equal expected

[<EntryPoint>]
let main argv = 
    for i in 1..1000 do
        printfn "%d = %s" i (toRomanNumeral i)
    0