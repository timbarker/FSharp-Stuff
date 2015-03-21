module MonadTests

open NUnit.Framework
open FsUnit
open Result

type ``Monad Laws``() = 
    let (>>=) m f = bind f m
    let return' a = Success a
    let m = Success ""
    let g x = Failure x
    let f x = Success x
    let x = ""
    
    [<Test>]
    member this.``(m >>= f) >>= g ≡ m >>= ( \x -> (f x >>= g) )``() = 
        (m >>= f) >>= g |> should equal (m >>= (fun x -> (f x >>= g)))
    
    [<Test>]
    member this.``(return x) >>= f ≡ f x``() = (return' x) >>= f |> should equal (f x)
    
    [<Test>]
    member this.``m >>= return ≡ m``() = m >>= return' |> should equal m
