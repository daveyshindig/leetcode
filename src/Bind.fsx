/// Exercise from F# for Fun and Profit, pg. 685 on bind and 
/// computation expressions.
let strToInt (str : string) : int option = 
    try
        Some (int str)
    with 
    | :? System.FormatException ->
        None

type IntBuilder() = 
    member this.Bind(m, f) =  Option.bind f m
    member this.Return(x) = Some x

let value = IntBuilder()

let stringAddWorkflow x y z =
    value
        {
        let! a = strToInt x
        let! b = strToInt y
        let! c = strToInt z
        return a + b + c
        }

let strAdd str i =
    match strToInt str with
    | Some x -> Some (x + i)
    | None -> None

let (>>=) m f = Option.bind f m

// test
let good = stringAddWorkflow "12" "3" "2"
printfn("good = %A") good
let bad = stringAddWorkflow "12" "xyz" "2"
printfn("bad = %A") bad
let good2 = strToInt "1" >>= strAdd "2" >>= strAdd "3"
let bad2 = strToInt "1" >>= strAdd "xyz" >>= strAdd "3"