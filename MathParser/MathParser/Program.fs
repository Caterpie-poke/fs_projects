open System

//-------------------- pchar --------------------
type Result<'a> =
| Success of 'a
| Failure of string

type Parser<'T> = Parser of (string -> Result<'T>)

let pchar charToMatch =
    let innerFunc str =
        if String.IsNullOrEmpty(str) then
            Failure "No more input"
        else
            let first = str.[0]
            if first = charToMatch then
                let remaining = str.[1..]
                //printfn "%A append" charToMatch
                Success (charToMatch|>string,remaining)
            else
                let msg = sprintf "'%c' don't match to '%c'" charToMatch first
                Failure msg
    Parser innerFunc
//let match_a = pchar 'a'
//-------------------- pstr --------------------
let pstr strToMatch =
    let innerFunc str =
        if String.IsNullOrEmpty(str) then
            Failure "No more input"
        else
            let tokenCharlist = [for i in 0..(String.length(strToMatch) - 1) -> str.[i]]
            let tokenString = String.Concat(tokenCharlist)
            if tokenString = strToMatch then
                let remaining = str.[(String.length(strToMatch))..]
                Success(strToMatch,remaining)
            else
                let msg = sprintf "'%s' is not found" strToMatch
                Failure msg
    Parser innerFunc
//implement by handwrite or and(.>>.)
//-------------------- run --------------------
let run parser input =
    let (Parser innerFn) = parser
    innerFn input
//run match_a "apple" |> ignore => Success('a',"pple")
//-------------------- pChar to str --------------------
let charToStr parser =
    let innerFunc input =
        let result = run parser input
        match result with
        | Success (char,remaining) ->
            Success (char|>string,remaining)
        | Failure err ->
            Failure err
    Parser innerFunc
//-------------------- andThen --------------------
let andThen parser1 parser2 =
    let innerFunc input =
        let result1 = run parser1 input//append
        match result1 with
        | Failure err ->
            Failure err  
        | Success (value1,remaining1) ->
            let result2 = run parser2 remaining1//append
            match result2 with 
            | Failure err ->
                Failure err
            | Success (value2,remaining2) ->
                Success ((value1|>string) + (value2|>string),remaining2)
                //Success (value1,Success(value2,remaining2))
    Parser innerFunc
let (.>>.) = andThen
//-------------------- orElse --------------------
let orElse parser1 parser2 =
    let innerFunc input =
        let result1 = run parser1 input//append
        match result1 with
        | Success result ->
            result1
        | Failure err ->
            let result2 = run parser2 input//append
            result2
    Parser innerFunc
let (<|>) = orElse
//-------------------- choice --------------------
let choice listOfParsers =
    List.reduce ( <|> ) listOfParsers
//-------------------- anyOf --------------------
let anyOf listOfChar =
    listOfChar
    |> List.map pchar
    |> choice
//let digit = anyOf ['0'..'9']
//let alpha = anyOf ['a'..'z']
//-------------------- repetition --------------------
let rec rpt parser =
    let innerFunc input =
        let result = run parser input
        match result with
        | Success result ->
            let newParser = parser .>>. (parser |> rpt)
            run newParser input
        | Failure err ->
            Success ("",input)
    Parser innerFunc
//let parse_a = pchar 'a'
//let a_rpt = rpt parse_a
//run a_rpt "aaaaa-a5"|>ignore
//run a_rpt "bbbb-b4"|>ignore
//run a_rpt "abcde-a1"|>ignore
//-------------------- blocked --------------------
let blocked parser =
    let innerFunc input =
        let result = run parser input
        match result with
        | Success (str,remaining) ->
            Success ("[" + str + "]",remaining)
        | Failure msg ->
            Failure msg
    Parser innerFunc
//-------------------- calculator --------------------
let calc parser =
    let innerFunc input =
        let result = run parser input
        match result with
        | Success (str,remaining) ->
            let answer = 45
            Success answer
        | Failure msg ->
            Failure msg
    Parser innerFunc

let toInt parser =
    let innerFunc input =
        let result = run parser input
        match result with
        | Success (str,remaining) ->
            Success (str|>int,remaining)
        | Failure msg ->
            Failure msg
    Parser innerFunc
//-------------------- any --------------------
let pSpaceSkip =
    let innerFunc input =
        let space = pchar(' ')
        let result = run space input
        match result with
        | Success (hoge,remaining) ->
            Success ("",remaining)
        | Failure err ->
            Failure err
    Parser innerFunc |> rpt

let skip = pSpaceSkip
//-------------------- pMulDiv --------------------
let pMul = pchar '*'
let pDiv = pchar '/'
//let pMulDiv = pMul <|> pDiv |> blocked

let rec pMulDiv2 parser =
    let innerFunc input =
        let pMulDiv = pMul <|> pDiv
        let result1 = run parser input
        match result1 with
        | Success (num1,remaining1) ->
            let resultOp = run pMulDiv remaining1
            match resultOp with
            | Success (op,remainingOp) ->
                let nextParser = pMulDiv2 parser
                let result2 = run nextParser remainingOp
                match op with
                | "*" ->
                    match result2 with
                    | Success (num2,remaining2) ->
                        Success (num1 * num2, remaining2)
                    | Failure msg ->
                        Failure msg
                | "/" ->
                    match result2 with
                    | Success (num2,remaining2) ->
                        Success (num1 / num2, remaining2)
                    | Failure msg ->
                        Failure msg
                | _ ->
                    Failure "operator(* or /) miss match"
            | Failure msg ->
                Success (num1,remaining1)
        | Failure msg ->
            result1
    Parser innerFunc

let mdOp = pMulDiv2
//-------------------- pAddSub --------------------
let pAdd = pchar '+'
let pSub = pchar '-'
//let pAddSub = pAdd <|> pSub |> blocked

let rec pAddSub2 parser =
    let innerFunc input =
        let pAddSub = pAdd <|> pSub
        let result1 = run parser input
        match result1 with
        | Success (num1,remaining1) ->
            let resultOp = run pAddSub remaining1
            match resultOp with
            | Success (op,remainingOp) ->
                let nextParser = pAddSub2 parser
                let result2 = run nextParser remainingOp
                match op with
                | "+" ->
                    match result2 with
                    | Success (num2,remaining2) ->
                        Success (num1 + num2, remaining2)
                    | Failure msg ->
                        Failure msg
                | "-" ->
                    match result2 with
                    | Success (num2,remaining2) ->
                        Success (num1 - num2, remaining2)
                    | Failure msg ->
                        Failure msg
                | _ ->
                    Failure "operator(+ or -) miss match"
            | Failure msg ->
                Success (num1,remaining1)
        | Failure msg ->
            result1
    Parser innerFunc

let amOp = pAddSub2
//-------------------- pDigit --------------------
//let pDigit = skip .>>. anyOf['1'..'9'] .>>. (anyOf['0'..'9']|>rpt) .>>. skip |> blocked
let pDigit = skip .>>. anyOf['1'..'9'] .>>. (anyOf['0'..'9']|>rpt) .>>. skip |> toInt
//-------------------- pProduct --------------------
//let pProduct = pDigit .>>. ((pMulDiv .>>. pDigit) |> rpt) |> blocked
let pProduct = pDigit |> mdOp
//-------------------- pInput --------------------
//let pInput = pProduct .>>. ((pAddSub .>>. pProduct)|> rpt)
let pInput = pProduct |> amOp
//-------------------- pMain --------------------
let expr1 = "1+2*3"
printfn "%s" expr1
printfn "%A" (run pInput expr1)
//return Success ("[[1]][+][[2][*][3]]", "")

let expr2 = "24/12-36"
printfn "%s" expr2
printfn "%A" (run pInput expr2)
//return Success ("[[24][/][12]][-][[36]]", "")

let expr3 = "810 + 114514"
printfn "%s" expr3
printfn "%A" (run pInput expr3)
//return Success ("[[810]][+][[114514]]", "")

let expr4 = "   12 + 5*6 +  20 -   64  / 4    "
printfn "%s" expr4
printfn "%A" (run pInput expr4)
//return Success ("[[12][*][3]][+][[64][/][4]]", "")

let expr5 = "2*3+6-8/4"
printfn "%s" expr5
printfn "%A" (run pInput expr5)
//return Success ("[[2][*][3]][+][[6]][-][[8][/][4]]", "")
