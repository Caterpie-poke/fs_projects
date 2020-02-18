// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
printfn "Test Start"
//-------------------------------------------
let ps str =
    printfn ""
    printfn str
let p nonstr =
    printfn "%A" nonstr
//-------------------------------------------
ps "--- hello ---"
let n = 5
let f num = num*num
p n
p (f n)
let numList1 = [1..10]
let numList2 = [1;2;3;4;5;6;7;8;9;10]
p numList1
p numList2
//-------------------------------------------
ps "--- parse initial ---"
let parserA str =
    if String.IsNullOrEmpty(str) then
        (false, "")
    else if str.[0] = 'A' then
        let remaining = str.[1..]
        (true, remaining)
    else
        (false, str)

p (parserA "ABC")
p (parserA "PPTP")
//-------------------------------------------
ps "--- if-else function ---"
let num = 12
let numCheck a =
    if a = 12 then
        true
    else
        false

p (numCheck num)
//-------------------------------------------
ps "--- void? ---"
let voidAble() = printfn "Hello"
let voidDisAble str = printfn "Hello, %A" str
voidAble()
ignore(voidAble())
voidDisAble "alice"
//-------------------------------------------
ps "--- pipeline operator ---"
let add1 num =
    num + 1
let x0 = 0//0
let x1 = add1 x0//1
let x2 = add1(add1(add1(x1)))//4
let x3 = x1 |> add1 |> add1 |> add1//4
let x4 = add1 << add1 << add1 <| x1//4
p (x0,x1,x2,x3,x4)
//-------------------------------------------
ps "--- string slice ---"
let s1 = "abcdefg"
let s2 = s1.[2..4]
p(s1,s2)//"cde"
//-------------------------------------------
ps "--- instead of for-loop ---"
let rec countUp max =
    if max > 0 then
        countUp(max-1)
        printf "%d " (max-1)

countUp 10
printfn ""
//-------------------------------------------
ps "--- Discriminated unions ---"
type Val =
| Num of int
| Add of Val * Val

let rec eval = function
| Num(num) -> num
| Add(a, b) -> eval a + eval b

let expr1 = Num(4)
let expr2 = Add(Num 2, Num 3)
printfn "%d" (eval expr1)
printfn "%d" (eval expr2)
//-------------------------------------------
ps "--- Math Test ---"
type Expr =
| Num of int
| Add of Expr * Expr
| Dec of Expr * Expr
| Mul of Expr * Expr
| Div of Expr * Expr

let rec math = function
| Num(num) -> num
| Add(a,b) -> math a + math b
| Dec(a,b) -> math a - math b
| Mul(a,b) -> math a * math b
| Div(a,b) -> math a / math b

let expr3 = Add(Mul(Num 2,Num 3),Div(Num 6,Num 2))
printfn "%A" expr3
printfn "%d" (math expr3)
//-------------------------------------------
ps "--- Pattern Match ---"
type Result<'a> =
| Success of 'a
| Failure of string

let pchar (charToMatch,str) =
    if String.IsNullOrEmpty(str) then
        Failure "No more input"
    else
        let first = str.[0] 
        if first = charToMatch then
            let remaining = str.[1..]
            Success (charToMatch,remaining)
        else
            let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
            Failure msg

let inputStr = "Apple"
let out1 = pchar('A',inputStr)
let out2 = pchar('B',"Apple")
let out3 = pchar('A',"A")
printfn "%A" (out1,out2,out3)
//-------------------------------------------
ps "--- inner func ---"
let pchar2 charToMatch =
    let innerFunc str =
        if String.IsNullOrEmpty(str) then
            Failure "No more input"
        else
            let first = str.[0]
            if first = charToMatch then
                let remaining = str.[1..]
                Success (charToMatch,remaining)
            else
                let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                Failure msg
    innerFunc

let matchA = pchar2 'A'
printfn "%A" (matchA "Apple")
//-------------------------------------------
ps "--- Making Parser Type ---"
//type Parser<'T> = Parser of (string -> Result<'T * string>)
type Parser<'T> = Parser of (string -> Result<'T>)
let pchar3 charToMatch =
    let innerFunc str =
        if String.IsNullOrEmpty(str) then
            Failure "No more input"
        else
            let first = str.[0]
            if first = charToMatch then
                let remaining = str.[1..]
                Success (charToMatch,remaining)
            else
                let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                Failure msg
    Parser innerFunc

let run parser input = 
    let (Parser innerFn) = parser 
    innerFn input

let inputSrc = "BBC"
let matchB = pchar3 'B'
printfn "%A" (run matchB inputSrc)
//-------------------------------------------
ps "--- AndThen ---"
let andThen parser1 parser2 =
    let innerFunc input =
        let result1 = run parser1 input
        
        match result1 with
        | Failure err ->
            Failure err  

        | Success (value1,remaining1) ->
            let result2 = run parser2 remaining1
            
            match result2 with 
            | Failure err ->
                Failure err
            
            | Success (value2,remaining2) ->
                let newValue = (value1,value2)
                Success (newValue,remaining2)

    Parser innerFunc

let ( .>>. ) = andThen

let parseA = pchar3 'A'
let parseB = pchar3 'B'
let parseAandB = parseA .>>. parseB
let parseBandA = parseB .>>. parseA
printfn "%A" (run parseAandB "ABCDE")
printfn "%A" (run parseAandB "AACDE")
printfn "%A" (run parseBandA "ABCDE")
printfn "%A" (run parseBandA "BACDE")
//-------------------------------------------
ps "--- OrElse ---"
let orElse parser1 parser2 =
    let innerFunc input =
        let result1 = run parser1 input

        match result1 with
        | Success result ->
            result1
        | Failure err ->
            let result2 = run parser2 input
            result2
    
    Parser innerFunc

let ( <|> ) = orElse

let parseAorB = parseA <|> parseB
let parseBorA = parseB <|> parseA
printfn "%A" (run parseAorB "ACDE")
printfn "%A" (run parseAorB "BCDE")
printfn "%A" (run parseAorB "XCDE")
printfn "%A" (run parseBorA "ACDE")
//-------------------------------------------
ps "--- Choice ---"
let choice listOfParsers =
    List.reduce ( <|> ) listOfParsers

let sampleListOfParser =
    [pchar3 'A';pchar3 'B';pchar3 'C']
let sampleChoice list =
    list
    |> choice
    //It means choice(list)

let aaa = sampleChoice sampleListOfParser
printfn "%A" (run aaa "AXX")
printfn "%A" (run aaa "BXX")
printfn "%A" (run aaa "CXX")
printfn "%A" (run aaa "XXX")
//-------------------------------------------
ps "--- AnyOf ---"
//ver.All
let all1 = List.reduce ( <|> ) << List.map pchar3 <| ['a'..'z']
let all2 = ['a'..'z'] |> List.map pchar3 |> List.reduce ( <|> )

//ver.Func
let anyOf1 listOfChar =
    listOfChar
    |> List.map pchar3
    |> choice

let anyOf2 listOfChar =
    choice
    << List.map pchar3
    <| listOfChar

let digit1 = anyOf1 ['0'..'9']
let digit2 = anyOf2 ['0'..'9']

printfn "%A" (run all1 "abc")
printfn "%A" (run all2 "ABC")
printfn "%A" (run digit1 "19")
printfn "%A" (run digit2 "D4C")
//-------------------------------------------
ps "--- Parser of String ---"
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

let matchHello = pstr "Hello"
printfn "%A" (run matchHello "Hello, world")
//-------------------------------------------
ps "--- * implement Prototype ---"
let repetition parser =
    let rec innerFunc str =
        let result = run parser str
        match result with
        | Success (target,remaining) ->
            innerFunc remaining
        | Failure err ->
            Failure err
    Parser innerFunc

let match_a = pchar3 'a'
let match_a_rpt<'a> = repetition match_a
printfn "%A" (run match_a "aaaab")
printfn "%A" (run match_a_rpt "aaaab")
//-------------------------------------------
ps "--- * implementation complete ---"
let pchar_rpt charToMatch =
    let rec innerFunc str =
        if String.IsNullOrEmpty(str) then
            Failure "No more input"
        else
            let first = str.[0]
            if first = charToMatch then
                let remaining = str.[1..]
                innerFunc remaining
            else
                Success (charToMatch, str.[0..])
    Parser innerFunc

let rpt_a = pchar_rpt 'a'
printfn "%A" (run rpt_a "aaaaaaaaabst")
printfn "%A" (run rpt_a "Precure")
//-------------------------------------------
ps "--- + implementation ---"
let pchar_rpt_1 charToMatch =
    let innerFunc str =
        if String.IsNullOrEmpty(str) then
            Failure "No more input"
        else
            let first1 = str.[0]
            if first1 = charToMatch then
                let remaining = str.[1..]
                let rec innerFunc2 str2 count =
                    let first2 = (string str2).[0]
                    if first2 = charToMatch then
                        let remaining = (string str2).[1..]
                        innerFunc2 remaining (count+1)
                    else
                        Success (charToMatch, count, str2)
                innerFunc2 remaining 1
            else
                let msg = "Not found"
                Failure msg
    Parser innerFunc

let a_rpt_1 = pchar_rpt_1 'a'
printfn "%A" (run a_rpt_1 "aaaaabc")
printfn "%A" (run a_rpt_1 "aabac")
printfn "%A" (run a_rpt_1 "bmbb")
//-------------------------------------------
ps "--- Digit* ---"
let digit = ['0'..'9']
let anyOf_rpt listOfChar =
    listOfChar
    |> List.map pchar_rpt
    |> choice
   
let digit_rpt = anyOf_rpt digit
printfn "%A" (run digit_rpt "123+456")
printfn "%A" (run digit_rpt "hello")
//-------------------------------------------
ps "--- Digit+ ---"
let anyOf_rpt_1 listOfChar =
    listOfChar
    |> List.map pchar_rpt_1
    |> choice

let digit_rpt_1 = anyOf_rpt_1 digit
printfn "%A" (run digit_rpt_1 "123+456")
printfn "%A" (run digit_rpt_1 "hello")
//-------------------------------------------
ps "--- * refactor ---"
//-------------------------------------------
//type Result<'a> =
//| Success of 'a
//| Failure of string

//type Parser<'T> = Parser of (string -> Result<'T>)

//let pchar3 charToMatch =
    //let innerFunc str =
    //    if String.IsNullOrEmpty(str) then
    //        Failure "No more input"
    //    else
    //        let first = str.[0]
    //        if first = charToMatch then
    //            let remaining = str.[1..]
    //            Success (charToMatch,remaining)
    //        else
    //            let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
    //            Failure msg
    //Parser innerFunc
//-------------------------------------------
let repeat parser =
    let rec innerFunc input =
        let result = run parser input
        match result with
        | Success (target, remaining) ->
            printfn "%A" result
            innerFunc remaining
        | Failure err ->
            Failure err
    Parser innerFunc
   
let mta<'a> = repeat match_a
printfn "%A" (run mta "aaaaaaabc")
//-------------------------------------------
ps "--- StringBuilder ---"
let sb = new Text.StringBuilder()
sb.Append("Hello") |> ignore
sb.Append(",") |> ignore
sb.Append("world") |> ignore
printfn "%s" (sb.ToString())
//-------------------------------------------
ps "--- Record (instead of struct) ---"
type student = {name:string ; age:int ; sex:string}
let alice = {name="Alice" ; age=15 ; sex="woman"}
let yoko = {alice with name = "Yoko"}
printfn "%A" alice
printfn "%A" yoko

type mathTree = {num:int ; left:mathTree ; right:mathTree}
//-------------------------------------------
ps "--- return Tree-like String by StringBuilder ---"
let writeNum sb =
    let numStr = sprintf "%d" 12
    (sb:Text.StringBuilder).Append(num)
let writeInner sb =
    (sb:Text.StringBuilder).Append("[")|>ignore
    writeNum sb|>ignore
    (sb:Text.StringBuilder).Append("][+][")|>ignore
    writeNum sb|>ignore
    (sb:Text.StringBuilder).Append("]")|>ignore

let treeSB = new Text.StringBuilder()
treeSB.Append("[")|>ignore
writeInner treeSB|>ignore
treeSB.Append("]")|>ignore
let resultStr = treeSB.ToString()
printfn "%s" resultStr
//-------------------------------------------
ps "--- Tree test1 ---"
type Tree =
| Node of Tree
| Leaf of string

let rec printTree tree =
    match tree with
    | Node node ->
        printTree node
    | Leaf leaf ->
        printf "%s" leaf

let testTree = Node(Node(Node(Leaf("Hello"))))
printTree testTree
//-------------------------------------------
ps "--- Tree test2 ---"
type Tree2 =
| Node2 of Tree2 * Tree2
| Leaf2 of string

let rec printTree2 tree =
    match tree with
    | Node2 (left,right) ->
        printTree2 left
        printTree2 right
    | Leaf2 leaf ->
        printf "%s" leaf

let testTree2 = Node2(Node2(Leaf2("Hello"),Leaf2("")),Node2(Node2(Leaf2(","),Leaf2("")),Node2(Leaf2("world"),Leaf2("\n"))))
printTree2 testTree2

let leaf src =
    let text = sprintf "%s" src
    Leaf2 text
    //return Leaf

let a = 'a'
let b = 'b'
let ab = a + b
printfn "%c" ab
//-------------------------------------------
