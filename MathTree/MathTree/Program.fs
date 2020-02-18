open System

type Tree =
| Leaf of string * string
| Next of 
| Node of Tree * Tree
| Failure of string

type Parser = Parser of (string -> Tree)

let pchar charToMatch =
    let innerFunc str =
        if String.IsNullOrEmpty(str) then
            Failure "No more input"
        else
            let first = str.[0]
            if first = charToMatch then
                let remaining = str.[1..]
                Leaf (charToMatch|>string,remaining)
            else
                let msg = sprintf "'%c' don't match to '%c'" charToMatch first
                Failure msg
    Parser innerFunc
    //pchar -> (return) Parser innerFunc
    //innerFunc -> (return) Tree ~
