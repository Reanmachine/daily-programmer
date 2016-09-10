open System
open System.Text.RegularExpressions

///////////////////////////////////////////////////////
// Result & Error Handling Framework

type Result<'Success, 'Failure> = 
    | Success of 'Success
    | Failure of 'Failure

let bind successFunction =
    function
    | Success value -> successFunction value
    | Failure failure -> Failure failure

let tuplize x y =
    match (x, y) with
    | (Success a, Success b) -> Success (a,b)
    | (Failure a, Success b) -> Failure a
    | (Success a, Failure b) -> Failure b
    // Ther's a missing case here that doesn't make sense to me :/ 

////////////////////////////////////////////////////////
// Domain Concepts

type DecodeErrors =
    | InvalidHandPattern of string
    | IncorrectLength
    | IncorrectFormat

type InputErrors =
    | NoArguments
    | TooManyArguments

type Hand = 
    | Left of string
    | Right of string

////////////////////////////////////////////////////////
// (Hand)ling - lol

let decodeLeftHand hand =
    match hand with
    | "00000" -> Success 0
    | "00010" -> Success 10
    | "00110" -> Success 20
    | "01110" -> Success 30
    | "11110" -> Success 40
    | "00001" -> Success 50
    | "00011" -> Success 60
    | "00111" -> Success 70
    | "01111" -> Success 80
    | "11111" -> Success 90
    | _ -> Failure (InvalidHandPattern "Left")

let decodeRightHand hand = 
    match hand with
    | "00000" -> Success 0
    | "01000" -> Success 1
    | "01100" -> Success 2
    | "01110" -> Success 3
    | "01111" -> Success 4
    | "10000" -> Success 5
    | "11000" -> Success 6
    | "11100" -> Success 7
    | "11110" -> Success 8
    | "11111" -> Success 9
    | _ -> Failure (InvalidHandPattern "Right")

let decodeHand input = 
    match input with
    | Left hand -> decodeLeftHand hand
    | Right hand -> decodeRightHand hand

//////////////////////////////////////////////////////
// Validation

let isCorrectLength (input:string) =
    if input.Length = 10 
        then Success input 
        else Failure IncorrectLength

let isOnlyZeroesAndOnes (input:string) =
    if Regex.IsMatch(input, "^[01]+$") 
        then Success input 
        else Failure IncorrectFormat

let validate input =
    Success input 
    |> bind isCorrectLength
    |> bind isOnlyZeroesAndOnes

///////////////////////////////////////////////////////
// Decode

let parse (input:string) =
    let left  = (Left  (input.Substring(0, 5))) |> decodeHand
    let right = (Right (input.Substring(5, 5))) |> decodeHand
    tuplize left right

let combine input =
    match input with
    | (left : int, right : int) -> Success (left + right) 

let decodeResult input output =
    Success (input, output)

let decode input =
    Success input
    |> bind validate
    |> bind parse
    |> bind combine
    |> bind (decodeResult input)

///////////////////////////////////////////////////////
// CLI

let getInput argv =
    match argv with
    | [x] -> Success x
    | []  -> Failure NoArguments
    | _   -> Failure TooManyArguments

let banner =
    printfn "/r/dailyprogrammer - F# - 280 - Easy"
    printfn "===================================="
    printfn ""

let usage msg =
    printfn "USAGE: 280-easy [input]"
    printfn "[!]\t%s" msg

[<EntryPoint>]
let main argv = 
    banner

    match argv |> Seq.toList |> getInput with
    | Success input -> 
        match decode input with
        | Success (input, output) -> 
            printfn "%s - %d" input output
            0
        | Failure IncorrectFormat ->
            printfn "%s - Invalid" input
            printfn "Reason: The input value was in an incorrect format or had invalid characters."
            -1
        | Failure IncorrectLength ->
            printfn "%s - Invalid" input
            printfn "Reason: The input value was not 10 characters long."
            -1
        | Failure (InvalidHandPattern hand) ->
            printfn "%s - Invalid" input
            printfn "Reason: The %s hand pattern was invalid." hand
            -1
    | Failure f -> 
        match f with
        | NoArguments       -> 
            usage "input must be specified"
            -1
        | TooManyArguments  -> 
            usage "too many arguments"
            -1
