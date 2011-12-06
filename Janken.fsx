module Janken =

    type JankenHand =
        private
        | Rock
        | Paper
        | Scissors

    type State =
        | Draw
        | Right
        | Left

    let Janken l r =
        match l, r with
        | Rock, Rock      -> Draw
        | Rock, Paper     -> Right
        | Rock, Scissors  -> Left
        | Paper, Rock     -> Left
        | Paper, Paper    -> Draw
        | Paper, Scissors -> Right
        | Scissors, Rock  -> Right
        | Scissors, Paper -> Left
        | Scissors, Scissors -> Draw

    let (|JankenHand|_|) j =
        match (j:string).ToLower() with
        | x when x.StartsWith("r") -> Some Rock
        | x when x.StartsWith("p") -> Some Paper
        | x when x.StartsWith("s") -> Some Scissors
        | _ -> None

module JankenMain =

    open Janken

    let main = function
        | [|_; JankenHand l; JankenHand r|] ->
            match Janken l r with
            | Draw -> "draw"
            | Left -> "Left wins"
            | Right -> "Right wins"
            |> printfn "%s"
            0
        | _ ->
            printfn "fsi Janken.fsx {R,P,S} {R,P,S}"
            1

JankenMain.main fsi.CommandLineArgs