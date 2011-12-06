type JankenHand =
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

let parse s =
    match (s:string).ToLower() with
    | x when x.StartsWith("r") -> Rock
    | x when x.StartsWith("p") -> Paper
    | x when x.StartsWith("s") -> Scissors
    | _ -> failwith "argument is not in (r,p,s)"

let main = function
    | [|_; l; r |] ->
        match Janken (parse l) (parse r) with
        | Draw -> "draw"
        | Left -> "Left wins"
        | Right -> "Right wins"
        |> printfn "%s"
        0
    | _ ->
        printfn "fsi Janken.fsx {R,P,S} {R,P,S}"
        1

main fsi.CommandLineArgs