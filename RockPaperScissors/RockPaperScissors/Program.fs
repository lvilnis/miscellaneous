// Learn more about F# at http://fsharp.net

type RPS = Rock | Paper | Scissors

type Strategy = Strategy1 | Strategy2 | Strategy3 | Random
    
let counter = function
    | Rock -> Paper
    | Paper -> Scissors
    | Scissors -> Rock

let getRandomMove() = 
    let random = (new System.Random()).Next() % 3
    match random with
    | 0 -> Rock
    | 1 -> Paper
    | 2 -> Scissors
    | _ -> failwith "not possible"

let printMove = function
    | Rock -> "Rock"
    | Paper -> "Paper"
    | Scissors -> "Scissors"

let counteri num = (List.map (fun _ -> counter)  [1..num]) |> List.reduce (>>) 

let getNextMove strategy lastMove = 
    match strategy with
    | Random -> getRandomMove()
    | Strategy1 -> counteri 1 lastMove
    | Strategy2 -> counteri 3 lastMove
    | Strategy3 -> counteri 5 lastMove
    
let equalCount opponentMoves yourMoves = 
    List.zip opponentMoves yourMoves |> List.filter (fun (op, yr) -> op = yr) |> List.length

let lossCount opponentsMoves strat =
    equalCount (opponentsMoves |> List.rev |> List.tail |> List.rev |> List.map (getNextMove strat) |> List.map counter) (List.tail opponentsMoves)

let foo = [Rock;Scissors;Paper;Paper;Scissors;Rock;Scissors]

let getBestStrat opponentsMoves =
    [ Strategy1; Strategy2; Strategy3; Random ] |> List.map (fun num -> (num, lossCount opponentsMoves num)) |> List.minBy snd |> fst

let getMoveFromInput = function
    | 'r' | 'R' -> Rock
    | 'p' | 'P' -> Paper
    | 's' | 'S' -> Scissors
    | _ -> failwith "Not a valid choice!"

let take10 is = if Seq.length is < 10 then is else  (Seq.take 10 is) |> Seq.toList

let askForMove() =
    System.Console.ReadKey().KeyChar

let rec getMoveSafe() = 
    try getMoveFromInput (askForMove()) with _ -> getMoveSafe()

let rec go opponentsPreviousMoves opponentWinCount myWinCount tieCount =
    do System.Console.WriteLine("\r\nComputer Wins: {0}\r\nPlayer Wins: {1}\r\nTies: {2}", myWinCount, opponentWinCount, tieCount)
    do System.Console.WriteLine("\r\nWhat is your throw? (R)ock, (P)aper, or (S)cissors?")
    let myNextMove =
        match opponentsPreviousMoves with
        | [] -> getRandomMove()
        | hd::tl -> getNextMove (getBestStrat opponentsPreviousMoves) hd
    let hisNextMove = getMoveSafe()
    do System.Console.WriteLine("\r\n\r\nComputer Move: {0}\r\nPlayer Move: {1}", printMove myNextMove, printMove hisNextMove)
    if hisNextMove = counter myNextMove
    then go (take10 (hisNextMove::opponentsPreviousMoves)) (opponentWinCount+1) myWinCount tieCount
    else if myNextMove = counter hisNextMove 
    then go (take10 (hisNextMove::opponentsPreviousMoves)) opponentWinCount (myWinCount+1) tieCount
    else go (take10 (hisNextMove::opponentsPreviousMoves)) opponentWinCount myWinCount (tieCount+1)

go [] 0 0 0