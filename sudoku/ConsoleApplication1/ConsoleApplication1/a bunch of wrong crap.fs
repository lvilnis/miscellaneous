
let x = -1
let sudoku = [ 
  5; 3; x; x; 7; x; x; x; x;
  6; x; x; 1; 9; 5; x; x; x;
  x; 9; 8; x; x; x; x; 6; x;
  8; x; x; x; 6; x; x; x; 3;
  4; x; x; 8; x; 3; x; x; 1;
  7; x; x; x; 2; x; x; x; 6;
  x; 6; x; x; x; x; 2; 8; x;
  x; x; x; 4; 1; 9; x; x; 5;
  x; x; x; x; 8; x; x; 7; 9; ]
let atPos i j (sudoku: 'a list) =
    if 1 > i || 1 > j || i > 9 || j > 9 then failwith "bad index" else
    sudoku.[9 * (i - 1) + j - 1]
    
type Cell = | Variable of int | Constant of int
type Constraint = | Constraint of (Cell Set) * (Cell Set)

let tagged = 
    seq { for i in [1..9] do
          for j in [1..9] do
          yield (if (atPos i j sudoku = x) 
                   then Variable (i * 10 + j)
                   else Constant (atPos i j sudoku)) }
              
// Seq.iter (function | Variable (num) -> printfn "%i" num | Constant (num) -> printfn "%i" num) tagged

let getComplement cnstSet =
    Set.difference (Set.ofList ([1..9] |> List.map (fun num -> Constant num))) cnstSet

let createConstraint cellList =
    let variables = List.filter (function | Variable _ -> true | _ -> false) cellList
    let constants = List.filter (function | Variable _ -> false | _ -> true) cellList
    Constraint (Set.ofList variables, getComplement (Set.ofList constants))

let rowConstraints tagged = 
    seq { for row in [1..9] do
          yield createConstraint ([1..9] |> List.map (fun col -> atPos row col (List.ofSeq tagged))) }

let colConstraints tagged = 
    seq { for col in [1..9] do
          yield createConstraint ([1..9] |> List.map (fun row -> atPos row col (List.ofSeq tagged))) }

let sectionConstraints tagged = 
    seq { for row in [1..3] do
          for col in [1..3] do
          let cells = 
              seq { for lilRow in [1..3] do
                    for lilCol in [1..3] do
                    yield atPos (3 * (row - 1) + lilRow) (3 * (col - 1) + lilCol) (List.ofSeq tagged) }
          yield createConstraint (List.ofSeq cells) }

let cellText = function | Variable id -> "(x" + id.ToString() + ")" | Constant num -> "( " + num.ToString() + " )" 

let printConstraint cstrnt = 
    match cstrnt with
    | Constraint (varSet, constSet) -> let left = (Set.fold (fun acc el -> acc + " " + cellText el) "" varSet) in 
                                       let right = (Set.fold (fun acc el -> acc + " " + cellText el) "" constSet) in 
                                       printfn "%s" (left + " <- " + right)                       
let allConstraints tagged =
    seq { for ish in [ rowConstraints tagged; colConstraints tagged; sectionConstraints tagged; ] do
          for stuff in ish do
          yield stuff }

let intersectConstraints (first, second) =
    match (first, second) with
    | (Constraint (lv, lc), Constraint (rv, rc)) -> Constraint (Set.intersect lv rv, Set.intersect lc rc)

let diffConstraints (first, second) =
    match (first, second) with
    | (Constraint (lv, lc), Constraint (rv, rc)) -> Constraint (Set.difference lv rv, Set.difference lc rc)

let partition toSub target =
    let isect = intersectConstraints (target, toSub) 
    let diff = diffConstraints (target, toSub)
    let diff2 = diffConstraints (toSub, target)
    match isect with
    | Constraint(leftSet, rightSet) -> if ((Set.count leftSet) > 0 && (Set.count rightSet) > 0 && toSub <> target) 
                                       then Seq.ofList [isect;diff;diff2] |> Seq.filter (function | Constraint(leftSet, rightSet) -> 
                                                                                                    let lcount = (Set.count leftSet)
                                                                                                    let rcount = Set.count rightSet
                                                                                                    lcount > 0 && rcount > 0 && lcount >= rcount)
                                       elif toSub = target 
                                       then Seq.ofList []
                                       else Seq.ofList [target]

let isPrimitive cstrnt =
    match cstrnt with
    | Constraint (first, second) when Set.count first = 1 && Set.count second = 1 -> true
    | _ -> false

let rec solve (constraints : seq<Constraint>) solved = 
    printfn "=============== solve ==============="
    printfn "Remaining to solve:"
    Seq.iter printConstraint constraints
    printfn "Solved:"
    Seq.iter printConstraint solved
    if (constraints |> Seq.length) = 0 then solved else
    match List.ofSeq constraints with
    | hd::tl -> let subbedTail = seq { for c in tl do
                                       for sub in partition hd c do
                                       yield sub }
                (if isPrimitive hd then (solve subbedTail (hd::solved)) else (solve (Seq.append subbedTail []) solved))


let solve2 (constraints : seq<Constraint>) var =
    constraints |> Seq.filter (function | Constraint(left, right) -> Set.contains var left) |> Seq.reduce (fun a b -> intersectConstraints(a,b))

let replaceTagged tagged solved =
    seq { for cell in tagged do
          if solved |> Seq.exists (fun c -> match c with | Constraint(leftSet,_) -> Set.contains cell leftSet)
          then yield solved |> Seq.find (fun c -> match c with | Constraint(leftSet,_) -> Set.contains cell leftSet)
                            |> (fun c -> match c with | Constraint(_,rightSet) -> (Set.toList rightSet).[0])
          else yield cell }

let partiallySolve tagged =
     let constraints = Seq.cache (allConstraints tagged)
     let variables = tagged |>  Seq.filter (function | Variable _ -> true | _ -> false)  
     variables
     |> Seq.map (solve2 constraints) 
     |> Seq.filter (function | Constraint(_,r) -> Set.count r = 1)
     |> replaceTagged tagged

let rec reallySolve tagged =
    let tagged = Seq.cache tagged
    if Seq.exists (function | Variable _ -> true | Constant _ -> false) tagged
    then partiallySolve tagged |> reallySolve
    else tagged

let printGrid tagged = 
    let tagged = List.ofSeq tagged
    for row in [1..9] do
    printfn "%s" ([1..9] |> List.map (fun col -> atPos row col tagged) |> List.fold (fun acc el -> acc + " " + cellText el) "")

//tagged |> partiallySolve |> partiallySolve |> partiallySolve |> partiallySolve |> partiallySolve |> partiallySolve |> partiallySolve |> printGrid