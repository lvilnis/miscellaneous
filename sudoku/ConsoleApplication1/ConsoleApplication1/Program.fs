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

let printGrid sudoku = 
    let cellText el = if el = -1 then "( x )" else "( " + el.ToString() + " )"
    for row in [1..9] do
    printfn "%s" ([1..9] |> List.map (fun col -> atPos row col sudoku) |> List.fold (fun acc el -> acc + " " + cellText el) "")

let findCellsWithOnlyOnePossibleSolution sudoku =
    seq { for row in [1..9] do
          for col in [1..9] do
          if atPos row col sudoku <> -1 then yield! [] else
          let rowCellGroup = [1..9] |> List.map (fun col -> atPos row col sudoku)
          let colCellGroup = [1..9] |> List.map (fun row -> atPos row col sudoku)
          let sectionCellGroup =
              let sectionRow = (row - 1) / 3 + 1
              let sectionCol = (col - 1) / 3 + 1
              seq { for lilRow in [1..3] do
                    for lilCol in [1..3] do
                    yield (atPos (3 * (sectionRow - 1) + lilRow) (3 * (sectionCol - 1) + lilCol) sudoku) } 
          let combinedNums = List.concat [rowCellGroup; colCellGroup; List.ofSeq sectionCellGroup] |> List.filter ((<>) x) |> Set.ofList
          let possibleValues = Set.difference (Set.ofList [1..9]) combinedNums
          if Set.count possibleValues = 1 then yield (row, col, (Set.toList possibleValues).[0]) }

let rec solve sudoku =
    let uniques = findCellsWithOnlyOnePossibleSolution sudoku |> Seq.cache
    let replace sudoku uniques =
        seq { for row in [1..9] do
              for col in [1..9] do
              let unique = uniques |> Seq.tryFind (function | (r, c, v) -> r = row && c = col)
              match unique with | Some (r, c, v ) -> yield v | _ -> yield atPos row col sudoku }
    if uniques |> Seq.length = 0 then sudoku 
    else solve (List.ofSeq (replace sudoku uniques))

do printfn "=== Unsolved Sudoku ==="
do printGrid sudoku
do printfn "=== Solved Sudoku ==="
do sudoku |> solve |> printGrid

do System.Console.ReadKey() |> ignore