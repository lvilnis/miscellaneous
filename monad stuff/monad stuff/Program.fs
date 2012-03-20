// Learn more about F# at http://fsharp.net

type 'T Log = Log of 'T * (string list)

type LogBuilder() = 
    member x.Bind(Log(value, messages), f) = 
        let (Log(newValue, newMessages)) = f(value) in Log(newValue, newMessages @ messages)
    member x.Return(value) = Log(value, [])
    member x.Zero() =  Log((), [])

let log = new LogBuilder()

let write value messages =  Log (value, [messages])
let printf message = Log((), [message])

let foo =
    log { do! printf "Starting computation..."
          let x = 1
          do! printf ("x: "+x.ToString())
          let y = x + 1
          do! printf ("y: "+y.ToString())
          let z = y * 4
          do! printf ("z: "+z.ToString())
          let w = z - 1
          do! printf ("w: "+w.ToString())
          return w }

let printLog = function | Log(_, msgs) -> msgs |> List.rev |> List.iter (fun el -> printfn "%s" el) 

type 'T Dist = Dist of (int -> 'T)

let seed = ref (int (System.DateTime.Now.Ticks % int64 100))

let sample (Dist(d)) = 
    let currentSeed = !seed
    printfn "seed: %i" currentSeed
    do seed := !seed + 1
    d(currentSeed)

type DistBuilder() =
    member x.Bind(dist, f) = f(sample dist)
    member x.Return(value) = Dist(fun i -> value)
    member x.Delay(f) = Dist(fun i -> sample (f()))

let dist = new DistBuilder()
let rnd = new System.Random()
let uniform = Dist(fun i -> rnd.NextDouble())

type CoinFace = | Heads | Tails

let coinflip = 
    dist { let! u = uniform
           if u <= 0.5 
           then return Heads 
           else return Tails }

let cases (caseList : (float*'a) list) : 'a Dist =
    let sumOfWeights = caseList |> List.sumBy fst
    let accumulateWeights acc el =
        match (acc, el) with
        | ((last,_)::tl,(next, v)) -> (last+next, v)::acc
        | _ -> el::acc
    let percentiles = caseList |> List.fold accumulateWeights [] |> List.rev
    dist { let! u = uniform
           let normalized = u * sumOfWeights
           let (_,matching) =  percentiles |> List.find (fun (wgt,_) -> wgt >= normalized)
           return matching }

let coinflip2 = cases [ (0.5, Heads); (0.5, Tails) ]

let nSidedDie n = cases [ for i in 1 .. n -> (1.0, i) ]

type Player = Player of int * int * int * int * int * int

let d6 = nSidedDie 6

let liftDist (func : 'a -> 'b) x =
    dist { let! a = x in return func a }

let liftDist2 (func : 'a -> 'b -> 'c) x y =
    dist { let! a = x
           let! b = y
           return func a b }

let stat = 
    dist { let! r1 = d6
           let! r2 = d6
           let! r3 = d6
           return r1 + r2 + r3 }

let stat2 = [d6;d6;d6] |> List.reduce (liftDist2 (+))

let player =
    dist { let! str = stat
           let! con = stat
           let! dex = stat
           let! wis = stat
           let! int = stat
           let! cha = stat
           return Player(str,con,dex,wis,int,cha) }

let consDist = liftDist2 (fun acc el -> List.Cons(el,acc))
let nilDist = dist { return [] }

let player2 =
    dist { let! [str;con;dex;wis;int;cha] = stat |> List.replicate 6 |> List.fold consDist nilDist
           return Player(str,con,dex,wis,int,cha) }

let runDist d n = 
    dist { let! samples = d |> List.replicate n |> List.fold consDist nilDist
           let groups = Seq.groupBy (fun i -> i) samples
           let counts = groups |> Seq.map (fun (k, xs) -> (k, Seq.length xs))
           return List.ofSeq counts |> List.sortBy fst }

let printCounts stuff = 
    let printCount (el,ct) =
        let countStr = (List.replicate ct "+" |> List.reduce (+)):string
        printfn ("Value: %i %s") el countStr
    dist { let! x = stuff 
           return List.iter printCount x }

// applicative functor implementations
let p v = Dist(fun i -> v)
let (<*>) afunc d1 = Dist(fun i -> ((sample afunc) (sample d1)))
let fmap func d1 = (p func) <*> d1
let (<&>) a b = fmap a b

let fmap2 func d1 d2 = func <&> d1 <*> d2

let flip f a b = f b a

let (><) = fmap2 (*)
let (+?) = fmap2 (+)
let dCons l r = (fun a b -> b :: a) <&> l <*> r

let liftList ds = List.fold dCons (p []) ds

let stat3 = d6 +? d6 +? d6
let player3 =  
    let stats = stat |> List.replicate 6 |> liftList
    let toPlayer = function | [str;con;dex;wis;int;cha] -> Player(str,con,dex,wis,int,cha)
    fmap toPlayer stats

let walk steps = 
    let stepList = List.replicate steps coinflip2
    let upDownList = List.map (function | Heads -> -1 | Tails -> 1) <&> (liftList stepList) 
    let cumulative = List.scan (+) 0 <&> upDownList
    cumulative

let hasToBeMonad =
    dist { let! n = d6
           let! flipNTimes = List.replicate n coinflip2 |> liftList
           let isHead = function | Heads -> true | _ -> false
           let numHeads = flipNTimes |> List.filter isHead |> List.length
           return numHeads }

let foo2 = runDist (nSidedDie 10 >< nSidedDie 10) 1000 |> printCounts |> sample
     




 



