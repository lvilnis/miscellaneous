module FScheme.Compiler

open FScheme.Types

open Microsoft.FSharp.Quotations

// racket for .net would be cool!!
// would be nice to have this compile dynamically typed parts and statically types parts...
// so like it could compile some of it to .net types and others itd have to compile to operations on SExprs

let verbose = true
let unitVar0 = new Var("unitVar0", typeof<unit>, false)
let vars = (ref []) : Var list ref
let createVar sym = 
    let newVar = new Var(sym, typeof<SExpr>, true)
    vars := newVar::(!vars)
    newVar
let createVars syms = List.map createVar syms
let lookupVar sym = List.find (fun (var : Var) -> var.Name = sym) !vars 
let sequence (first::exprs) = List.fold (fun acc el -> Expr.Sequential(acc, el)) first exprs
let rec csequence sexprs = sequence (List.map compile sexprs)
and compileList sexprs =
    csequence sexprs
and compile sexpr : Expr =
    if verbose then printfn "Compile (before): %A" sexpr
    let compiled = match sexpr with
    | SNumeric(_) | SString(_) | SFalse | STrue | SNull -> Expr.Value(sexpr)
    // | SAssign(var, value) -> Expr.VarSet(createVar var, compile value) //This is unsupported! in LINQ Expression Trees!
    | SSymbol(sym) -> 
        match sym with
        | "%" -> <@ fun (SNumeric(a)) (SNumeric(b)) -> SNumeric(a % b) @>.Raw // look into lifting these
        | "+" -> <@ fun (SNumeric(a)) (SNumeric(b)) -> SNumeric(a + b) @>.Raw
        | "-" -> <@ fun (SNumeric(a)) (SNumeric(b)) -> SNumeric(a - b) @>.Raw
        | "/" -> <@ fun (SNumeric(a)) (SNumeric(b)) -> SNumeric(a / b) @>.Raw
        | "*" -> <@ fun (SNumeric(a)) (SNumeric(b)) -> SNumeric(a * b) @>.Raw
        | "=" -> <@ fun a b -> if a = b then STrue else SFalse @>.Raw
        | "cons" -> <@ fun hd (SQuoted(SList(tl))) -> SQuoted(SList(hd::tl)) @>.Raw
        | "car" -> <@ fun (SQuoted(SList(hd::tl))) -> hd @>.Raw
        | "cdr" -> <@ fun (SQuoted(SList(hd::tl))) -> SQuoted(SList(tl)) @>.Raw
        | "map" -> <@ fun (func: SExpr -> SExpr) (SQuoted(SList(xs))) -> List.map func xs @>.Raw
        | "strcat" -> <@ fun (SString(a)) (SString(b)) -> SString(a + b)@>.Raw
        | sym -> Expr.Var(lookupVar sym)
    | SLambda(args, body) ->
        let boundArgs = (if args.Length = 0 then [unitVar0] else createVars (List.rev args))
        List.fold (fun acc arg -> Expr.Lambda(arg, acc)) (csequence body) boundArgs
    | SIf(cond, thenExpr, elseExpr) ->
        let ccond, cthen, celse = compile cond, compile thenExpr, compile elseExpr
        Expr.IfThenElse(ccond, cthen, celse)
    | SLet(bindings, bodyExprs) ->
        let boundVars = createVars (bindings |> List.map fst)
        let compiledBindings = List.map compile (bindings |> List.map snd)
        List.zip boundVars compiledBindings |>
        (List.fold (fun acc (symbol, value) -> Expr.Let(symbol, value, acc)) 
            (csequence bodyExprs))
//    | SDefine(sym, bodyExpr) ->   // Can't do defines very properly...
//    | SDefineFunc(syms, bodyExprs ->
//        Expr.let
    | SList(sexprs) ->
        match sexprs with
        | oper::args -> Expr.Applications(compile oper, List.map (fun el -> [compile el]) args)
        | [] -> failwith "AHHH!!! empty list!!!!"
    | _ -> failwith (sprintf "Unsupported SExpr type for compilation: %A" sexpr)
    if verbose then printfn "Compile (after): %A" compiled
    compiled

let executeTimed (func : unit -> 'T) : 'T * int =
    let timer = new System.Diagnostics.Stopwatch()
    do timer.Start()
    let results = func()
    do timer.Stop()
    (results, timer.Elapsed.Milliseconds)

let RunProgram source : string = 
    try 
        let sexprTree = FScheme.Parser.ParseSchemeStringToExpressionTree source
        let fsharpexpression = compileList sexprTree
        let compiledDelegate, compileTime = executeTimed (fun ( ) -> Linq.QuotationEvaluator.CompileUntyped fsharpexpression)
        let results, executionTime = executeTimed compiledDelegate
        sprintf "Success (Timing { Compile: %i ms Execution: %i ms }) : %A" compileTime executionTime results
    with 
        err -> (sprintf "Failure: %s" err.Message)


