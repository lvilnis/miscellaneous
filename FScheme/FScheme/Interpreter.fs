module FScheme.Interpreter

open FScheme.Types
open Microsoft.FSharp.Quotations

let verbose = false

let lookup (env : Env) sym =
    match List.tryFind (fun frame -> Map.containsKey sym !frame) env with
    | Some frame -> (!frame).[sym]
    | None -> SSymbol(sym)

let rec eval env sexpr : (SExpr * Env) =
    if verbose then printfn "Eval: %A" (sexpr, env)
    match sexpr with
    | SSymbol(sym) -> (lookup env sym, env)
    | SLambda(args, body) -> (SClosure(args, env, body), env)
    | SIf(cond, thenExpr, elseExpr) ->
        let evaldCond, newEnv = eval env cond
        let condVal = 
            match evaldCond with 
            | STrue -> true 
            | SFalse -> false 
            | _ -> failwith "invalid if condition"
        eval newEnv (if condVal then thenExpr else elseExpr)
    | SDefineFunc(sym::args, body) ->
        let procedure = SClosure(args, env, body)
        let curFrame::_ = env
        do curFrame := (Map.add sym procedure !curFrame)
        (SNull, env)
    | SDefine(sym, expr) -> 
        let evaldBody, newEnv = eval env expr
        let curFrame::_ = newEnv
        do curFrame := (Map.add sym evaldBody !curFrame)
        (SNull, env)
    | SAssign(sym, value) ->
        let evaldValue, newEnv = eval env value
        (evaldValue, setVariable newEnv sym evaldValue)
    | SLet(bindings, bodyExprs) ->
        let keyList,valueList = List.unzip bindings
        let evaldValueList,newEnv = evlist env valueList
        let evaldBindingList = List.zip keyList evaldValueList
        let innerEnv = (ref (Map.ofList evaldBindingList)) :: env
        let evaldExprs, _::newOuterEnv = evlist innerEnv bodyExprs
        let finalValue::_ = List.rev evaldExprs
        (finalValue, newOuterEnv)
    | SList(sexprs) ->
        match sexprs with
        | oper::args -> 
            let evaldArgs, newEnv = evlist env args
            let evaldOper, newEnv = eval newEnv oper
            apply newEnv evaldOper evaldArgs
        | [] -> (SList([]), env)
    | SQuoted(inner) -> (inner, env)
    | s -> (s, env)

and apply env proc args =
    if verbose then printfn "Apply: %A" (proc, args, env)
    match proc with
    | SClosure(argList, innerEnv, bodyExprs) ->
        let evaldExprs, _::newOuterEnv = evlist (bindArgs innerEnv argList args) bodyExprs
        let finalValue::_ = List.rev evaldExprs
        (finalValue, env)
    | SSymbol(sym) -> applyPrimitive env sym args

and applyPrimitive env sym args =
    let applyArithmetic op =
        (List.map (function | SNumeric(n) -> n) args 
        |> List.reduce op
        |> SNumeric, env)
    match (sym, args) with
    | "%",_ -> applyArithmetic (%)
    | "+",_ -> applyArithmetic (+)
    | "-",_ -> applyArithmetic (-)
    | "/",_ -> applyArithmetic (/)
    | "*",_ -> applyArithmetic ( * )
    | "=", (l::r::[]) -> (if l = r then STrue else SFalse), env
    | "strcat",_ -> (SString(args |> List.map (function SString(str)->str)  |> String.concat "")), env
    | "list",_ -> SList(args), env
    | "map", func::SList(toMap)::[] -> SList((List.map (fun el -> (apply env func [el])) toMap) |> List.map fst), env
    | ("cons", newHead :: (SList(xs)) :: []) -> (SList(newHead::xs), env)
    | ("car", (SList(hd::_))::[]) -> (hd, env)
    | ("cdr", (SList(_::tl))::[]) -> (SList(tl), env)
    | _ -> failwith (sprintf "unknown operator or invalid use: oper:%A args:%A" sym args)

and evlist env sexprs : (SExpr list * Env) = 
    let accum acc el =
        match acc with
        | (evald, lastEnv) -> 
            let evaldExpr, nextEnv = (eval lastEnv el)
            (evaldExpr::evald, nextEnv)
    List.fold accum ([], env) sexprs |> (fun (xs, env) -> (List.rev xs, env))

and setVariable env varName value =
    let idx = List.tryFindIndex (fun frame -> Map.containsKey varName !frame) env
    match idx with
    | Some idx -> 
        let frame = env.[idx]
        let newFrame = Map.add varName value !frame
        do List.iteri (fun i f -> if i=idx then do f:=newFrame) env
        env
    | _ -> failwith "No such variable to set!!" 
    
and bindArgs env argList args = (ref (List.zip argList args |> Map.ofList)) :: env

let RunProgram source : SExpr list * Env = 
    FScheme.Parser.ParseSchemeStringToExpressionTree source
    |> evlist [ref Map.empty]

let GetProgramText source = 
    RunProgram source |> fst |> List.map (fun sexpr -> sprintf "%A" sexpr)
                      |> List.fold (fun acc el -> acc + "\r\n" + el) ""


