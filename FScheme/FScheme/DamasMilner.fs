module FScheme.DamasMilner

open FScheme.Types

type TypeParameter = TypeParameter of int

type SExprType =
    | TFunc of TypeParameter list * SExprType list * SExprType
    | TString
    | TNumeric
    | TBool
    | TNull
    | TList of SExprType
    | TQuoted //for now
    | TParam of TypeParameter

type InferenceExpr =
    | IKnown of SExprType
    | ICompound of CompoundInferenceExpr
    | IVar of InferenceVariable

and CompoundInferenceExpr =
    | CFunction of InferenceExpr list * InferenceExpr
    | CList of InferenceExpr

and InferenceVariable =
    | VIdentifier of string * int
    | VExpression of SExpr
    | VParameter of int

type TypingScope = (string * int) Set list ref

type Constraint = Constraint of InferenceExpr * InferenceExpr

type Substitution = Substitution of InferenceVariable * InferenceExpr


let (<==) left right = Substitution(left, right)
let (<=>) left right = Constraint(left, right)
let (-->) args ret = ICompound(CFunction(args, ret))

let VId = IVar << VIdentifier
let VEx = IVar << VExpression
let VPa = IVar << VParameter 
let CLi = ICompound << CList
let K = IKnown

let generateTypeConstraints (expressions : SExpr list) : Constraint list =
    let constraints = new ResizeArray<Constraint>()
    let typingScope = ref [Set.empty] : TypingScope
    let push c = do constraints.Add(c)
    let getFreshIdForSym =
        let count = ref 0
        (fun () -> 
            let current = !count
            do count := current + 1
            current)
    let fresh =
        let tvCount = ref 0
        (fun () -> 
            let current = !tvCount
            do tvCount := current + 1
            VPa(current))
    let pushScope() = 
        do typingScope := Set.empty::(!typingScope)
    let popScope() =
        let _::oldScope = !typingScope
        do typingScope := oldScope
    let bindIdentifier (var : string) =
        let currentFrame::parentScopes = !typingScope
        let freshId = getFreshIdForSym()
        do typingScope := (Set.add (var, freshId) currentFrame)::parentScopes
        VId(var, freshId)
    let bindIdentifiers = List.map bindIdentifier
    let lookupIdentifier (var : string) =
        let matches = (fun (sym, id) -> var=sym)
        List.tryFind (Set.exists matches) !typingScope 
            |> Option.map (Set.toList >> List.find matches >> VId)
    let rec walkExprs terms = List.map walkExpr terms
    and walkExpr (term : SExpr) : InferenceExpr =
        match term with
        | STrue -> K(TBool)
        | SFalse -> K(TBool)
        | SNull -> K(TNull)
        | SString(_) -> K(TString)
        | SNumeric(_) -> K(TNumeric)
        | SQuoted(_) -> K(TQuoted) // for now!!!... ??
        | SSymbol(sym) -> 
            match sym with
            | "%" | "+" | "-" | "/" | "*" -> [K(TNumeric); K(TNumeric)] --> K(TNumeric)
            | "=" -> let a = fresh() in [a; a] --> K(TBool)
            | "strcat" -> [K(TString); K(TString)] --> K(TString)
            | "map" -> let a, b = fresh(), fresh() in [[a] --> b; CLi(a)] --> CLi(b)
            | "cons" -> let a = fresh() in [a; CLi(a)] --> CLi(a)
            | "car"-> let a = fresh() in [CLi(a)] --> a
            | "cdr"-> let a = fresh() in [CLi(a)] --> CLi(a)
            | sym -> 
                match lookupIdentifier(sym) with
                | Some(idvariable) -> idvariable
                | None -> failwith (sprintf "Can't find identifier %s" sym)
        | SList(oper::args) -> // remember, for some reason i named combination/application "SList"
            let functionInferenceExpr::argInferenceExprs = walkExprs (oper::args)
            do push (functionInferenceExpr <=> argInferenceExprs --> VEx(term))
            VEx(term)
        | SLet(bindings, bodyExprs) ->
            let syms, bindingBodies = List.unzip bindings
            let walkedBindings = walkExprs bindingBodies
            do pushScope()
            let boundIdentifiers = bindIdentifiers syms
            for (sym, bindingBody) in bindings
                do push (bindIdentifier(sym) <=> walkExpr bindingBody)
            let ret::_ = walkExprs bodyExprs |> List.rev
            do popScope()
            do push (ret <=> VEx(term))
            VEx(term)
        | SLambda(argStrs, bodyExprs) ->
            do pushScope()
            let boundArgs = bindIdentifiers argStrs
            let ret::_ = walkExprs bodyExprs |> List.rev
            do popScope()
            do push (boundArgs --> ret <=> VEx(term))
            VEx(term)
        | SDefine(sym, bodyExpr) ->
            let boundIdentifier = bindIdentifier(sym)
            do pushScope()
            do push (boundIdentifier <=> walkExpr bodyExpr)
            do popScope()
            K(TNull)
        | SDefineFunc(sym::argStrs, bodyExprs) ->
            let boundIdentifier = bindIdentifier(sym)
            do pushScope()
            let boundArgs = bindIdentifiers argStrs
            let ret::_ = walkExprs bodyExprs |> List.rev
            do push (boundArgs --> ret <=> boundIdentifier)
            do popScope()
            K(TNull)
        | SIf(cnd, thn, els) -> 
            let ifEx, cndEx, thnEx, elsEx = VEx(term), walkExpr cnd, walkExpr thn, walkExpr els
            do push (cndEx <=> K(TBool))
            do push (thnEx <=> elsEx)
            do push (ifEx <=> thnEx)
            ifEx
        | SAssign(varName, value) -> 
            do push (VEx(term) <=> walkExpr value)
            VEx(term)
        //| SClosure of string list * Env * SExpr list
        | anything -> VEx(anything)
    do walkExprs expressions |> ignore
    constraints |> List.ofSeq

//val UnifyConstraints : Constraint list -> Substitution list -> Either<Substitution list, string>
let rec UnifyConstraints (constraints : Constraint list) (substitution : Substitution list) : (Either<Substitution list, string>) =
    match constraints with
    | Constraint(IVar(variable), other)::rest
    | Constraint(other, IVar(variable))::rest ->
        let newConstraints, newSubs = substitute rest substitution (variable <== other)
        UnifyConstraints newConstraints ((variable <== other)::newSubs)
    | Constraint((ICompound(CFunction(largs, lret)) as left), (ICompound(CFunction(rargs, rret)) as right))::rest ->
        if List.length largs <> List.length rargs 
        then Left(sprintf "Failed to unify %A with %A - different number of arguments" left right)
        else UnifyConstraints ([lret <=> rret] @ (List.map2 (<=>) largs rargs) @ rest) substitution
    | Constraint(ICompound(CList(lt)), ICompound(CList(rt)))::rest ->
        UnifyConstraints ([lt <=> rt] @ rest) substitution
    | Constraint(l, r)::rest when l = r -> UnifyConstraints rest substitution
    | Constraint(l, r)::rest -> Left (sprintf "Failed to unify %A with %A" l r)
    | [] -> Right substitution

and substitute (constraints : Constraint list) (substitution : Substitution list) (vnv : Substitution) =
    let subbedConstraints = constraints |> List.map (fun (Constraint(lhs, rhs)) ->
        subExpr vnv lhs <=> subExpr vnv rhs)
    let subbedSubstitution = substitution |> List.map (fun (Substitution(lhs, rhs)) -> 
        lhs <== subExpr vnv rhs)
    subbedConstraints, subbedSubstitution

    // add "." notation for varargs and type checking
    // finish javascript parser and work on static analysis

and subExpr sub target = // turn into data directed so that type constraints can be generated modularly
    let (Substitution(var, newVal)) = sub
    match target with
    | ICompound(CFunction(args, ret)) -> ICompound(CFunction(List.map (subExpr sub) args, subExpr sub ret))
    | ICompound(CList(el)) -> ICompound(CList(subExpr sub el))
    | IVar(v) when v = var -> newVal 
    | _ -> target

let makeTree str =
    FScheme.Parser.ParseSchemeStringToExpressionTree str

let GenerateTypeConstraintsForText (text : string) : Constraint list =
    makeTree text |> generateTypeConstraints



let mapr fn = function | Left(msg) -> Left(msg) | Right(value) -> Right(fn value)

let ReplaceUnderConstrainedWithVariables (subst : Substitution list) =
    let inferenceVariablesToTypeVariables = ref Map.empty
    let current = ref -1
    let param v =
        match Map.tryFind v !inferenceVariablesToTypeVariables with
        | Some(p) -> p
        | None ->
            let cur = !current
            current := cur - 1
            inferenceVariablesToTypeVariables := (Map.add v (VParameter(cur)) !inferenceVariablesToTypeVariables)
            VParameter(cur)
    let rec replace iexpr =
        match iexpr with
            | ICompound(CFunction(args, ret)) -> 
                ICompound(CFunction(List.map replace args, replace ret))
            | ICompound(CList(el)) -> ICompound(CList(replace el))
            | IVar(v) -> match v with
                | VExpression(_) | VIdentifier(_, _) -> IVar(param(v))
                | VParameter(n) -> IVar(v)
            | els -> els
    let replaced = subst |> List.map (function | Substitution(var, iexp) -> Substitution(var, replace iexp))
    let shift = -(Map.toList !inferenceVariablesToTypeVariables |> List.map (fun (var, VParameter(n)) -> n) |> List.fold min 0) + 1
    let newSubs = Map.toList !inferenceVariablesToTypeVariables |> List.map (fun (iv,(VParameter(n))) -> Substitution(iv, IVar(VParameter(n+0))))
    List.append replaced newSubs




let GenerateTypesForSubstitution (subst : Substitution list) : (SExpr * SExprType) list =
    let replaced = ReplaceUnderConstrainedWithVariables subst
    let convertSubstitution = function | Substitution(ivar, iexpr) ->
        let lhs = match ivar with
            | VExpression(sexp) -> Some(sexp)
            | VIdentifier(id, n) -> Some(SSymbol(id))
            | VParameter(n) -> None
        let rec getRhs iexpr =
            match iexpr with 
            | IKnown(stype) -> stype
            | IVar(VParameter(n)) -> TParam(TypeParameter(n))
            | ICompound(CFunction(args, ret)) -> TFunc([], List.map getRhs args, getRhs ret)
            | ICompound(CList(et)) -> TList(getRhs et)
        match lhs with
        | Some(lhs) -> Some(lhs, (getRhs iexpr))
        | None -> None
    List.map Option.get (List.filter Option.isSome (List.map convertSubstitution replaced))

let rec prettyPrintType stype =
    match stype with
    | TFunc(typeparams, argtypes, retType) ->
        let printedArgs, printedRet = List.map prettyPrintType argtypes, prettyPrintType retType
        "(" + (List.fold (fun acc el -> acc + " " + el) "" printedArgs) + ") -> " + printedRet
    | TString -> "string"
    | TNumeric -> "numeric"
    | TBool -> "bool"
    | TNull -> "null"
    | TList(et) -> (prettyPrintType et) + " list"
    | TQuoted -> failwith "wtf"
    | TParam(TypeParameter(n)) -> "'T(" + n.ToString() + ")"

let GenerateSubstitutionForText (text : string) : Either<(SExpr * SExprType) list, string> =
    makeTree text 
        |> generateTypeConstraints 
        |> (fun cs -> UnifyConstraints cs []) 
        |> mapr GenerateTypesForSubstitution

// still like the idea of traversing using a zipper...
// but i should totes try to have tree context sensitive clicking around and stuff, with types in the editor

let AnalyzeTypes (text : string) : string =
    let msg = ""
    let msg = msg + sprintf "\n===== Scheme type constraint generation tests: =====\n"
    let msg = msg + sprintf "Success: %A" (GenerateTypeConstraintsForText text)
    let msg = msg + sprintf "\n===== Scheme type inference tests: =====\n"
    let msg = msg + (GenerateSubstitutionForText text
        |> mapr (List.map (fun (l,r) -> (l, prettyPrintType r)))
        |> (function | Left(errmsg) -> sprintf "Failure: %A" errmsg
                     | Right(succ) -> sprintf "Success: %A" succ))
    msg