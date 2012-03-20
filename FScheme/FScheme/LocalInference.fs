module FScheme.LocalInference

open FScheme.Types

type TypeVariable = TypeParam of int

// i need to add for real type variables to my hindley milner
// checker, to get let polymorphism.
// really simple lambda-calculus only types, for the moment
type SExprType =
    | TFunc of TypeVariable list * SExprType list * SExprType
    | TVariable of TypeVariable
    | TString
    | TNumeric
    | TBool
    | TNull
    //| TList of SExprType // no lists for now, polymorphism is hard
    //| TQuoted //for now
//    | TTop           // no subtyping for now
//    | TBottom

// example
let idType = 
    TFunc([TypeParam 1], [TVariable (TypeParam 1)], TVariable (TypeParam 1))

type InferenceVariable =
    | VIdentifier of string * int
    | VExpression of SExpr
    | VParameter of int

// need a substitution for inferencevariables to types
// if some are typevariables, they will be substituted with solutions
type Substitution = Map<InferenceVariable, SExprType>

type Constraint = Constraint of SExprType * TypeVariable * SExprType
//let VId = IVar << VIdentifier
//let VEx = IVar << VExpression
//let VPa = IVar << VParameter 
//let CLi = ICompound << CList
//let K = IKnown
//  
//
//let generateTypeConstraints (expressions : SExpr list) : Constraint list =
//    let constraints = new ResizeArray<Constraint>()
//    let typingScope = ref [Set.empty] : TypingScope
//    let push c = do constraints.Add(c)
//    let getFreshIdForSym =
//        let count = ref 0
//        (fun () -> 
//            let current = !count
//            do count := current + 1
//            current)
//    let fresh =
//        let tvCount = ref 0
//        (fun () -> 
//            let current = !tvCount
//            do tvCount := current + 1
//            VPa(current))
//    let pushScope() = 
//        do typingScope := Set.empty::(!typingScope)
//    let popScope() =
//        let _::oldScope = !typingScope
//        do typingScope := oldScope
//    let bindIdentifier (var : string) =
//        let currentFrame::parentScopes = !typingScope
//        let freshId = getFreshIdForSym()
//        do typingScope := (Set.add (var, freshId) currentFrame)::parentScopes
//        VId(var, freshId)
//    let bindIdentifiers = List.map bindIdentifier
//    let lookupIdentifier (var : string) =
//        let matches = (fun (sym, id) -> var=sym)
//        List.tryFind (Set.exists matches) !typingScope 
//            |> Option.map (Set.toList >> List.find matches >> VId)
//    let rec walkExprs terms = List.map walkExpr terms
//    and walkExpr (term : SExpr) : InferenceExpr =
//        match term with
//        | STrue -> K(TBool)
//        | SFalse -> K(TBool)
//        | SNull -> K(TNull)
//        | SString(_) -> K(TString)
//        | SNumeric(_) -> K(TNumeric)
//        | SQuoted(_) -> K(TQuoted) // for now!!!... ??
//        | SSymbol(sym) -> 
//            match sym with
//            | "%" | "+" | "-" | "/" | "*" -> [K(TNumeric); K(TNumeric)] --> K(TNumeric)
//            | "=" -> let a = fresh() in [a; a] --> K(TBool)
//            | "strcat" -> [K(TString); K(TString)] --> K(TString)
//            | "map" -> let a, b = fresh(), fresh() in [[a] --> b; CLi(a)] --> CLi(b)
//            | "cons" -> let a = fresh() in [a; CLi(a)] --> CLi(a)
//            | "car"-> let a = fresh() in [CLi(a)] --> a
//            | "cdr"-> let a = fresh() in [CLi(a)] --> CLi(a)
//            | sym -> 
//                match lookupIdentifier(sym) with
//                | Some(idvariable) -> idvariable
//                | None -> failwith (sprintf "Can't find identifier %s" sym)
//        | SList(oper::args) -> // remember, for some reason i named combination/application "SList"
//            let functionInferenceExpr::argInferenceExprs = walkExprs (oper::args)
//            do push (functionInferenceExpr <=> argInferenceExprs --> VEx(term))
//            VEx(term)
//        | SLet(bindings, bodyExprs) ->
//            let syms, bindingBodies = List.unzip bindings
//            let walkedBindings = walkExprs bindingBodies
//            do pushScope()
//            let boundIdentifiers = bindIdentifiers syms
//            for (sym, bindingBody) in bindings
//                do push (bindIdentifier(sym) <=> walkExpr bindingBody)
//            let ret::_ = walkExprs bodyExprs |> List.rev
//            do popScope()
//            do push (ret <=> VEx(term))
//            VEx(term)
//        | SLambda(argStrs, bodyExprs) ->
//            do pushScope()
//            let boundArgs = bindIdentifiers argStrs
//            let ret::_ = walkExprs bodyExprs |> List.rev
//            do popScope()
//            do push (boundArgs --> ret <=> VEx(term))
//            VEx(term)
//        | SDefine(sym, bodyExpr) ->
//            let boundIdentifier = bindIdentifier(sym)
//            do pushScope()
//            do push (boundIdentifier <=> walkExpr bodyExpr)
//            do popScope()
//            K(TNull)
//        | SDefineFunc(sym::argStrs, bodyExprs) ->
//            let boundIdentifier = bindIdentifier(sym)
//            do pushScope()
//            let boundArgs = bindIdentifiers argStrs
//            let ret::_ = walkExprs bodyExprs |> List.rev
//            do push (boundArgs --> ret <=> boundIdentifier)
//            do popScope()
//            K(TNull)
//        | SIf(cnd, thn, els) -> 
//            let ifEx, cndEx, thnEx, elsEx = VEx(term), walkExpr cnd, walkExpr thn, walkExpr els
//            do push (cndEx <=> K(TBool))
//            do push (thnEx <=> elsEx)
//            do push (ifEx <=> thnEx)
//            ifEx
//        | SAssign(varName, value) -> 
//            do push (VEx(term) <=> walkExpr value)
//            VEx(term)
//        //| SClosure of string list * Env * SExpr list
//        | anything -> VEx(anything)
//    do walkExprs expressions |> ignore
//    constraints |> List.ofSeq
//
//
//
//
