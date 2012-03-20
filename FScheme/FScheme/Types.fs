module FScheme.Types

type Either<'A, 'B> =
    | Right of 'A
    | Left of 'B

type SExpr =
    | STrue
    | SFalse 
    | SNull
    | SSymbol of string
    | SString of string
    | SNumeric of float
    | SList of SExpr list
    | SLet of (string * SExpr) list * SExpr list
    | SLambda of string list * SExpr list
    | SDefine of string * SExpr
    | SDefineFunc of string list * SExpr list
    | SIf of SExpr * SExpr * SExpr
    | SQuoted of SExpr
    | SAssign of string * SExpr
    | SClosure of string list * Env * SExpr list

and Env = (Map<string, SExpr> ref) list