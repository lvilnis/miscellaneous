module FScheme.Parser

open FParsec
open FScheme.Types
// Boring generic aliases for common parsers

let ws = spaces
let str s = pstring s
let strci s = pstringCI s
let str_ws s = str s .>> ws
let strci_ws s = strci s .>> ws

// Now here's the cool parser stuff:

type 'T List =
    | Cons of 'T * 'T List
    | Nil


let test1 = Cons(5, Cons(6, Cons(7, Nil)))

type ASTNode =
    | NTrue
    | NFalse
    | NNull
    | NSymbol of string
    | NString of string
    | NNumeric of float
    | NList of ASTNode list
    | NQuoted of ASTNode

let (node, nodeRef) = createParserForwardedToRef()

let ntrue = str_ws "#t" |>> (fun _ -> NTrue)

let nfalse = str_ws "#f" |>> (fun _ -> NFalse)

let nnumeric = pfloat .>> ws |>> NNumeric

let nstring =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape = function | 'n' -> '\n' | 'r' -> '\r' | 't' -> '\t' | c -> c
    let escapedChar = str "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (str "\"") (str_ws "\"")
            (manyChars (normalChar <|> escapedChar)) |>> NString

let nliteral = ntrue <|> nfalse <|> nnumeric <|> nstring

let nsymbolstring =
   let validChars = letter <|> digit <|> anyOf ['-'; '_'; '+'; '/'; '*'; '%'; '!'; '=']
   (many1Chars validChars) .>> ws

let nsymbol = nsymbolstring |>> NSymbol

let nlist = many node |>> NList

let nquoted = str_ws "`" >>. node |>> NQuoted

let arglist = (str_ws "(" >>. many nsymbolstring .>> str_ws ")")

let combination = str_ws "(" >>. (nlist) .>> str_ws ")"

do nodeRef := combination <|> nliteral <|> nsymbol <|> nquoted

let parser = ws >>. many node .>> eof

let rec private parseTree node : SExpr =
    match node with
    | NTrue -> STrue
    | NFalse -> SFalse
    | NNull -> SNull
    | NSymbol(s) -> SSymbol(s)
    | NString(s) -> SString(s)
    | NNumeric(n) -> SNumeric(n)
    | NList(nodes) -> 
        match nodes with
        | NSymbol("if")::condExpr::thenExpr::elseExpr::[] -> 
            SIf(parseTree condExpr, parseTree thenExpr, parseTree elseExpr)
        | NSymbol("lambda")::NList(argsList)::firstBody::moreBodys -> 
            SLambda(argsList |> List.map (fun (NSymbol(s))->s), List.map parseTree (firstBody::moreBodys))
        | NSymbol("set!")::NSymbol(binding)::newValue::[] -> 
            SAssign(binding, parseTree newValue)
        | NSymbol("define")::NList(symAndArgsList)::bodyExprs -> 
            SDefineFunc(symAndArgsList |> List.map (fun (NSymbol(s))->s), List.map parseTree bodyExprs)
        | NSymbol("define")::NSymbol(name)::bodyExpr::[] -> 
            SDefine(name, parseTree bodyExpr)
        | NSymbol("let")::((NList(bindings))::firstExpr::rest) -> 
            SLet(bindings |> List.map (function | (NList(NSymbol(name)::value::[])) -> (name, parseTree value)), List.map parseTree (firstExpr::rest))
        | nodes -> SList(List.map parseTree nodes)
    | NQuoted(q) -> SQuoted(parseTree q)

let ParseSchemeStringToExpressionTree source =
    match run parser source with
    | Success(res,_,_) -> List.map parseTree res
    | Failure (msg,_,_) -> failwith (sprintf "Couldn't parse source: %s" msg)





