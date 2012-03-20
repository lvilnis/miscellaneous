// Learn more about F# at http://fsharp.net

open System
open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let ws = spaces

let str_ws s = (pstring s .>> ws)
let str s = pstring s

let float_ws t = (pfloat .>> ws) t
//let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"

let identifierString s =
    let startChar = letter
    let interiorChars = manySatisfy Char.IsLetterOrDigit
    (startChar .>>. interiorChars .>> ws |>> (fun(f,r)->r.Insert(0, f.ToString()))) s

let stringLiteral s =
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"' && c <> '\'')
    let escapedChar =  pstring "\\" >>. (anyOf "\\nrt\"'" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c   -> string c)
    between ((pstring "\"") <|> (pstring "'")) ((pstring "\"") <|> (pstring "'")) (stringsSepBy normalCharSnippet escapedChar) s

//
//let rec json input =  (str_ws "{" >>. key .>> str_ws ":" >>. (floatLiteral <|> json) .>> str_ws ";") input

let listBetweenStrings beginString endString item map =
    between (str_ws beginString) (str endString)
            (sepBy (item .>> ws) (str_ws ",") .>> ws) |>> map

type JSExpression =
    | JsonExpr of Json
    | BinOpExpr of JSBinaryOps
    | UnOpExpr of JSUnaryOps
    | Identifier of string
    | DotExpr of JSExpression * JSExpression
    | IndexExpr of JSExpression * JSExpression
    | FunctionExpr of (JSExpression list) * JSExpression
    | FunctionCallExpr of JSExpression*(JSExpression list)
    | FunCallPost of JSExpression list
    | IndexPost of JSExpression

and Json = 
    | JString of string
    | JNumber of float
    | JBool   of bool
    | JNull
    | JList  of JSExpression list
    | JDict of Map<string, JSExpression>

and JSBinaryOps =
    | JMult of JSExpression*JSExpression
    | JDiv of JSExpression*JSExpression
    | JAdd of JSExpression*JSExpression
    | JSub of JSExpression*JSExpression
    | JEq of JSExpression*JSExpression
    | JEqq of JSExpression*JSExpression
    | JEqqq of JSExpression*JSExpression
    | JLt of JSExpression*JSExpression
    | JGt of JSExpression*JSExpression
    | JLte of JSExpression*JSExpression
    | JGte of JSExpression*JSExpression
    | JNeqq of JSExpression*JSExpression
    | JNeqqq of JSExpression*JSExpression

and JSUnaryOps =
    | JNegate of JSExpression
    | JNot of JSExpression

let (jvalue, jvalueRef) : Parser<_,_> * Parser<_,_> ref =
    let (jv,jvr) = createParserForwardedToRef<JSExpression, _>()
    (jv,jvr)

let (jexpr, jexprRef) : Parser<_,_> * Parser<_,_> ref =
    let (jv,jvr) = createParserForwardedToRef<JSExpression, _>()
    (jv,jvr)

let jstring s = (stringLiteral |>> (JString >> JsonExpr)) s

let jnumber s = (pfloat .>> ws |>> (JNumber >> JsonExpr)) s 

let jbool s = ((stringReturn "true"  (JsonExpr (JBool true)))
           <|> (stringReturn "false" (JsonExpr (JBool false)))) s

let jnull = stringReturn "null" (JsonExpr JNull)

let jlist s =  listBetweenStrings "[" "]" jexpr (JList >> JsonExpr) s

let jident s = (identifierString |>> Identifier) s

let strkeyvaluepair s = ((jstring |>> (fun (JsonExpr(JString(s))) -> s)) .>>. (ws >>. str_ws ":" >>. jexpr)) s
let idkeyvaluepair s = ((jident |>> (fun (Identifier(s)) -> s)) .>>. (ws >>. str_ws ":" >>. jexpr)) s

let jdict s =  listBetweenStrings "{" "}" strkeyvaluepair (Map.ofList >> JDict >> JsonExpr) s

let jfunc s = null

let indexPost = between (str_ws "[") (str_ws "]")
                        (jexpr |>> IndexPost)
let fcall = between (str_ws "(") (str_ws ")")
                    (sepBy jexpr (str_ws ",") |>> FunCallPost)

let jconstant = jnull <|> jstring <|> jnumber <|> jbool

let prim = jident <|> jconstant <|> (str_ws "(" >>. jexpr .>> str_ws ")")
let post = indexPost <|> fcall

let indexOrCallExpr = 
    let rec consumePosts pre posts =
        match posts with
        | IndexPost(p)::tl -> consumePosts (IndexExpr(pre,p)) tl
        | FunCallPost(p)::tl -> consumePosts (FunctionCallExpr(pre,p)) tl
        | [] -> pre
        | _ -> failwith "unknown postfix expr!!!"
    pipe2 prim (many post) consumePosts

//let jobject s =  listBetweenStrings "{" "}" idkeyvaluepair (Map.ofList >> JObj >> JsonExpr) s

//
//let jdot s = ((jexpr .>>. (ws >>. str_ws "." >>. jident)) |>> (fun (left, Identifier(rt)) -> DotExpr(left, rt)))s
// 
//
//let jdot s = ((jbool <|> jstring <|> jident) .>>. (str_ws "." >>. jident) 
//    |>> (fun (left, Identifier(rt)) -> DotExpr(left, rt))  ) s

let toOperator ctor = fun a b -> BinOpExpr(ctor(a,b))

let opp = new OperatorPrecedenceParser<JSExpression,unit,unit>()
let expr = opp.ExpressionParser
opp.TermParser <- jvalue <|> between (str_ws "(") (str_ws ")") expr


opp.AddOperator(InfixOperator("+", ws, 3, Associativity.Left, toOperator JAdd))
opp.AddOperator(InfixOperator("-", ws, 3, Associativity.Left, toOperator JSub))
opp.AddOperator(InfixOperator("*", ws, 4, Associativity.Left, toOperator JMult))
opp.AddOperator(InfixOperator("/", ws, 4, Associativity.Left, toOperator JDiv))

opp.AddOperator(InfixOperator("==", ws, 2, Associativity.Left, toOperator JEqq))
opp.AddOperator(InfixOperator("===", ws, 2, Associativity.Left, toOperator JEqqq))
opp.AddOperator(InfixOperator("<", ws, 2, Associativity.Left, toOperator JLt))
opp.AddOperator(InfixOperator(">", ws, 2, Associativity.Left, toOperator JGt))
opp.AddOperator(InfixOperator("<=", ws, 2, Associativity.Left, toOperator JLte))
opp.AddOperator(InfixOperator(">=", ws, 2, Associativity.Left, toOperator JGte))
opp.AddOperator(InfixOperator("!=", ws, 2, Associativity.Left, toOperator JNeqq))
opp.AddOperator(InfixOperator("!==", ws, 2, Associativity.Left, toOperator JNeqqq))

opp.AddOperator(InfixOperator("=", ws, 1, Associativity.Left, toOperator JEq))

opp.AddOperator(PrefixOperator("-", ws, 5, true, JNegate >> UnOpExpr))
opp.AddOperator(PrefixOperator("!", ws, 5, true, JNot >> UnOpExpr))

opp.AddOperator(InfixOperator(".", ws, 6, Associativity.Left, (fun l r -> DotExpr(l,r))))

do jvalueRef := (fun s -> (choice [ indexOrCallExpr; jbool; jnull; jdict; jlist; jstring; jnumber;  jident; ]) s)
do jexprRef := expr

let testJsonParser s = test jexpr s

let test1 = testJsonParser "{ 'asd':  567.9, 'hello!!' : [{'key':'value'}, [[]], 123.6] }"
let test2 = testJsonParser "asd + (123 + [1,2,3,4,5]*79) + ({'foo':foo,'bar':[1,2,3,{'baz':baz}]})"
let test3 = testJsonParser "asd !== (1 + (2 * (4 / (67.8 + l + [1,2,asdf,gh,k]))))"
let test4 = testJsonParser "foo.bar.baz + qux"
let test5 = testJsonParser "(foo + bar).baz + qux"
let test6 = testJsonParser "asd(1,4) + qux"
let test7 = testJsonParser "function (x, y) { x + y }"
let test8 = testJsonParser "(function (x, y) { x * (y+'foo') })({'bar':123}, [1,2,3,4])"

let input = System.Console.ReadLine()

test jexpr input

System.Console.ReadKey() |> ignore