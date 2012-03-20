module FScheme.Tests

open FScheme.Types
open FParsec

let test1 = "(foo bar #t)"
let test2 = "(foo (foo (foo 123 baz) 3) ((foo bar 12) bar baz))"
let test3 = "(+ 1 (- (/ 5 3) (* 6 5)))"
let test4 = "`((lambda (x y) (+ x y)) \"hello, world\" 12 345 (1 2 3 4))"

let evalTest1 = "(let ((x 1)) (let ((x \"foo\")) x))"
let evalTest2 = "(- 10000 (* 87 (* 9 (+ 8 5))))"
let evalTest3 = "(((lambda (z) (lambda (x y) (+ (+ x y) z))) 6) 12 13)"
let evalTest4 = "
(define x 1)

(define foo
    (lambda (x y z)
        (+ (+ x y) z)))

(define bar
    (lambda ()
        (set! x (+ x 1))))

(foo (bar) (bar) (bar))
"
let evalTest5 = "
(let ((x 1) (y 2))
     (set! x y)
     (set! y (+ x 5))
     (set! x (- y 2))
     (+ x y))
"
let evalTest6 = "
(define add +)

(define (addprime x y)
    (+ x y))

(define (main)
    (add 1 2 3 4 5)
    (addprime 1 2)
    (let 
        ((x 1)
         (y 2)
         (z 3))
         (set! y z)
         `(set! y 109)
         (cons x (cons y (cons z ())))))

(add 1 2 3 4 5)
(addprime 1 2)

(main)
"
let evalTest7 = "
(define (main)
    (define x 12)
    (define (foo x y)
        (- (+ x y) 5))
    (let ((y (foo x x)))
        (* y x)))
(main)
"
let evalTest8 = "
(define factorial
    (lambda (n)
        (if (= n 0)
            1
            (* n (factorial (- n 1))))))
(factorial 6)
"

let evalTest95 = "
(define flip
    (lambda (f)
        (lambda (a b)
            (f b a))))
((flip /) 3 1)
"

let evalTest9 = "
(define (test1) 
    (+ 1 (* 9 8)))

(define (test2)
    (- 10000 (* 87 (* 9 (+ 8 5)))))
(define (test3) 
    (((lambda (z) (lambda (x y) (+ x y z))) 6) 12 13))

(define (test4)
    (define x 1)

    (define foo
        (lambda (x y z)
            (+ x y z)))

    (define bar
        (lambda ()
            (set! x (+ x 1))
            x))

    (foo (bar) (bar) (bar)))

(define (test5)
    (let ((x 1) (y 2))
        (set! x y)
        (set! y (+ x 5))
        (set! x (- y 2))
        (+ x y)))
        
(define (test6)
    (define add +)

    (define (addprime x y)
        (+ x y))

    (define (main)
        (add 1 2 3 4 5)
        (addprime 1 2)
        (let 
            ((x 1)
            (y 2)
            (z 3))
            (set! y z)
            `(set! y 109)
            (cons x (cons y (cons z ())))))

    (add 1 2 3 4 5)
    (addprime 1 2)

    (main))

(define (test7)
    (define (main)
        (define x 12)
        (define (foo x y)
            (- (+ x y) 5))
        (let ((y (foo x x)))
            (* y x)))
    (main))

(define (test8)
    (define factorial
        (lambda (n)
            (if (= n 0)
                1
                (* n (factorial (- n 1))))))
    (factorial 6))

(define (result)
    (define test1Results (list \"Test 1\" (= 73 (test1))))
    (define test2Results (list \"Test 2\" (= -179 (test2))))
    (define test3Results (list \"Test 3\" (= 31 (test3))))
    (define test4Results (list \"Test 4\" (= 9 (test4))))
    (define test5Results (list \"Test 5\" (= 12 (test5))))
    (define test6Results (list \"Test 6\" (= (list 1 3 3) (test6))))
    (define test7Results (list \"Test 7\" (= 228 (test7))))
    (define test8Results (list \"Test 8\" (= 720 (test8))))
    (define (printResult testResult)
        (define name (car testResult))
        (define result (car (cdr testResult)))
        (if result (strcat name \" succeeded\") (strcat name \" failed\")))
    (map printResult (list test1Results test2Results test3Results test4Results test5Results test6Results test7Results test8Results)))
  
(result)          
"

let compiletest1 = "(+ (- 123 42) (* 7 8))"
let compiletest2 = "((lambda (x) (+ x 123)) 12)"

let GetTestSourceCode() =
    List.fold (fun acc el -> acc+el) "" [evalTest1;evalTest2;evalTest3;evalTest4;evalTest5;evalTest6;evalTest7;evalTest8;evalTest9;evalTest95] 

let RunTests() =
    do printfn "\n===== Scheme parser tests: =====\n"
    do [test1; test2; test3; test4] 
        |> List.map (FScheme.Parser.ParseSchemeStringToExpressionTree) |> ignore 
    do printfn "\n===== Scheme interpreter tests: =====\n"
    do [evalTest1;evalTest2;evalTest3;evalTest4;evalTest5;evalTest6;evalTest7;evalTest8;evalTest9;evalTest95] 
        |> List.map FScheme.Interpreter.RunProgram
        |> List.map fst
        |> List.map (printfn "Success: %A")
    do printfn "\n===== Scheme type constraint generation tests: =====\n"
    do [evalTest1;evalTest2;evalTest3;evalTest8;evalTest95] 
        |> List.map FScheme.DamasMilner.GenerateTypeConstraintsForText
        |> List.map (printfn "Success: %A")
    do printfn "\n===== Scheme type inference tests: =====\n"
    do [evalTest1;evalTest2;evalTest3;evalTest8;evalTest95] 
        |> List.map FScheme.DamasMilner.GenerateSubstitutionForText
        |> List.map (function | Left(errmsg) -> printfn "Failure: %A" errmsg
                              | Right(succ) -> printfn "Success: %A" succ)
    do printfn "\n===== Scheme compiler tests: =====\n"
    do [compiletest1; compiletest2] 
        |> List.map FScheme.Compiler.RunProgram 
        |> List.map (printfn "%s")
    do System.Console.ReadKey() |> ignore

