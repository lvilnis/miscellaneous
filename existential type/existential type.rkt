#lang typed/racket

;; we wish to encode the following abstract data type:
;; (Exists (a) (List a (a -> Integer)))

(: id (All (a) (a -> a)))
(define (id x) x)

;;(define: xy : (All (Y) ((All (a) ((List a (a -> Integer)) -> Y)) -> Y)) 
;;  (plambda: (Y) ([f : (All (a) ((List a (a -> Integer)) -> Y))]) 
;;            ((inst f Integer) `(,3 ,id))))


;; this one doesn't work since the types get expanded before macro can be applied
(define-syntax Exists
  (syntax-rules ()
    ((Exists (X) T)
     (All (Y) ((All (X) (T -> Y)) -> Y)))))

(define-syntax define-existential:
  (syntax-rules ()
    ((define-existential: id _ (Exists (X) T) representation-type body ...)
     (define: id : (All (Y) ((All (X) (T -> Y)) -> Y)) 
       (plambda: (Y) ([f : (All (X) (T -> Y))]) 
                 ((inst f representation-type) (begin body ...)))))))

;; add match existential form to be easiest way of unpacking...
;; (match xy [(exists (X) `(,state ,to-int)) (to-int state)])

(define-syntax unpack-existential:
  (syntax-rules ()
    ((unpack-existential: : return-type id [type-variable] [term _ term-type] body ...)
     (#{ id @ return-type }
      #{ (plambda: (type-variable) 
                   ([arg : term-type]) 
                   (begin 
                     (match-define term arg)
                     body ...)) 
         :: (All (type-variable) (term-type -> return-type)) }))))

(define-existential: xy : (Exists (a) (List a (a -> Integer))) 
  Integer `(,3 ,id))

(: bool-to-int (Boolean -> Integer))
(define (bool-to-int b) (if b 1 0))

(define-existential: xy2 : (Exists (a) (List a (a -> Integer))) 
  Boolean `(,#t ,bool-to-int))

(: string-to-int (String -> Integer))
(define (string-to-int s) (string-length s))

(define-existential: xy3 : (Exists (a) (List a (a -> Integer)))
  String `(,"hello" ,string-to-int))

(define (test)
  (unpack-existential: : Integer
    xy
    [b]
    [`(,state ,to-int) : (List b (b -> Integer))]
    (to-int state)))

(: unpack-and-get-int ((All (Y) ((All (a) ((List a (a -> Integer)) -> Y)) -> Y)) -> Integer))
(define (unpack-and-get-int packed)
  (unpack-existential: : Integer
    packed
    [b]
    [`(,state ,to-int) : (List b (b -> Integer))]
    (to-int state)))


;; looks like inst-ing xy causes the quantifier
;; to be renamed. this shouldnt matter...


;; note that we need the "inst Integer" part, so there has to be some inference
;; step to figure out the hidden type at first....
;; ok now i got it working....


;; and after rebuild its broken. it appears to depend on me first getting it working with
;; lambdas, mispelling "term" as "val", and then it just magic-tizes and works.

;; this means (All (Y) ((All (a) ((List a (a -> Integer)) -> Y)) -> Y)


;; we need a package type:
;; (Exists (X) T) = (All (Y) ((All (X) (T -> Y)) -> Y)


;; we need a "pack" function
;; (: pack (All (S) ((All (X)))))