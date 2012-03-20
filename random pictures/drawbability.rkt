#lang typed/racket/no-check
(require slideshow)
(require plot)
(require (planet schematics/random:1:0/random))

(define-type (Dist a) (-> a))

(: sample-dist (All (a) ((Dist a) -> a)))
(define (sample-dist d) (d))

(: dist-return (All (a) (a -> (Dist a)))) 
(define (dist-return val) (λ () val))

(: dist-bind (All (a b) ((Dist a) (a -> (Dist b)) -> (Dist b))))
(define (dist-bind d func)
  (func (sample-dist d)))

(: dist-fmap (All (a b) ((a -> b) -> (Dist a) -> (Dist b))))
(define ((dist-fmap func) d)
  (dist-bind d (λ (x) (dist-return (func x)))))

(: uniform (Dist Real))
(define uniform random)


(: dist-fmap* (All (c a b ...) ((a b ... b -> c) -> (Dist a) (Dist b) ... b -> (Dist c))))
(define (dist-fmap* f)
  (λ (da . dbs) (λ () (apply f (sample-dist da) (map sample-dist dbs)))))


(define ^ dist-fmap*)
(define p dist-return)

(: sample-population (All (a) ((Dist a) Natural -> (Dist (Listof a)))))
(define (sample-population d sample-size)
  (lift-list (repeat d sample-size))) 

(: 5th-order-statistic (Dist Real))
(define 5th-order-statistic 
  ((^ min) uniform uniform uniform uniform uniform))

(: nth-order-statistic (Natural -> (Dist Real)))
(define (nth-order-statistic n)
  (: min-list ((Listof Real) -> Real))
  (define (min-list xs) (apply min xs))
  ((^ min-list) (sample-population uniform n)))

(: repeat (All (a) (a Natural -> (Listof a))))
(define (repeat element num-times)
  (if (= num-times 0)
       null
      (cons element (repeat element (- num-times 1)))))
      
(: samples-dist (All (a) ((Dist a) Natural -> (Dist (Listof a)))))
(define (samples-dist d num-times)
  ((^ (inst repeat a)) d (p num-times)))      

(: samples-dists (All (a) ((Dist a) Natural -> (Dist (Listof a)))))
(define (samples-dists d num-times)
  (lift-list (repeat d num-times)))  

(define samples-ndists (inst samples-dists Real))

(: mean ((Listof Real) -> Real))
(define (mean xs)
  (/ (foldl + 0 xs) (length xs)))

(: variance ((Listof Real) -> Real))
(define (variance vec)
  (let* ((mu (mean vec))
         (diffs (map (λ: ([el : Real]) (- el mu)) vec))
         (sq-diffs (map (λ: ([el : Real]) (* el el)) diffs)))
    (/ (foldl + 0 sq-diffs) (length vec))))

(: sigma ((Listof Real) -> Real))
(define (sigma vec) 
  (define s (sqrt (variance vec)))
  (if (real? s) s 0))

(: sample-mean ((Dist Real) Natural -> (Dist Real)))
(define (sample-mean d num)
  ((^ mean) (samples-ndists d num)))

(: sample-sigma ((Dist Real) Natural -> (Dist Real)))
(define (sample-sigma d num)
  ((^ sigma) (samples-ndists d num)))

(define awesome ((^ plot) ((^ line) (p (λ (x) (uniform))))))

(define awesome2 (((^ plot) ((^ line) ((^ Gaussian-PDF) uniform uniform)))))

(define awesome3 ((^ cc-superimpose) ((^ circle) (p 10)) ((^ circle) (p 50))))

(define awesome4 (apply (^ cc-superimpose) (repeat ((^ circle) ((^ *) (p 100) uniform)) 10)))

(define awesome5 (apply (^ cc-superimpose) (repeat ((^ circle) ((^ +) (p 150) ((^ *) (p 50) random-gaussian))) 100)))

(define awesome6 ((mu random-gaussian 1000)))
(define awesome7 ((dsigma random-gaussian 1000)))

(define awesome8 ((^ Gaussian->PDF) ((^ cons) (mu random-gaussian 1000) (dsigma random-gaussian 1000))))
(define awesome9 (((^ plot) ((^ line) ((^ Gaussian->PDF) ((^ cons) (mu random-gaussian 1000) (dsigma random-gaussian 1000)))))))

(define awesome10 (apply (^ cc-superimpose) (repeat ((^ circle) ((^ abs) ((^ *) (p 100) random-gaussian))) 100)))
