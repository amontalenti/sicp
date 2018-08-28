#lang scheme

;; initial experiments

; variable definition
(define pi 3.14)

; function definition

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

; function calls
(displayln "dec/inc")
(displayln (dec (inc 0)))
(displayln "---")

; complex functions with conditions

(define (largest a b c)
  (cond ((and (> a b) (> a c) (> b c)) (list a b))
        ((and (> b a) (> b c) (> c a)) (list b c))
        (else (list a c))))

(define (square x)
  (* x x))

(define (square-of-sum a b c)
  (square (apply + (largest a b c))))

;; newton's method

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

; this is how I wrote this at first, accidentally, and
; it resulted in a fun infinite recursion case
; (define (square-iter guess x)
;	(if good-enough? guess x)
;	guess
;	(square-iter (improve guess x) x))

(define (square-iter guess x)
  (if (good-enough? guess x)
    guess
    (square-iter (improve guess x) x)))

(define (sqrt x)
  (square-iter 1.0 x))

(displayln "sqrt")
(displayln (sqrt 9))
(displayln "---")

; showcase tail recursion optimization

(define (sum term a next b)
   (define (iter a result)
     (if (> a b)
         result
         (iter (next a) (+ (term a) result))))
    (iter a 0))

(displayln "sum")
(displayln
  (let ([fn (lambda (x) x)])
    (sum fn 1 inc 100)))
(displayln "---")

; showcase higher-order functions

(define (double fn)
 (lambda (x) (fn (fn x))))

(displayln "double")
(displayln (((double (double double)) inc) 5))
(displayln "---")

(define double-2x1 ((double double) (double inc)))
(define double-3x0 (((double double) double) inc))

; this is mind-bending... stare at it

(displayln "doublers")
(displayln (double-2x1 5))
(displayln (double-3x0 5))
(displayln "---")

; currying!

(define (compose f g)
  (lambda (x) (f (g x))))

(displayln "composer")
(displayln ((compose square inc) 6))
(displayln "---")

; this mind-bendingly repeatedly applies the function
; by composing it, for each value in n, between n
; and 0... and this is itself tail call optimized

(define (repeatedi f n)
  (define (iter n result-func)
    (if (= n 0)
      result-func
      (iter (- n 1) (compose f result-func))))
    (iter n (lambda (x) x)))

((repeatedi inc 0) 0)
((repeatedi inc 50) 50)

; Open question at the end of day one:
; could we "unravel" the Russian dolls?
; for example: (f (g (h (x))))... could that somehow
; become (f g h x)... and then we could iteratively
; do (f(g) => o)... and then (o(h) => p) ... (p x)
; or, are we just brain-fried at this point?