#lang scheme

; data abstraction with car/cdr and cons cells

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

; writing a rational normalizer for positive/negative

(define (normalize rat)
   ; numerator
   (cond
     ; EITHER positive numerator OR (negative numerator AND negative denomator)
     ((or (> 0 (numer rat)) (and (< 0 (numer rat)) (< 0 (denom rat))))
     ; force positive
         (make-rat (abs (numer rat)) (abs (denom rat))))
     ; either numerator or demominator is negative
     ((or (< (numer rat) 0) (< (denom rat) 0))
         (make-rat (- 0 (abs (numer rat))) (abs (denom rat))))
     ; is there an else? leave alone
     (else
         (displayln "weird rational?")
         (make-rat (numer rat) (denom rat)))))

(define (normal-rat numer denom) (normalize (make-rat numer denom)))
           
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

; print a rational number

(print-rat (make-rat 2 3))
(print-rat (normal-rat -2 -3))
(print-rat (normal-rat 2 3))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(add-rat (make-rat 1 2) (make-rat 1 2))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(sub-rat (make-rat 1 2) (make-rat 1 2))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(mul-rat (make-rat 1 2) (make-rat 1 2))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(div-rat (make-rat 4 1) (make-rat 1 4))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(equal-rat? (make-rat 1 2) (make-rat 1 2))
(equal-rat? (make-rat 1 4) (make-rat 2 8))

(define (numer-1 x)
  (let ((g (gcd (car x) (cdr x))))
  (/ (car x) g)))

(define (denom-1 x)
  (let ((g (gcd (car x) (cdr x))))
  (/ (cdr x) g)))

(define (simplify rat)
  (make-rat (numer-1 rat) (denom-1 rat)))

(simplify (make-rat 4 16))

(simplify (normal-rat -4 -16))

(define (square x) (* x x))

; this returns in reverse order; why?
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (flatten (iter items null)))

(define (for-each fn items)
  (map fn items)
  (void))

(square-list (list 1 2 3))

(define (accumulate op initial sequence)
  (void))

; still need to implement accumulate for these using
; the tail recursive (iterative) option

(define (map-2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (append-2 seq1 seq2)
  (accumulate cons (void) (void) ))

(define (length-2 sequence)
  (accumulate (void) 0 sequence))

; exercise 2.54, implementing recursive equality

(define (equal-deep lhs rhs)
  (if (or (and (symbol? lhs) (symbol? rhs)) (and (null? lhs) (null? rhs)))
      (eq? lhs rhs)
      (and (equal-deep (car lhs) (car rhs)) (equal-deep (cdr lhs) (cdr rhs)))))

(define (equal-2 lhs rhs)
  (if (= (length lhs) (length rhs))
      (equal-deep lhs rhs)
      false))

(equal-2 '(x y z) '(x y z)) ; that works!

(and (equal? (list 1 2 3) (quote (1 2 3))) (equal? '(1 2 3) (quote (1 2 3)))) ; #t

(and (equal? (list 1 'x 3) (quote (1 x 3))) (equal? '(1 x 3) (quote (1 x 3)))) ; #t

