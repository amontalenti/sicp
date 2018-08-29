#lang scheme

; defining our own cons, car, and cdr

(define (cons-2 x y) (lambda (m) (m x y)))
(define (car-2 p) (p (lambda (x y) x)))
(define (cdr-2 p) (p (lambda (x y) y)))

(car-2 (cons-2 1 2))
(cdr-2 (cons-2 1 2))

(define (identity x) x)

; Boolean logic using lambda-calculus

(define (INC x) (lambda (x) (+ x 1)))

(define (TRUE x) (lambda (y) x))
(define (FALSE x) (lambda (y) y))

(define (NOT x) ((x FALSE) TRUE))
(define (AND x) (lambda (y) ((x y) FALSE)))
(define (OR x) (lambda (y) ((x TRUE) y)))

(define (ZERO f) (lambda (x) x))
(define (ONE f) (lambda (x) (f x)))
(define (TWO f) (SUCC ONE))
(define (THREE f) (SUCC TWO))

(define (SUCC n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define (PHI p)
  ((CONS (SUCC (CAR p))) (CAR p)))
(define (PRED n) (CDR ((n PHI) ((CONS ZERO) ZERO))))

; Test.  Test against zero

(define (ADD a) (lambda (b) ((a SUCC) b)))
(define (MUL a) (lambda (b) (lambda (f) (a (b f)))))

(define (CONS x) (lambda (y) (lambda (m) ((m x) y))))
(define (CAR p) (p TRUE))
(define (CDR p) (p FALSE))

(define (Z x) (((x FALSE) NOT) FALSE))

(define (SUM-N n) (((Z n) ZERO) ((ADD n) (SUM-N (PRED n)))))

(define R (lambda (f) (lambda (n) (((Z n) ZERO) ((ADD n) (f (PRED n)))))))

(define (Y f)
  ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))

((THREE INC) 0)

;;;

(define (attach-tag tag contents)
  (cons tag contents))

(define (type-tag item)
  (car item))

(define (contents item)
  (cdr item))

(define (make-box x y w h)
  (attach-tag 'box (cons (cons x y) (cons w h))))

(define (width box)
  (car (cdr (contents box))))

(define (height box)
  (cdr (cdr (contents box))))

(define (area box)
  (* (width box) (height box)))

; stubbed out
(define bob-box? true)
(define alice-box? true)
(define bob-area true)
(define alice-area true)

; dispatch to implementation
(define (gen-area box)
  (cond ((bob-box? box) (bob-area box))
        ((alice-box? box) (alice-area box))))

(contents (make-box 2 2 2 2))
(area (make-box 2 2 2 2))

(define table (make-hash))

(hash-set! table 'alice make-box)
(hash-set! table 'bob make-box)
((hash-ref table 'alice) 2 2 2 2)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds"))))

(new-withdraw 50)
(new-withdraw 50)

; evil function
(define f
  (let ((state 0))
    (lambda (x)
        (begin
          (set! state (+ state 1))
          (+ state x)
         ))))

(+ (f 0) (f 1))
(+ (f 0) (f 1))