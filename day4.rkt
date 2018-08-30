#lang racket

; This is sort of like a decorator in Python

(define (make-monitored f)
  (let ((ncalls 0))
    (define (mf arg)
      (cond ((eq? arg 'how-many-calls?) ncalls)
            ((eq? arg 'reset-count) (set! ncalls 0))
            (else (set! ncalls (+ ncalls 1)) (f arg))))
    mf)
  )

(define inc (lambda (x) (+ x 1)))

(define m-inc (make-monitored inc))

(define a 0)

(begin
  (set! a (m-inc a))
  (set! a (m-inc a))
  (set! a (m-inc a))
  (set! a (m-inc a))
)

(displayln (m-inc 'how-many-calls?))
(displayln (m-inc 'reset-count))
(displayln (m-inc 'how-many-calls?))


; Use a Racket "Mutable List", provided for compatibility with Scheme
(mlist 1 2 3)
(require compatibility/mlist)

; Mutable Stack, similar to my functional JavaScript example
(define (make-stack)
  (mcons 'stack null))
(define (push stack item)
  (set-mcdr! stack (mcons item (mcdr stack))))
(define (pop stack)
  (let ((item (mcar (mcdr stack))))
    (set-mcdr! stack (mcdr (mcdr stack)))
    item
    )
  )
(define s (make-stack))
(push s 1)
(push s 2)
(push s 3)
(displayln (pop s))

(define (mlast-pair x)
  (if (null? (mcdr x))
      x
      (mlast-pair (mcdr x))))

(mlast-pair (mlist 1 2 3))

(define (mappend x y)
  (if (null? x)
      y
      (mcons (mcar x) (mappend (mcdr x) y))))

(mappend (mlist 1 2 3) 2)

(define x (mlist 'a 'b))
(define y (mlist 'c 'd))
(define z (mappend x y))
(define w (mappend x y))


; these are lists with the same elements, but different memory addresses

(mcdr x)
(eq? z w) ;#f
(equal? z w) ;#t

; this is a cycle of mutable lists (ugh)

(define (make-cycle x)
  (set-mcdr! (mlast-pair x) x)
  x)

(define cycled (make-cycle (mlist 'a 'b 'c)))
cycled

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-mcdr! x y )
          (loop temp x))))
  (loop x '()))

(mystery (mlist 1 2 3))

(define (count-pairs x)
  (define seen null)
  (define (count x)
    (if (or (not (mpair? x)) (memq x seen))
        0
        (begin
          (set! seen (cons x seen))
          (+ (count (mcar x))
             (count (mcdr x))
           1)
         )
     ))
  (count x))

(define j (mcons 1 2))
(define k (mcons 3 j))

(count-pairs (mcons j k)) ; 3
(count-pairs (mcons j j)) ; 2

(define (make-q-basic)
  (define front null)
  (define rear null)
  (define q (mcons front rear))
  q
)

(define (front-ptr queue) (mcar q))
(define (rear-ptr queue) (mcdr q))
(define (set-front-ptr! q item) (set-mcar! q item))
(define (set-rear-ptr! q item) (set-mcdr! q item))
(define (empty-q? q) (null? (front-ptr q)))

(define (front-q q)
  (if (empty-q? q)
      (error "FRONT called with an empty queue" q)
      (mcar (front-ptr q))))

(define (insert-q! q item)
  (let ((new-pair (mcons item null)))
    (cond ((empty-q? q)
           (set-front-ptr! q new-pair)
           (set-rear-ptr! q new-pair)
           q)
          (else
           (set-mcdr! (rear-ptr q) new-pair)
           (set-rear-ptr! q new-pair)
           q))))

(define (delete-q! q)
  (cond ((empty-q? q)
         (error "DELETE! called with an empty queue" q))
        (else
         (set-front-ptr! q (mcdr (front-ptr q)))
         q)))

(define q (make-q-basic))
(insert-q! q 'a)
(insert-q! q 'b)
(delete-q! q)

; Object-oriented progrmaming... with Scheme and home-grown dynamic dispatch

(define (make-q-obj)
 (let ((front-ptr null)
       (rear-ptr null))
   (define (empty?)
     (null? front-ptr))
   (define (insert! item)
     (let ((new-entry (mcons item null)))
       (if (empty?)
           (begin
             (set! front-ptr new-entry)
             (set! rear-ptr new-entry)
           )
           (begin
             (set-mcdr! rear-ptr new-entry)
             (set! rear-ptr new-entry)
           ))))
   (define (dispatch m . args)
     ; dispatch table for methods
    (cond  [(eq? m 'insert!) (apply insert! args)]
           [(eq? m 'empty?) (empty?)]
           [(eq? m 'front) front-ptr]
           [(eq? m 'rear) rear-ptr]
           [(eq? m 'repr) (displayln front-ptr)]))
   dispatch))

(define qobj (make-q-obj))
(qobj 'insert! 1)
(qobj 'insert! 2)
(qobj 'insert! 3)
(qobj 'repr)
(qobj 'empty?)
(qobj 'front)
(qobj 'rear)
(qobj 'repr)