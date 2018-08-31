#lang racket

; our own Scheme interpreter... in Scheme

(define envs (cons (make-hash) null))

(define env (car envs))

(define (child-env!)
   (set! envs (cons envs (make-hash))))

(define (parent-env!)
  (set! envs (car envs)))

(define (update-env! keys values)
  ; I felt like a functional programming zen master when I wrote this
  (apply (curry hash-set*! env) (flatten (map list keys values))))

(define (my-eval expr)
   (cond ((primitive? expr) expr)
         ((symbol? expr) (lookup-name expr))
         ((define? expr) (define-symbol expr))
         ((pair? expr) (call-procedure expr))
         (else (error "Bad expression:" expr)))
  )

(define (primitive? expr)
  (or (number? expr) (string? expr) (boolean? expr) (null? expr)))

(define (define? expr)
  (and (pair? expr) (eq? (car expr) 'define)))

(define (define-variable expr)
  (let ([def (car expr)]
        [name (cadr expr)]
        [body (caddr expr)])
    (set-name! name body)
    ))

(define (define-function expr)
  (let ([def (car expr)]
        [spec (cadr expr)]
        [body (caddr expr)])
    (if (symbol? spec)
        ; simple varargs function definition
        (define-simple-function spec body)
        ; argument definition, but we're throwing away args right now
        (define-complex-function spec body))
    ))

(define (define-simple-function spec body)
  (set-name! (car spec) (cons 'udf body)))

(define (define-complex-function spec body)
  ; this is the most interesting piece of the interpreter
  ; notice how it differs from interpret.py, where we
  ; implement the 'lambda' form using a nested Python function
  (let ([proc-name (car spec)]
        [proc-args (cdr spec)])
    (hash-set! env proc-name (cons 'udf (lambda args
       (begin
         ; create nested scope
         (child-env!)
         ; update local bindings
         (update-env! proc-args args)
         ; evaluate result
         (let ([result (my-eval body)])
           ; restore scope
           (parent-env!)
           result)
    ))))))

(define (define-symbol expr)
  (let ([def (car expr)]
        [type (cadr expr)])
    (if (symbol? type)
        (define-variable expr)
        (define-function expr))))

(define (call-procedure expr)
  (let ([name (get-procedure-name expr)]
        [args (get-procedure-args expr)])
            (evaluate-proc name args)))

(define (evaluate-proc name args)
  (let ([proc (my-eval name)]
        [proc-args (map my-eval args)])
            (my-apply proc proc-args)))

(define (get-procedure-name expr)
  (let ([name (car expr)])
    name))

(define (get-procedure-args expr)
  (let ([args (cdr expr)])
    args))

(define (lookup-name name)
  (chain-lookup name envs))

(define (chain-lookup name envs)
  (hash-ref env name))

(define (set-name! name value)
  (hash-set! env name value))

; built-ins and constants
(set-name! 'true #t)
(set-name! 'false #f)
(set-name! 'pi 3.14)
(set-name! '+ (cons 'builtin +))
(set-name! '- (cons 'builtin -))
(set-name! '/ (cons 'builtin /))
(set-name! '* (cons 'builtin *))

(define (udf-apply fn args)
  (apply fn args))

(define (my-apply proc args)
  (let ([tag (car proc)]
        [fn (cdr proc)])
    (cond ([eq? tag 'builtin] (apply fn args))
          ([eq? tag 'udf] (udf-apply fn args))
          (else (error "else for my-apply")))))

(my-eval '(+ 40 2)) ; 42
(my-eval '(define meaning 42)) ; modify environment

; this is terrible and leaks all UDFs local variables into the global scope!
; that's because we haven't finished our implementation of `envs` yet, and
; I needed somewhere to stuff the locals
(my-eval '(define (square x) (* x x))) ; make a square function
(my-eval '(square 5)) ; gives 25!
envs