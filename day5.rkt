#lang racket

; our own Scheme interpreter... in Scheme

; Environment is a list of hash tables
(define env (cons (make-hash) null))

(define (lookup-name name)
  (define (lookup hashes name)
    (if (null? hashes)
        (error "KeyError:" name)
        (if (hash-has-key? (car hashes) name)
            (hash-ref (car hashes) name)
            (lookup (cdr hashes) name))))
  (lookup env name))

(define (new-env oldenv)
  (cons (make-hash) oldenv))

(define (set-name! name value) (hash-set! (car env) name value))

(define (hash-update! h keys values)
  ; I felt like a functional programming zen master when I wrote this
  (apply (curry hash-set*! h) (flatten (map list keys values)))
  void)

(define (if? expr)
  (and (pair? expr) (eq? (car expr) 'if)))

(define (eval-if expr)
  (define (eval-predicate)
    (my-eval (cadr expr)))
  (define (eval-true)
    (my-eval (caddr expr)))
  (define (eval-false)
    (my-eval (cadddr expr)))
  (if (eval-predicate) (eval-true) (eval-false)))

(define (my-eval expr)
   (cond ((primitive? expr) expr)
         ((if? expr) (eval-if expr))
         ((define? expr) (define-symbol expr))
         ((symbol? expr) (lookup-name expr))
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
        [proc-args (cdr spec)]
        [define-scope env])
    (set-name! proc-name (cons 'udf (lambda args
       (let ([old-env env]
             ; create nested scope
             [function-scope (make-hash)])
         ; update local bindings
         (hash-update! function-scope proc-args args)
         (set! env (cons function-scope define-scope))
         ; evaluate result
         (let ([result (my-eval body)])
           ; restore scope
           (set! env old-env)
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

; built-ins and constants
(set-name! 'true #t)
(set-name! 'false #f)
(set-name! 'pi 3.14)
(set-name! '+ (cons 'builtin +))
(set-name! '- (cons 'builtin -))
(set-name! '/ (cons 'builtin /))
(set-name! '* (cons 'builtin *))
(set-name! '<= (cons 'builtin <=))
(set-name! '>= (cons 'builtin >=))
(set-name! '> (cons 'builtin >))
(set-name! '< (cons 'builtin <))

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
(my-eval '(define (fib n)
            (if (<= n 2)
                1
                (+ (fib (- n 1)) (fib (- n 2))))))
(my-eval '(fib 10))
