# Scheme Interpreter Project
#
# This file contains various tests for the Scheme interpreter.  The
# variable PROJECTLEVEL turns on various tests for different project
# stages.  Increase the number to enable more tests.
#
# For more complete testing, you might run this file under pytest.
#
#    python3 -m pytest test_scheme.py
#

PROJECTLEVEL = 13

from scheme import parse_expression, substitute, Interpreter, builtins

# -----------------------------------------------------------------------------
# PART 1
#
# Translate Scheme expressions (presented as a string) and convert into a 
# Python object.
#
# Reading:  1.1.1 Expressions  (pg. 1 - 7)

if PROJECTLEVEL >= 1:
    def test_parse_numbers():
        assert parse_expression('3') == 3
        assert parse_expression('3.4') == 3.4
    
    def test_parse_symbol():
        assert parse_expression('abc') == 'abc'

    def test_parse_combinations():
        assert parse_expression('(+ 3 4)') == ('+', 3, 4)
        assert parse_expression('(3)') == (3, )
        assert parse_expression('(+ (* 3 4) (* 5 6))') == ('+', ('*', 3, 4), ('*', 5, 6))

    if __name__ == '__main__':
        test_parse_numbers()
        test_parse_symbol()
        test_parse_combinations()

# ----------------------------------------------------------------------
# PART 2
#
# Substitution.  Make sure you can substitute names in a Scheme expression
# with values.
#
# Reading:  1.1.2 Naming and the Environment (p. 7-8)
#           1.1.3 Evaluating Combinations (p. 9-11)
#           1.1.4 Compound Procedures (p. 11-13)
#           1.1.5 The Substitution Model for Procedure Application (p. 13-17)

if PROJECTLEVEL >= 2:
    def test_substitute():
        assert substitute(('*', 'x', 'x'), 'x', 7) == ('*', 7, 7)
        assert substitute(('*', 'x', 'x'), 'x', ('+', 3, 4)) == \
                         ('*', ('+', 3, 4), ('+', 3, 4))

    if __name__ == '__main__':
        test_substitute()

# Further tests require the use of an interpreter object
interp = Interpreter()

# -----------------------------------------------------------------------------
# PART 3
#
# Basic evaluation of expressions and definitions of names.
#
# Reading:  1.1.1 - 1.1.4. (p. 1-17)

if PROJECTLEVEL >= 3:
    def test_numbers():
        assert interp('3') == 3
        assert interp('3.4') == 3.4
        
    def test_combinations():
        assert interp('(+ 3 4)') == 7
        assert interp('(+ (* 3 4) (* 10 20))') == 212

    def test_simple_definitions():
        interp('(define x 3)')
        interp('(define y 4)')
        assert interp('x') == 3
        assert interp('y') == 4

    def test_combinations_lookup():
        assert interp('(+ x y)') == 7
        assert interp('(+ (* x y) (+ x 10))') == 25

    def test_builtins_as_first_class():
        # Evaluating the name of a builtin should produce the builtin object
        assert interp('+') == builtins['+']

    if __name__ == '__main__':
        test_numbers()
        test_combinations()
        test_simple_definitions()
        test_combinations_lookup()
        test_builtins_as_first_class()

# ----------------------------------------------------------------------
# PART 4
#
# User-defined procedures.  You need to give your interpreter the
# ability to define and call a user-defined procedure.  There are two
# variations on the syntax:
#
# You can define a procedure using a Lambda expression:
#
#    (define square (lambda (x) (* x x)))
#
# The following is a bit of syntactic sugar that simplifes the syntax
#
#    (define (square x (* x x)))
#
# You'll need to define a Procedure object.  This object will need
# to record the following
#
#    - The argument names
#    - The body of the procedure
#    - The environment in which the procedure was defined.
#
# You'll need to modify the Interpreter class above to apply the
# procedure.  See the notes in the class for a few details.

if PROJECTLEVEL >= 4:
    from scheme import Procedure
    def test_procedure_lambda():
        # Some procedure definition tests (p. 13-14)
        interp('(define square (lambda (x) (* x x)))')
        assert isinstance(interp('square'), Procedure)

        # Procedure application
        assert interp('(square 21)') == 441
        assert interp('(square (+ 2 5))') == 49
        assert interp('(square (square 3))') == 81

    def test_procedure_alt_definition():
        # Alternative definition syntax
        interp('(define (sum-of-squares x y) (+ (square x) (square y)))')
        assert interp('(sum-of-squares 3 4)') == 25

    def test_procedure_complex():
        # A more complex example
        interp('(define (f a) (sum-of-squares (+ a 1) (* a 2)))')
        assert interp('(f 5)') == 136

    if __name__ == '__main__':
        test_procedure_lambda()
        test_procedure_alt_definition()
        test_procedure_complex()

# -----------------------------------------------------------------------------
# PART 5
#
# Conditional Expressions and Predicates.   You need to add relations
#
#    (< a b)
#    (<= a b)
#    (> a b)
#    (>= a b)
#    (= a b)
#
# In addition you need true and false symbols
#
#    true
#    false
#
# You'll also need to extend your interpreter with the following special forms
#
#    (cond (pred1 expr1)
#          (pred2 expr2)
#          ...
#          (else exprn))
#
#    (if predicate consequent alternative)
#
#    (and expr1 expr2 ... exprn)
#
#    (or expr1 expr1 ... exprn)
#
#    (not expr)
#
# Reading: 1.1.6 (p. 17 - 21)
#

if PROJECTLEVEL >= 5:
    def test_basic_relations():
        # Basic relations and booleans
        assert interp('(< 2 3)') == True
        assert interp('(<= 2 3)') == True
        assert interp('(> 3 2)') == True
        assert interp('(>= 3 2)') == True
        assert interp('(= 4 4)') == True
        assert interp('true') == True
        assert interp('false') == False

    def test_cond():
        # Tests of the cond special form.  (p. 17)
        interp('''
        (define (abs x)
            (cond ((> x 0) x)
                  ((= x 0) 0)
                  ((< x 0) (- 0 x))))
        ''')
        assert interp('(abs 3)') == 3
        assert interp('(abs -3)') == 3
        assert interp('(abs 0)') == 0

    def test_cond_else():
        # Alternative implementation (p. 18)
        interp('''
        (define (abs2 x)
             (cond ((< x 0) (- 0 x))
                   (else x)))
        ''')
        assert interp('(abs2 3)') == 3
        assert interp('(abs2 -3)') == 3

    def test_if():
        # Alternative implementation (p. 18)
        interp('''
        (define (abs3 x)
             (if (< x 0) (- 0 x) x))''')

        assert interp('(abs3 3)') == 3
        assert interp('(abs3 -3)') == 3

    def test_and():
        # Tests of and-or
        assert interp('(and true false)') == False
        assert interp('(and true true)') == True
        assert interp('(and false true)') == False
        assert interp('(and false false)') == False
        assert interp('(and true true true)') == True
        assert interp('(and)') == True

    def test_or():
        assert interp('(or true false)') == True
        assert interp('(or true true)') == True
        assert interp('(or false true)') == True
        assert interp('(or false false)') == False
        assert interp('(or false false false)') == False
        assert interp('(or)') == False

    def test_not():
        # Tests of not operator
        assert interp('(not (< 2 3))') == False
        assert interp('(not (< 3 2))') == True

    def test_short_circuit():
        # Tests of short-circuit behavior. "and" and "or" only
        # use as many arguments as are needed to determine the final 
        # outcome.  For example (and expr1 expr2) only evaluates
        # (expr2) if expr1 is True. 
        #
        # Note: in these tests, "undefined" is an undefined name. It
        # should not raise an error since the first value determines
        # the outcome
        assert interp('(and false undefined)') == False
        assert interp('(or true undefined)') == True

    def test_even_odd():
        # Give your interpreter built-in even? and odd? functions. 
        assert interp('(even? 2)') == True
        assert interp('(even? 3)') == False
        assert interp('(odd? 2)') == False
        assert interp('(odd? 3)') == True

    if __name__ == '__main__':
        test_basic_relations()
        test_cond()
        test_cond_else()
        test_if()
        test_and()
        test_or()
        test_not()
        test_short_circuit()
        test_even_odd()

# -----------------------------------------------------------------------------
# PART 6 
#
# Reading:  1.1.8 Procedures as Black-Box Abstractions
#
# You need to make sure your interpreter can support nested procedure
# definitions and nested names.  For example:
#
# (define (f x)
#      (define a (+ x 1))
#      (define b (- x 1))
#      (* a b))
#
# Or nested function definitions
#
# (define (fact n)
#     (define (fact-iter value n)
#          (if (> n 0) (fact-iter (* value n) (- n 1))
#                      value
#          )
#     )
#     (fact-iter 1 n)
# )
#
# To make this work, you're going to need to think a bit about environments
# and how they work with procedure application. You'll probably need
# some way to implement nested scopes (hint: ChainMap).  You'll also need
# to make it possible for a procedure to have multiple expressions enclosed
# inside.  This might require some minor changes to the apply() method and
# the lambda special form.

if PROJECTLEVEL >= 6:
    def test_nested_definitions():
        interp('''
    (define (outer x y)
        (define z (+ x 3))
        (define (inner x) (+ x z))
        (inner y))
    ''')
        assert interp('(outer 2 3)') == 8

    if __name__ == '__main__':
        test_nested_definitions()

# -----------------------------------------------------------------------------
# PART 7
#
# Reading: 1.3 Formulating Abstractions with Higher-Order Procedures.
#
# For this part, you need to make sure your interpreter allows procedures
# to be passed as arguments. Read p. 56-60
#
# For example:
#
#    (define (f func x y) (func x y))
#
#    (f + 2 3)  --> 5
#
# You'll also need to implement the "let" special form.  Let is
# syntactic sugar for a trick with lambda.  Read p.64-65.
#
#   (let ((var1 expr1)
#         (var2 expr2)
#         ...
#         (varn exprn))
#         body)
#
# This is the same as:
#
#  ((lambda (var1 var2 ... varn) body)
#    expr1 expr2 ... exprn)
#
# Finally, you need to allow procedures to be returned as results. Read p. 72-78.
#
#    (define (f x y) (lambda () (+ x y)))
#    (define g (f 2 3))
#    (g)  --> 5
#
# One tricky part of returning a procedure is that it must capture the complete
# environment in which it was defined.  So, in the above example, the
# procedure returned by "f" captures the values of "x" and "y" which are
# held in an environment attached the procedure.  This is commonly known as a
# "closure" although SICP doesn't really use this terminology.   To make this
# work, you'll need to store the environment as part of each procedure.

if PROJECTLEVEL >= 7:
    def test_procedure_argument():
        # Functions as first-class arguments
        interp('''
    (define sum (lambda (term a next b)
         (if (> a b)
             0
             (+ (term a)
                (sum term (next a) next b)))))
    ''')

        interp('''
    (define (inc n) (+ n 1))
    ''')
        assert interp('(sum square 1 inc 10)') == 385

        # Lambda expressions
        assert interp('(sum square 1 (lambda (n) (+ n 1)) 10)') == 385

    def test_let():
        # Let special form
        interp('(define x 2)')
        assert interp('''
          (let ((x 3)
                (y (+ x 2)))
                (* x y))
          ''') == 12

        interp('''
    (define f (lambda (x y)
       (let ((a (+ 1 (* x y)))
             (b (- 1 y)))
          (+ (+ (* x (square a))
                (* y b))
             (* a b)))))
    ''')
        assert interp('(f 2 3)') == 78

        
    def test_procedure_return():
        # Functions as returned values
        interp('''
    (define (g x)
        (lambda (y) (+ x y)))
    ''')

        interp('(define h (g 10))')
        assert isinstance(interp('h'), Procedure)
        assert interp('(h 20)') == 30
        
    if __name__ == '__main__':
        test_procedure_argument()
        test_let()
        test_procedure_return()

# -----------------------------------------------------------------------------
# PART 8
#
# Reading:  79-98.
#
# Data abstraction.   You need to give your interpreter the ability to 
# create a Pair data structure with the cons built-in function.

if PROJECTLEVEL >= 8:

    def test_cons():
        interp('(define p (cons 2 3))')
        assert interp('(car p)') == 2
        assert interp('(cdr p)') == 3

        interp('(define n (cons (cons 2 3) (cons 4 5)))')
        assert interp('(car (car n))') == 2
        assert interp('(cdr (car n))') == 3
        assert interp('(car (cdr n))') == 4
        assert interp('(cdr (cdr n))') == 5

    def test_equality():
        # Equality test.  You need to give your interpreter an equal? operator
        # for testing if two pairs have the same value. Like Python ==
        interp('(define p (cons 2 3))')
        interp('(define q (cons 2 3))')
        assert interp('(equal? p q)') == True

        # Identity test.  Test if two pairs are exactly the same pair using eq?
        # Similar to Python "is" operator
        assert interp('(eq? p p)') == True
        assert interp('(eq? p q)') == False

    def test_car_cdr():
        # Optional features.  Various combinations of car/cdr are often 
        # supported by short-cut functions.  For example:
        # 
        # (car (cdr n))   ->  (cadr n)

        interp('(define n (cons (cons 2 3) (cons 4 5)))')
        assert interp('(caar n)') == 2
        assert interp('(cdar n)') == 3
        assert interp('(cadr n)') == 4
        assert interp('(cddr n)') == 5

    if __name__ == '__main__':
        test_cons()
        test_equality()
        test_car_cdr()

# -----------------------------------------------------------------------------
# PART 9
#
# Reading: 2.2.1 Representing Sequences (p. 99 - 107)
#          2.2.3 Sequences as conventional interfaces (p. 113-118)
#
# In this part, you give your interpreter the ability to work with sequences.
# This includes common sequence operations such as map, filter, and reduce.
#
# First, you need a way to construct a list.  This is done via nested cons cells.
#
#   (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil)))))   -> [1, 2, 3, 4, 5]
#
# The "nil" symbol is like Python's None.  It represents an empty list.
#
# To make the syntax more sane, define a "list" special form for creating
# a list:
#
#   (list 1 2 3 4 5)  --> (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil)))))
#
# You'll also need a few test functions.  These are builtins
#
#   (null? x)        ; tests if x is nil
#   (pair? p)        ; tests if p is a Pair

if PROJECTLEVEL >= 9:
    def test_pair():
        interp('(define p (cons 2 3))')
        assert interp('nil') == None
        assert interp('(null? nil)') == True
        assert interp('(pair? p)') == True
        assert interp('(pair? 2)') == False

    def test_list():
        interp('(define a (list 1 2 3))')
        assert interp('(car a)') == 1
        assert interp('(car (cdr a))') == 2
        assert interp('(car (cdr (cdr a)))') == 3
        assert interp('(null? (cdr (cdr (cdr a))))') == True

        # zero-length lists
        assert interp('(list)') == None

    def test_list_builtins():
        # Give your interpreter the following functions as builtin library functions
        interp('(define a (list 1 2 3))')
        
        # Give your interpreter a length function (p. 102)
        assert interp('(length? a)') == 3

        # Give your interpreter an indexing function  (p. 101)
        assert interp('(list-ref a 1)') == 2

        # Give your interpreter an append function for joining two lists together (p. 103)
        interp('(define b (list 4 5))')
        interp('(define c (append a b))')
        assert interp('(length? c)') == 5
        assert interp('(equal? c (list 1 2 3 4 5))') == True

        # Give your interpreter a map function (p. 105)
        interp('(define d (map (lambda (x) (* x x)) a))')
        assert interp('(equal? d (list 1 4 9))') == True

        # Give your interpreter a filter function (p. 115)
        interp('(define e (filter (lambda (x) (> x 1)) a))')
        assert interp('(equal? e (list 2 3))') == True

        # Give your interpreter an accumulate function (p. 116)
        assert interp('(accumulate + 0 a)') == 6

        # Give your interpreter a function for creating a range of integers (p. 116)
        interp('(define g (enumerate-interval 1 3))')
        assert interp('(equal? g a)') == True

    if __name__ == '__main__':
        test_pair()
        test_list()
        test_list_builtins()

# -----------------------------------------------------------------------------
# PART 10
#
# Reading:  2.3 Symbolic Data (p. 142-145)
#
# You need to give your interpreter the ability to quote symbols.
#
# (quote abc) -> 'abc'
# (quote 123) -> 123
# (quote (1 2 3)) -> (list 1 2 3)

if PROJECTLEVEL >= 10:
    def test_quote_simple():
        assert interp('(quote x)') == 'x'
        assert interp('(quote 1)') == 1

    def test_quote_list():
        assert interp('(equal? (quote (1 2 3)) (list 1 2 3))') == True
        assert interp('(equal? (quote (a b c)) (list (quote a) (quote b) (quote c)))') == True
        assert interp('(quote ())') == None

        # Symbols always compare by identity
        assert interp('(eq? (quote abc) (quote abc))')

    def test_quote_syntax():
        # Bonus.  Support the quote (') syntax
        if True:
            assert interp("'x") == 'x'
            assert interp("'1") == 1
            assert interp("(equal? '(1 2 3) (list 1 2 3))") == True
            assert interp("(equal? '(a b c) (list 'a 'b 'c))") == True

    if __name__ == '__main__':
        test_quote_simple()
        test_quote_list()
        test_quote_syntax()

# -----------------------------------------------------------------------------
# PART 11
#
# Reading:  Modularity, Objects, and State (p. 217-261)
#
# You're going to modify your interpreter to allow variables and pairs to be
# mutated in-place.  You'll also add the "begin" special form that allows
# multiple expressions to be chained.
#

if PROJECTLEVEL >= 11:
    def test_set():
        interp('(define abc 42)')
        interp('(set! abc 37)')
        assert interp('abc') == 37

    def test_nested_set():
        interp('''
           (define (incr n)
              (lambda ()
                 (begin
                    (set! n (+ n 1))
                    n
                 )
              )
           )
        ''')

        interp('(define a (incr 0))')
        assert interp('(a)') == 1
        assert interp('(a)') == 2
        assert interp('(a)') == 3

    def test_mutable_pair():
        interp('(define p (cons 1 2))')
        interp('(set-car! p 3)')
        assert interp('(car p)') == 3
        interp('(set-cdr! p 4)')
        assert interp('(cdr p)') == 4

    if __name__ == '__main__':
        test_set()
        test_nested_set()
        test_mutable_pair()

# -----------------------------------------------------------------------------
# PART 12
#
# Streams and delayed evaluation.  For this section, you need to implement
# delay, force, and cons-stream special forms. 
#
# (delay expr) is a syntactic transformation to (lambda () expr).
# (cons-stream a b) is the same as (cons a (delay b))
# (force a) is the same as (a)
#

if PROJECTLEVEL >= 12:
    def test_delay_force():
        interp('(define p (delay (+ 3 4)))')
        assert interp('(force p)') == 7

    def test_stream_cons():
        interp('(define (stream-car s) (car s))')
        interp('(define (stream-cdr s) (force (cdr s)))')
        interp('(define s (cons-stream 1 (+ 3 4)))')
        assert interp('(stream-car s)') == 1
        assert interp('(stream-cdr s)') == 7

    if __name__ == '__main__':
        test_delay_force()
        test_stream_cons()

# -----------------------------------------------------------------------------
# PART 13
#
# Metacircular evaluator.   For this part of the project, you need to give
# your interpreter an apply procedure.  It takes as input, a procedure and
# a list of arguments.   The procedure is called on those arguments.

if PROJECTLEVEL >= 13:
    def test_apply():
        interp('(define (f x y z) (+ x (+ y z)))')
        assert interp('(f 1 2 3)') == 6
        assert interp('(apply f (list 1 2 3))') == 6

    if __name__ == '__main__':
        test_apply()
