# lambda calculus code

# Booleans
def TRUE(x):
    return lambda y: x

def FALSE(x):
    return lambda y: y

# Logical operators
def AND(x):
    return lambda y: x(y)(FALSE)

def OR(x):
    return lambda y: x(TRUE)(y)

def NOT(x):
    return x(FALSE)(TRUE)

# Pairs
def CONS(a):
    return lambda b: (lambda z: z(a)(b))

def CAR(p):
    return p(TRUE)

def CDR(p):
    return p(FALSE)

# Numbers
def ZERO(f):
    return lambda x: x

def ONE(f):
    return lambda x: f(x)

def TWO(f):
    return lambda x: f(f(x))

def SUCC(n):
    return lambda f: (lambda x: f(n(f)(x)))

THREE = SUCC(TWO)
FOUR = SUCC(THREE)
FIVE = SUCC(FOUR)

# Try some math
def f(x):
    return x + 1

print(THREE(f)(0))
print(FOUR(f)(0))

print(THREE(SUCC)(FOUR)(f)(0))   # -> 3 + 4 = 7
print(THREE(FOUR(f))(0))         # -> 3 * 4 = 12
print(THREE(FOUR)(f)(0))         # -> 4 ** 3 = 64

# Zero conditional test

def Z(x):
    return x(FALSE)(NOT)(FALSE)     

# Predecessors
def PHI(p):
    return CONS(SUCC(CAR(p)))(CAR(p))

def PRED(n):
    return CDR(n(PHI)(CONS(ZERO)(ZERO)))


# Recursion:

# The "Y Combinator"
def Y(f):
    return (lambda x: f(x(x)))(lambda x: f(x(x)))

# A recursive function (sum of first n integers)
R = lambda r: (lambda n: Z(n)(ZERO)(n(SUCC)(r(PRED(n)))))

# Apply it. Sum the first four integers.
# total = Y(R)(FOUR)
# print(total(f)(0))
# It doesn't work.   WHY?




        

