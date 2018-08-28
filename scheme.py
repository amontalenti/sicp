# Scheme Interpreter Project
#
# In this project, you'll write a simple Scheme interpreter.  The
# project is implemented in stages, roughly following the outline
# of material in the SICP book.    
#
# This file contains a shell of Python code that needs to be filled
# out to make it work. 
#
# There are a set of tests found in the file test_scheme.py.  To
# complete the project, you should follow the more detailed instructions
# in that file.

import operator

def parse_expression(text):
    '''
    Parse a Scheme expression into a Python object. 

    The syntax of Scheme is minimal.  You need to deal with 4 basic tokens

      -  Parentheses ( )
      -  Numbers   123, 3.4
      -  Symbols   abc, anything else that's not ( ) or a number.

    Represent numbers as Python numbers.  Represent symbols as Python 
    strings.  Represent compound expressions as tuples.  For example:

       (* 3 5)

    Gets translated to the tuple:

       ('*', 3, 5)

    More complicated expressions turn into nested tuples. For example:
 
       (+ (* 3 5) (- 10 2))

    Translates to the tuple:

      ('+', ('*', 3, 5), ('-', 10, 2))

    '''
    pass

def substitute(expr, name, val):
    '''
    Given a Scheme expression represented as a Python object, a name,
    and a value, substitute the the name with the value.

    For example, if expr is ('*', 'x', 'x'),  substituting 'x' with 7
    produces ('*', 7, 7).   Substituting 'x' with ('+', 3, 4)
    produces ('*', ('+', 3, 4), ('+', 3, 4)).
    '''
    pass

# ----------------------------------------------------------------------
# The Scheme interpreter.
#
# The interpreter instance works as a callable.  Create an instance and
# call it like a function on a Scheme expression. For example:
#
#   interp = Interpreter()
#   interp('(+ 3 4)')          -> 7
#
# The above example, '+' is a builtin procedure.  Your interpreter will
# need to keep a table of primitive operations for basic things such as
# math operations.
#
# The interpreter needs to keep an environment of names so that you can
# define things and look them up later:
#
#   interp('(define x 42)')   
#   interp('(+ x 10)')        -> 52
#
# "define" is not a normal procedure, but an example of a "special form".
# Your interpreter will need to check for it and implement it as a 
# special case. For example, in the above code (define x 42) can't
# be treated as a normal procedure because the "x" is as yet undefined.
#
# Built-in procedures are first-class objects like numbers.  So,
# things like this are perfectly legal:
#
#   interp('(define add +)')
#   interp('(add 3 4)')        -> 7
# -----------------------------------------------------------------------------

import operator
from collections import ChainMap

# Built-in functions.  These are primitive operations actually
# carried out by Python.  

builtins = {
    '+': operator.add,
    '-': operator.sub,
    '*': operator.mul,
    '/': ...             # What are type semantics of division?

    # More builtins will need to be added as you proceed in later parts
    }

class Interpreter:
    def __init__(self):
        # ChainMap combines two or more dictionaries into a single 
        # logical dictionary.   We're using this to create an environment
        # for defining symbols and looking up names
        self.environment = ChainMap({}, builtins)

        # Initialize builtin-library functions (if any)
        self.init_library()

    def __call__(self, code):
        '''
        Evaluate a scheme expression (presented as a string) and return a result
        '''
        expr = parse_expression(code)
        return self.eval(expr)

    def eval(self, expr):
        '''
        Evaluate a scheme expression that's already been parsed into a Python representation.
        '''
        # If the expr is a simple number, leave it as is
        if isinstance(expr, (int, float)):
            return expr

        # If expr is a string, it's a "symbol".  Look it up in the environment
        elif isinstance(expr, str):
            return self.environment[expr]
        
        # If expr is a tuple, it's a combination or special form.
        elif isinstance(expr, tuple):
            op, *args = expr

            # Check for special forms
            if hasattr(self, f'special_{op}'):
                return getattr(self, f'special_{op}')(*args)
            
            # Not a special form, evaluate "op" as a procedure
            op = self.eval(op)

            # Evaluate the arguments
            args = [ self.eval(arg) for arg in args ]

            if isinstance(op, Procedure):
                # User-defined procedures (written in Scheme)
                return self.apply(op, args)
            else:
                # Built-in procedure (implemented in Python)
                return op(*args)

    def apply(self, proc, args):
        '''
        Apply a scheme procedure.
        proc is an instance of Procedure below.
        args is a tuple of already evaluated arguments.
        '''
        pass

    def special_define(self, name, value):
        '''
        Special form (define name value).  Stores a value in the environment
        '''
        self.environment[name] = self.eval(value)

    def init_library(self):
        # If you want any library functions (implemented in Scheme), add them here.
        # For example, procedures such as map, filter, in later stages of the project.
        # self('(define (func x y) (...))')
        pass

class Procedure:
    '''
    Class for representing a Scheme procedure.  Minimally it must
    store the argument names and procedure body.  Additional attributes
    might be added later
    '''
    def __init__(self, argnames, body):
        self.argnames = argnames
        self.body = body

    def __repr__(self):
        return f'Procedure({self.argnames}, {self.body})'

class Pair:
    '''
    Class for representing a cons pair.
    '''
    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    def __repr__(self):
        return f'({self.car} . {self.cdr})'

    def __eq__(self, other):
        return isinstance(other, Pair) and (self.car, other.cdr) == (other.car, other.cdr)

# This is merely a very basic interactive REPL for the scheme
# interpreter.  This could be hacked to be a lot better.
def run(lines):
    import traceback
    interp = Interpreter()
    expr = []
    nesting = 0
    for line in lines:
        line = line.strip()
        if not line or line.startswith(';'):
            continue
        expr.append(line)
        nesting += line.count('(') - line.count(')')
        if nesting == 0:
            try:
                print(interp('\n'.join(expr)))
            except Exception as e:
                traceback.print_exc()
            expr = []

if __name__ == '__main__':
    import itertools
    import sys
    if len(sys.argv) == 2:
        lines = itertools.chain(open(sys.argv[1]), sys.stdin)
    else:
        lines = sys.stdin
    run(lines)


        
