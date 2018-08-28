#!/usr/bin/env python3

# Evaluates an expression
#    42, 4.2 -> 42, 4.2
#    symbol
#    (proc arg1 arg2 arg2)
#    (proc, arg1, arg2, arg3)

env = {
    '+' : lambda x, y: x + y,
    '-' : lambda x, y: x - y,
    'car': lambda p: p['car'],
    'cdr': lambda p: p['cdr']
}

def eval(expr):
    if isinstance(expr, (int, float)):
        return expr
    elif isinstance(expr, str):
        # Lookup on a symbol name
        return env[expr]
    elif isinstance(expr, tuple):
        if expr[0] == 'define':
            # Special forms
            env[expr[1]] = eval(expr[2])
        elif expr[0] == 'if':
            ...
        elif expr[0] == 'and':
            ...
        elif expr[0] == 'cons':
            # For a pair, you need to represent it somehow
            # behind the scenes (could use dict, tuple, whatever)
            return {'car': eval(expr[1]), 'cdr': eval(expr[2])}
        else:
            # Call apply (procedure)
            proc = eval(expr[0])
            # Applicative evaluation. Arguments evaluate first
            args = [eval(arg) for arg in expr[1:]]
            return apply(proc, args)

# Executes a procedure
def apply(procedure, args):
    return procedure(*args)
