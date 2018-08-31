#!/usr/bin/env python3
import logging

from collections import ChainMap
from inspect import isfunction
from pprint import pprint

logging.basicConfig()
log = logging.getLogger()

# scopes
env = ChainMap({
    '+' : lambda x, y: x + y,
    '-' : lambda x, y: x - y,
    '>': lambda x, y: x > y,
    '<': lambda x, y: x < y,
    '=': lambda x, y: x == y,
    '>=': lambda x, y: x >= y,
    '<=': lambda x, y: x <= y,
    'null': None,
    'car': lambda p: p['car'],
    'cdr': lambda p: p['cdr']
})

# code line pointer
ptr = None

def s_eval(expr):
    global ptr
    if isinstance(expr, list):
        print("list =>", expr)
        return expr
    elif isinstance(expr, (int, float)):
        print("primitive =>", expr)
        return expr
    elif isinstance(expr, str):
        print("lookup =>", expr)
        # Lookup on a symbol name
        return env[expr]
    elif isinstance(expr, tuple):
        if isinstance(expr[0], tuple):
            print("nest =>", expr)
            for line in expr:
                ptr = line
                result = s_eval(line)
            return result
        elif expr[0] == 'define':
            print("define =>", expr)
            symbol = expr[1]
            expression = expr[2]
            if not isfunction(expression):
                expression = s_eval(expression)
            env[symbol] = expression
            print("set =>", symbol)
        elif expr[0] == 'if':
            print("if =>", expr)
            sym, *rest = expr
            predicate, if_true, if_false = rest
            if s_eval(predicate):
                print("if => True")
                return s_eval(if_true)
            else:
                print("if => False")
                return s_eval(if_false)
        elif expr[0] == 'and':
            print("and =>", expr)
            sym, *predicates = expr
            for predicate in predicates:
                if not s_eval(predicate):
                    print("and => False")
                    return False
            print("and => True")
            return True
        elif expr[0] == 'or':
            print("or =>", expr)
            sym, *predicates = expr
            for predicate in predicates:
                if s_eval(predicate):
                    print("or => True")
                    return True
            print("or => False")
            return False
        elif expr[0] == 'cond':
            # FIXME: cond needs tests to make sure it's right
            print("cond =>", expr)
            conditions = expr[1:]
            for condition in conditions:
                predicate, body = condition[0], condition[1:]
                if predicate == "else" or s_eval(predicate):
                    return s_eval(body)
        elif expr[0] == 'cons':
            print("cons =>", expr)
            # For a pair, you need to represent it somehow
            # behind the scenes (could use dict, tuple, whatever)
            return {'car': s_eval(expr[1]), 'cdr': s_eval(expr[2])}
        elif expr[0] == 'list':
            print("list =>", expr)
            elements = expr[1:]
            cells = tuple(reversed(elements))
            head = cells[0]
            tail = cells[1:] if len(cells[1:]) > 0 else None
            if tail is None:
                return s_eval(("cons", head, tail))
            return s_eval(("cons", head, ("list",) + tail))
        elif expr[0] == 'lambda':
            proc_args = expr[1]
            proc_body = expr[2]
            # nested scope
            define_scope = env

            # This will require some explanation!
            # This inner function here is our "procedure", and it has a
            # a few responsibilities:
            #     1. Keep a reference to the scope at definition time,
            #        so that it can access (close over) so-called "free
            #        variables"; this is known as closure. Ironically,
            #        this is implemented using Python closure over the
            #        define_scope label, closed over using `nonlocal`.
            #     2. Check arity of the function.
            #     3. Create a local `function_scope`, which comes from
            #        binding the function argument symbols from the
            #        function definition to the values passed in to
            #        the function. Ironically, this is done using a
            #        *-args function and closure, as well.
            #     4. Set up the environment for our call to `s_eval`,
            #        evaluate the s-expressions recursively, and
            #        capture the return value. Then, restore the
            #        environment to ensure no "dynamic scope" leakage.
            def proc(*args):
                global env
                nonlocal define_scope
                assert len(args) == len(proc_args), f"arity mismatch: {ptr}"
                # update scope with locals
                function_scope = define_scope.new_child()
                locals = dict(zip(proc_args, args))
                function_scope.update(locals)
                print(f"=> lambda call; bindings: {locals}")
                # set function scope
                old_env = env
                env = function_scope
                # evaluate body of the lambda
                result = s_eval(proc_body)
                # restore to prior scope
                env = old_env
                # return evaluated result of lambda
                return result

            proc.proc_body = proc_body
            proc.proc_args = proc_args
            return proc
        elif expr[0] == 'quote':
            raise NotImplemented
        elif expr[0] is None:
            return
        else:
            print("apply =>", expr)
            # Call apply (procedure)
            proc_name = expr[0]
            proc = env.get(proc_name, None)
            assert proc is not None, f"function {proc_name} not found: {ptr}"
            if hasattr(proc, "proc_body"):
                print("proc_body =>", proc.proc_body)
            else:
                print("proc_body => <builtin>")
            # Applicative evaluation. Arguments evaluate first
            args = [s_eval(arg) for arg in expr[1:]]
            return s_apply(proc_name, proc, args)

# Executes a procedure
def s_apply(proc_name, proc, args):
    global env
    print(f"call => '{proc_name}' fn with {tuple(args)}")
    ret = proc(*args)
    return ret

def main():
    print("\ngood run with if/and/list:")
    print("---")
    print(s_eval(
        ("if",
         ("and", 0, ("+", 42, 1), 0),
         ("+", 0, 2),
         ("list", 1, 2, ("-", 0, 2))
        )
     )
    )

    print("\narity error:")
    print("---")
    try:
        print(s_eval((
            ("define", "inc", ("lambda", ("x",), ("+", "x", 1))),
            ("inc", 1, 1)
        )))
    except:
        log.exception("can't s_eval")

    print("\nfunction lookup error:")
    print("---")
    try:
        print(s_eval((
            ("define", "inc", ("lambda", ("x",), ("+", "x", 1))),
            ("dec", 1)
        )))
    except:
        log.exception("can't s_eval")

    print("\nUDFs working:")
    print("---")
    # correct
    print(s_eval((
        ("define", "inc", ("lambda", ("x",), ("+", "x", 1))),
        ("define", "dec", ("lambda", ("x",), ("-", "x", 1))),
        ("define", "incdec", ("lambda", ("x", "y"), ("inc", "y"))),
        ("inc", 1),
        ("dec", 1),
        ("incdec", 0, 1),
        ("define", "fib", ("lambda", ("n",),
            ("if", ("<=", "n", 2),
             1,
             ("+", ("fib", ("-", "n", 1)), ("fib", ("-", "n", 2)))))),
        ("fib", 10)
    )))

if __name__ == "__main__":
    main()
