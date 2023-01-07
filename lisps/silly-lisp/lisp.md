 
SILLY LISP
==========


FUNCTIONS
=========

Arithmetic
----------
```
(* x y)
(+ x y)
(- x y)
(/ x y)
(< x y)
(<= x y)
(remainder x y)
```

General
----------
```
(apply f args)
(equal? x y)
```

I/O
---
```
(exit code)
(print x...)
(read-file path)
(repr x...)
(typeof x)
```

List Processing
---------------
```
(assoc key alist)
(car cons)
(cdr cons)
(cons car cdr)
(list x...)
(setcar cons car)
(setcdr cons cdr)
```

String
------
```
(char-at string index)
(chr byte)
(implode string-list)
(ord char)
(string->symbol string)
(string-length string)
(substring string low high)
(symbol->string symbol)
```

SPECIAL FORMS
=============
```
(quote x)
(quasiquote x)
    (unquote x)

(begin defines...
        values...)
    (define variable value)
    (define (function args) body)

(if condition true false)
(let id value
     (function args) body
     final)
(let* id value
      (function args) body
      final)
(letrec (function args) body
        (function args) body
        final)

(lambda (required-args) body)
(lambda (required-args . rest-args) body)

(case value
      pattern body
      pattern body)

      pattern:  constant
                'object
                symbol
                (lhs . rhs)

(cond
    condition body
    condition body)

(and values...)
(or values...)
```
