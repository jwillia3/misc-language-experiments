Grammar
-------
```
top:    expr
comp:   {LINEBREAK expr} 'end'
        expr
block:  'do' '(' id,... ')' comp
expr:   expr op expr
        suffix
suffix: suffix '(' expr,... ')' [block]
        suffix '[' expr ']'
        prefix
prefix: [!-]? prim
prim:   'if' expr comp 'else' comp
        'while' expr comp
        'return' expr
        'fun' '(' id,... ')' comp
        'var' id '=' expr
        'class' id block
        'nil' / 'false' / 'true' / num / string
        id
        '(' expr ')'
        '[' expr,... ']'
        '{' entry,... '}'
entry:  id ':' expr
        string ':' expr
        int ':' expr
```

Operators
---------

```
High    Left                Right
        if while
        () [] .
        ! -                                     (unary)
        * / %
        + -
        == != < > <= >= < >
        &&
        ||
                            ? :
Low                         = += -= *= /= %=

```

TODO
====
[ ] Associate bytecode with pos
[ ] Ranges
[ ] Error on too many args
[ ] Automatic line continuation on binary op and comma
[ ] Const var declaration
[ ] Destructuring var
[ ] Exceptions
[ ] String interpolation
[ ] Spread args `f(array...)`
[ ] Spread array `[1, flatten..., 2]`
[ ] Spread map `{a:1, flatten... }`
[ ] Make sure immediates are always <64K
[ ] Convert to better branch prediction on switch
[ ] Check stack exhaustion
[ ] Malicious binaries can crash VM (e.g. MFGET)
[ ] Line reader is exploitable for lines over 64KB
