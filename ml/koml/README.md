 
Grammar
-------
```
top:    'let' ['rec'] decls
decls:  decls "and" decls
        id aexpr... '=' expr
        aexpr '=' expr
expr:   'if' expr 'then' expr 'else' expr
        'case' expr ('|' expr '->' expr)...
        'let' ['rec'] decls 'in' expr
        iexpr
iexpr:  iexpr op iexpr
        {aexpr} aexpr
aexpr:  int / character / string / true / false
        id
        '(' expr,... ')'
        '[' expr,... ']'
        'fn' aexpr... '->' expr

id:     [a-zA-Z0-9_']+ | [!%&$*+-/:<=>?@~`^|]+
op:     '<' '>' '<=' '>=' '==' '<>' [+-*/]
```
