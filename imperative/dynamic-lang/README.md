```
top:        import/def/var/class
import:     'import' ID
def:        'def' ID '(' ID,... ')' stmt
var:        'var'/'const' ID '=' expr ';'
class:      'class' ID '{' classdef '}'
classdef:   'class' def/var
stmt:       'if' '(' expr ')' stmt 'else' stmt
            'while' '(' expr ')' stmt
            'return' [expr] ';'
            var
            def
            expr ';'
expr:       expr1 '?' expr ':' expr
expr1:      prefix OP prefix
prefix:     ('-' | 'not')... suffix
suffix:     suffix '(' expr,... ')'
            suffix '.' ID
            suffix '=>' stmt_or_expr
            suffix '[' expr ']'
            primary
primary:    NUM / STRING / 'true' / 'false' / 'null'
            ID
            '(' expr,... ')'
            '[' expr,... ']'
            '{' (ID ':' expr),... '}'
OP:         ??                                      # 7 right
            * / %                                   # 6 left
            + - ^                                   # 5 left (^ right)
            == != < <= >= >                         # 4 left
            &&                                      # 3 right
            ||                                      # 2 right
            = += -= *= /= %=                        # 1 right
```

# Why tuples and lists?
- tuples are immutable and a value type
- `(1, 2) == (1, 2) == true` unlike for lists
- tuples cannot expand or be modified

# About `this`
- `this` is not a variable, it is special
- `this` is assigned when calling `obj.method(args)`
- a new bound fn is made when `obj.method` is not in a call position

# Etc
- `??` has a high precedence unlike JavaScript
