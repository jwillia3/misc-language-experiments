GRAMMAR
-------

```
top:    {'let' ['rec'] decls}                   # Top-level definitions
decls:  decls 'and' decls                       # Multiple definitions
        id {id} '=' expr ['where' decls]        # Value or Function definition
        '[' id,... ']' '=' expr                 # List decomposition
expr:   expr op expr                            # Operator application
        expr '`'id expr                         # Infix function application
        'if' expr 'then' expr 'else' expr       # Conditional expression
        'let' 'rec' decls 'in' expr             # Local recursive declaration
        'let' decls 'in' expr                   # Local declaration
        {aexpr} aexpr                           # Function application
aexpr:  int / string / false / true             # Literal constant
        id                                      # Variable
        '(' expr ')'                            # Parenthesised expression
        '[' expr,... ']'                        # List expression
        'fn' {id} '->' expr                     # Function
```

Operator Precedence
-------------------

```
┌───────┬───────────────────┬───────────────┐
│ Level │ Left              │ Right         │
├───────┼───────────────────┼───────────────┤
│  10   │                   │               │
├───────┼───────────────────┼───────────────┤
│   9   │ `                 │               │
├───────┼───────────────────┼───────────────┤
│   8   │ * / rem           │               │
├───────┼───────────────────┼───────────────┤
│   7   │ + - @             │ ^             │
├───────┼───────────────────┼───────────────┤
│   6   │ nth               │ :             │
├───────┼───────────────────┼───────────────┤
│   5   │ == <> < > <= >=   │               │
├───────┼───────────────────┼───────────────┤
│   4   │                   │ &&            │
├───────┼───────────────────┼───────────────┤
│   3   │                   │ ||            │
├───────┼───────────────────┼───────────────┤
│   2   │ <-                │               │
├───────┼───────────────────┼───────────────┤
│   1   │                   │ $             │
├───────┼───────────────────┼───────────────┤
│   0   │                   │ ;             │
└───────┴───────────────────┴───────────────┘
```
