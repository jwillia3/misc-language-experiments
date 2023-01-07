
Grammar
-------
```
top:    ('let' ['rec'] decls)...                # Top-level definitions
decls:  decls "and" decls                       # Multiple definitions
        id aexpr... '=' expr                    # Function definition
        aexpr '=' expr                          # Variable definition
expr:   'if' expr 'then' expr 'else' expr       # Conditional
        'case' expr ('|' expr '->' expr)...     # Case analysis
        'let' ['rec'] decls 'in' expr           # Local definition
        expr id expr                            # Operator application
        {aexpr} aexpr                           # Function application
aexpr:  int / char / string / true / false      # Constant
        id                                      # Variable reference
        '!' aexpr                               # Dereference
        '(' expr,... ')'                        # Tuple
        '[' expr,... ']'                        # List
        'fn' aexpr... '->' expr                 # Anonymous function

id:     [a-zA-Z0-9_']+ | [!$%&*+-./:<=>?@^|~]+
```

== Operators
 lvl | Left             | Right
-----+------------------+---------------
  9  |                  | .
  8  |                  |
  7  | * / rem          |
  6  | + -              | ^
  5  | @                |
  4  | == <> < > <= >=  |
  3  |                  | &&
  2  |                  | orelse
  1  |                  | := $=
  0  |                  | ;

Strings are double-quoted. Escape sequences are introduced by backslash (\\).
\n \t \" and \' \\ have their conventional meaning.

Characters are single-quoted.

Integers must be all digits except for an optional negative sign.
Digits are always interpreted as base 10 even if there is an initial 0.



Transformations
---------------
- Ground Rules:
  - Expressions are always replaced, never modified
  - Checks are run on subtrees only after all transforms are done
- De Bruijn index all variables
  - At run-time all variables are accessed by rather than name
  - The de Bruijn index tells you how many let definitions and
    parameters have been declared since the given variable
  - Signal an error if a variable is not defined
- Find all references to primitives
  - If in a call position, use a special call node
  - If in any other position (used by value), refer to its
    static value. I.e. all instances of (+) refer to the same
    fn value. When partially applied, it will generate a new
    specialised fn
- Functions are broken down to one-variable definitions
  - Multiple parameters are broken down one-by-one
  - If a param is a pattern, a named param is created to hold
    its value and it is broken into a decision tree.
  - The true branch should be the body of the function
  - The false branch should be a crash pointing to the source
    of the l.h.s. and printing the param variable
- Case expressions are broken down
  - If the subject is not a variable, make a variable for it
    since its value will be referred to many times in the
    decision tree
  - For each rule, generate a decision tree in which the true
    branch is the r.h.s. and the false branch is the decision
    tree for the next rule.
  - The final false branch should be a crash expression whose
    source location is the subject of the case expression and
    the value it prints should be the case variable
  - By the end, there will be no more case expressions
- Recursive let expressions are not broken down
- Let expressions are broken into single-variable definitions
  - Lets with multiple definitions are chained into single
    definitions (.e.g `let x=1 and y=2 in 0` goes to
    `let x=1 in let y=2 in 0`)
  - If the l.h.s. is not a simple variable, create a variable
    for it and use that as the subject of the decision tree
  - Convert each definition to a decision tree where the subject
    is the r.h.s., its true case is decision tree of the next
    definition, and its false case is a crash expression whose
    location is the l.h.s. pattern, and prints the subject
    variable
- Decision Trees
  ```
  # where x is the subject of the pattern
  p <lit> x yes no -> if x == <lit> then yes else no
  p <_> x yes no   -> yes, if x is lit or x is id
  p <id> x yes no  -> let <id> = x in yes
  p (a,b,c) x yes no -> istuple x && tuplelength x == <length> && a'
                        where a' = p a (x @* 0) b' no
                              b' = p b (x @* 1) c' no
                              c' = p c (x @* 2) yes no
  p [a,b,c] x yes no -> islist x && length x == <length> && a'
                        where a' = p a (x @ 0) b' no
                              b' = p b (x @ 1) c' no
                              c' = p c (x @ 2) yes no
  p (a:b) x yes no   -> iscons x && p a (hd x) (p b (tl x) yes no) no
  ```

