Grammar
=======

```
top:    {data/infix/let}                        Top level definition
infix:  'infixl'/'infixr' int {id}              Infix declaration
data:   'datatype' id '=' ctors                 Type definition w/o args
        'datatype' '( id,... ')' id             Type definition w/ args
ctors:  ctors '|' ctors                         Multiple constructors
        cid                                     No-Argument constructor
        cid ty                                  Single-argument constructor
ty:     aty '->' ty                             Function type
        aty {'*' aty}                           Tuple type
aty:    id                                      Type name
        aty id                                  Type application
        '(' ty,... ')' id                       Multi-arg type application
        '(' ty ')'                              Parenthesized type
let:    'let' ['rec'] defs                      Top-level definition
defs:   defs '|' defs                           Multiple definitions
        lrules                                  Function definition
        aexpr '=' expr                          Destructuring definition
expr:   'if' expr 'then' expr 'else' expr       Conditional expression
        'case' expr {'|' expr '->' expr}        Case analysis
        'let' ['rec'] defs                      Local definition
        expr op expr                            Infix expression
        {aexpr} aexpr                           Function application
aexpr:  int / char / string                     Constants
        id / cid                                Variable / Constructor
        '(' expr,... ')'                        Tuple
        '[' expr,... ')'                        List
        'fn' frules                             Function definition
frules: frules '|' frules                       Multiple rules
        {aexpr} '->' expr                       Single function rule
lrules: lrules '|' lrules                       Multiple rules
        id {aexpr} '=' expr                     Single let rule

int:    [0-9]+
char:   [']esc[']
string: ["]esc*["]
esc:    .|\\[tn\\"']
id:     [a-zA-Z0-9]+|[]
```

Transformations
---------------
* Check correctness
  * Variables defined
  * Identify built-in operations
  * Form is correct
    * letrec only defines functions
    * pattern forms are correct
  * Type-check
* Fix evaluation order and simplify forms
* Simplify case expressions
