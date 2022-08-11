# Proto Applicative Language
A small, dynamically typed, eager applicative language.

Features:
- Pattern matching in functions, let, and case expressions
- Single-argument data constructors
- Tail call elimination

## Grammar
```
expr:   'if' expr 'then' expr 'else' expr
        'case' expr {'|' bexpr '->' expr}
        'let' ['rec'] def {'and' def} 'in' expr
        cexpr
cexpr:  {bexpr} bexpr {op cexpr}
bexpr:  cname/name/int/str
        '(' [expr {',' expr}] ')'
        '[' [expr {',' expr}] ']'
        '\\' {bexpr} '->' expr
def:    bexpr {bexpr} '=' expr
OP:     [:;^]
NAME:   [a-z_'][a-zA-Z0-9_']+
        [!$%&*+\-./:<=>?@^|~]
CNAME:  [A-Z][a-zA-Z0-9_']+
RESV:   '->'/':'/'='/'^'/'and'/'case'/'else'/'if'/'in'/'let'/'rec'/'then'/'|'
```

## Types
- undefined
  - the result of an invalid expression
  - a pattern match that does not match the value
  - the value of an undefined variable
  - the value of applying a non-function to any argument
  - the value of applying any function to `undef`
- tuple/list/unit/nil
  - tuples are cartesian pairs `(x, y)`
  - unit `()` and nil `[]` are the same
  - lists are right-nested pairs that end with `[]`
  - `(a,b,c,d)` is shorthand for `a:b:c:d:()`
  - `[a,b,c,d]` is shorthand for `a:b:c:d:[]`
- integer
- string
- constructed data
  - `True` and `False` are used for booleans
  - any capital identifier is automatically a data constructor
- function
  - a data constructor that has not been applied to a value is a function
  - params can be patterns but they are irrefutable; there are no alternatives
  - certain procedures are built in and already exist in the environment

## Example
```
let rec foldr f init xs =
  case xs
  | (x:xs') -> f x (foldr f init xs')
  | [] -> init
and reverse xs = foldl (flip cons) [] xs
and map f xs = foldr (\x xs-> f x : xs) [] xs
in
map (+ 100) [1, 2, 3]
```
