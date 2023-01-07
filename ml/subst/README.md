Synopsis
--------

A small lazy functional language.

- The implementation technique is graph substitution
- The language includes case analysis
- Cell-based: no separation between expressions and values
- Strings are char lists
- `undef` for errors

Grammar
-------

```
cmpl:   'case' expr {'|' def}
        '\' {simp} '->' expr
        iexpr
iexpr:  iexpr op iexpr
        aexpr
aexpr:  {simp} simp
simp:   int / char / string / "true" / "false" / "undef"
        id
        '(' expr ')'
        '[' expr,.. ']'
def:    simp '->' expr

int:    -?[0-9]+
char:   '%'.
string: "(\\.|.)*"

```

Operators


```
┌─────────┬───┐
│ .       │   │
├─────────┼───┤
│ * /     │   │
├─────────┼───┤
│ + -     │   │
├─────────┼───┤
│         │ : │
├─────────┼───┤
│ = ! < > │   │
├─────────┼───┤
│         │ $ │
└─────────┴───┘
```
