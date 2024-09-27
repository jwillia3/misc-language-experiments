# GRAMMAR
```
expr    nil/true/false/int/string/name/(expr)/[expr,...]
        expr [expr] / expr (expr,...) / expr.name / expr.int
        not expr / - expr
        expr op expr
        fun [name] (name,...) expr
        datatype name (name,...)
        var name = expr
        if expr then expr else expr
        puts expr
        { expr;... }
op      * / %
        + - ^
        is
        == <> < > <= >=
        and or
        =
```