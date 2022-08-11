Grammar
-------

```
top:    {let [rec] dec | data | infix}
infix:  infixl/infixr int id...
data:   datatype (id) id = [|] ctor {| ctor}
ctor:   cid [ty]
exp:    infexp
        if exp then exp else exp
        case exp {| exp -> exp}
        fn frule {| fnrule}
        let dec in exp
        let rec dec in exp
infexp: appexp
        infexp id infexp
appexp: {atexp} atexp
atexp:  int / char / string
        id
        cid
        (exp,...)
        [exp,...]
dec:    lrule {| lrule}
        aexp = exp
frule:  atexp... -> exp
lrule:  id atexp = exp
```
