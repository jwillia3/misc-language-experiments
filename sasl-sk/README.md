= G R A M M A R

```
program     (def dec)...
dec         atexp... = exp
exp         if exp then exp else exp
            let [rec] dec... in exp
            infexp
infexp      appexp id infexp
appexp      appexp atexp
atexp       false | true | number | string | id | ( exp ) | [ exp,... ]
```

= O P E R A T O R S / P R E L U D E
```
level       left                right
8
7
6           * / rem
5           - +
4           == <> <= >= < >
3
2           
1                               :
0           
```

= C O M P I L I N G

Compile
{\x.a}      => [x]{a}
{a b}       => {a} {b}
{a}         => a

Abstract x
[x](a b)    => S [x]a [x]b
[x]x        => I
[x]y        => K y

Sfgx    => (fx)(gx)
Kxy     => x
Yh      => h(Yh)
Bfgx    => f(gx)
Cfgx    => (fx)g
Ix      => x
Uf(Pxy) => fxy
Ufx     => crash
?true xy => x
?false xy => y
?axy    => crash
+xy     => x+y
-xy     => x-y