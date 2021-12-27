program:    dec...
dec:        "def" id id... = exp.
exp:        exp "where" (id = exp),...
            if exp then else exp
            infexp
infexp:     appexp id appexp
appexp:     atexp... atexp
atexp:      false/true/number/string/id/(exp)/[exp,...]

