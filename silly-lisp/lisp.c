#ifdef USE_GC
#define DEBUG_GC 1
#define malloc GC_malloc
#include <gc.h>
#endif
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum special {
    // Normal built-in functions
    STYPEOF, SPRINT, SREPR, SCAR, SCDR, SCONS, SLIST, SEQUAL,
    SLT, SLE, SADD, SSUB, SMUL, SDIV, SREM, SCHR, SORD, SCHARAT,
    SSTRLEN, SSUBSTR, SIMPLODE, STOSYMBOL, STOSTRING, SASSOC,
    SRDFILE, SEXIT, SAPPLY, SSETCAR, SSETCDR,

    // Special forms
    SQUOTE, SQUASI, SBEGIN, SIF, SLET, SLETSTAR, SLETREC,
    SLAMBDA, SCASE, SCOND, SAND, SOR, SSET,
};

struct spec { char *id; int arity; };
struct spec specs[] = {
    {"typeof",1}, {"print",-1}, {"repr",-1}, {"car",1},
    {"cdr",1}, {"cons",2}, {"list",-1}, {"equal?",2}, {"<",2},
    {"<=",2}, {"+",2}, {"-",2}, {"*",2}, {"/",2},
    {"remainder",2}, {"chr",1}, {"ord",1}, {"char-at",2},
    {"string-length",1}, {"substring",3}, {"implode",1},
    {"string->symbol",1}, {"symbol->string",1}, {"assoc",2},
    {"read-file",1}, {"exit",1}, {"apply",2}, {"setcar",2},
    {"setcdr",2},

    {"quote",1}, {"quasiquote",1}, {"begin",-1}, {"if",3},
    {"let",-1}, {"let*",-1}, {"letrec",-1}, {"lambda",-1},
    {"case",-1}, {"cond",-1}, {"and",-1}, {"or",-1}, {"set",2},
    {0,0}
};

typedef struct obj {
    enum {NIL,INT,STR,SYM,CONS,FN,SPEC} type;
    union {
        struct { int n; char *text; };
        struct cons     *cons;
        enum special    spec;
        struct fn       *fn;
    };
    char *loc;
} obj;
struct cons { obj car, cdr; };
struct fn { obj ps, body, env; };
char    *typenames[] = {
            "nil","integer","string","symbol","cons","function","proc",0
        };

char    *src;
char    source[65536];
char    tokbuf[sizeof source];
char    *interns[65536];
char    *fname;
int     line;
int     token;
obj     nil;
obj     _true;
obj     lambdasym;
obj     quotesym;
obj     quasisym;
obj     unquotesym;
obj     definesym;
obj     emptystring;
obj     chars[256];
int     openparens[65536];
int     nopenparens;

obj readobj(int token);
obj process(obj c, obj env);
obj eval(obj c, obj env);
obj pr(obj x, bool rep);

char *intern(char *text) {
    char **i;
    for (i = interns; *i; i++)
        if (!strcmp(text, *i)) return *i;
    return *i = strcpy(malloc(strlen(text) + 1), text);
}

bool consp(obj x) { return x.type == CONS; }
bool nullp(obj x) { return x.type == NIL; }
bool intp(obj x) { return x.type == INT; }
bool symbolp(obj x) { return x.type == SYM; }
bool stringp(obj x) { return x.type == STR; }
bool specialp(obj x) { return x.type == SPEC; }
bool functionp(obj x) { return x.type == FN; }
obj num(int n) { return (obj){INT, .n=n}; }
obj string(char *s, int n) { return (obj){STR, .n=n, .text=s}; }
obj symbol(char *id) { return (obj){SYM, .text=intern(id), .n=-1}; }
obj special(enum special s) { return (obj){SPEC, .spec=s}; }
obj cons(obj car, obj cdr) {
    struct cons *cons = malloc(sizeof *cons);
    *cons = (struct cons){car, cdr};
    return (obj){CONS, .cons=cons};
}
obj lambda(obj ps, obj body, obj env) {
    struct fn *fn = malloc(sizeof *fn);
    *fn = (struct fn){ps, body, env};
    return (obj){FN, .fn=fn};
}

obj car(obj x) { return consp(x)? x.cons->car: nil; }
obj cdr(obj x) { return consp(x)? x.cons->cdr: nil; }
obj caar(obj x) { return car(car(x)); }
obj cadr(obj x) { return car(cdr(x)); }
obj cdar(obj x) { return cdr(car(x)); }
obj cddr(obj x) { return cdr(cdr(x)); }
obj first(obj x) { return car(x); }
obj second(obj x) { return car(cdr(x)); }
obj third(obj x) { return car(cddr(x)); }
obj setcar(obj x, obj car) { if (consp(x)) x.cons->car = car; return car; }
obj setcdr(obj x, obj cdr) { if (consp(x)) x.cons->cdr = cdr; return cdr; }

bool equal(obj x, obj y) {
    if (x.type != y.type) return false;
    switch (x.type) {
    case NIL:   return true;
    case INT:   return x.n == y.n;
    case STR:   return x.text == y.text ||
                    (x.n == y.n && !memcmp(x.text, y.text, x.n));
    case SYM:   return x.text == y.text;
    case CONS:  for ( ; consp(x) && consp(y); x = cdr(x), y = cdr(y))
                    if (!equal(car(x), car(y))) return false;
                return equal(x, y);
    case FN:    return x.fn == y.fn;
    case SPEC:  return x.spec == y.spec;
    }
}

obj assoc(obj x, obj env) {
    for ( ; consp(env); env = cdr(env))
        if (equal(caar(env), x)) return car(env);
    return nil;
}

obj setloc(obj c, int line) {
    c.loc = malloc(strlen(fname) + 16 + 1);
    sprintf(c.loc, "%s:%d", fname, line);
    return c;
}

void opensource(char *fn) {
    fname = intern(fn);
    line = 1;
    src = source;
    FILE *file = fn? fopen(fn, "rb"): 0;
    if (!file) { printf("lisp: cannot open %s\n", fn? fn: ""); exit(1); }
    source[fread(source, 1, sizeof source, file)] = 0;
    fclose(file);
}

int readtoken() {
    while (isspace(*src) || *src == ';')
        if (*src == ';') while (*src && *src != '\n') src++;
        else if (*src == '\n') src++, line++;
        else src++;

    char *t = tokbuf;
    char *puns = "()[].'`,; \t\n";

    if (*src == 0) return token = 0;
    if (*src == '\"')
        for (src++; *src; src++)
            if (*src == '\"') return *t = 0, src++, token = 'S';
            else if (*src == '\n') *t++ = *src++, line++;
            else if (*src == '\\')
                switch (*++src) {
                case 'n': *t++ = '\n'; break;
                case 't': *t++ = '\t'; break;
                default:  *t++ = *src; break;
                }
            else *t++ = *src;
    while (*src && !strchr(puns, *src)) *t++ = *src++;
    *t = 0;
    return token = (*tokbuf? 'A': *src++);
}

obj readlist() {
    int loc = line;
    readtoken();
    if (token == ')') { nopenparens--; return nil; }
    if (token == 0) {
        printf("lisp: %s:%d: unclosed list\n", fname, line);
        for (int i = nopenparens; i-- > 0; )
            printf("list: > %s:%d\n", fname, openparens[i]);
        exit(1);
    }
    if (token == '.') {
        obj end = readobj(readtoken());
        if (readtoken() != ')')
            printf("lisp: %s:%d: missing close after dot\n", fname, line), exit(1);
        nopenparens--;
        return end;
    }
    obj car = readobj(token);
    obj cdr = readlist();
    return setloc(cons(car, cdr), loc);
}

obj readobj(int token) {
    int loc = line;
    switch (token) {
    case 0:     return nil;
    case 'A':   return isdigit(tokbuf[*tokbuf == '-'])
                        ? setloc(num(atoi(tokbuf)), loc)
                        : setloc(symbol(intern(tokbuf)), loc);
    case 'S':   return setloc(string(intern(tokbuf), strlen(tokbuf)), loc);
    case '(':   openparens[nopenparens++] = line; return readlist();
    case '\'':  return setloc(cons(quotesym, cons(readobj(readtoken()), nil)), loc);
    case '`':   return setloc(cons(quasisym, cons(readobj(readtoken()), nil)), loc);
    case ',':   return setloc(cons(unquotesym, cons(readobj(readtoken()), nil)), loc);
    default:   printf("lisp: %s:%d: misplaced (%c)\n", fname, line, token); exit(1);
    }
    return nil;
}

obj pr(obj x, bool rep) {
    obj i;
    switch (x.type) {
    case NIL:   printf("()"); break;
    case INT:   printf("%d", x.n); break;
    case STR:   if (rep) {
                    putchar('"');
                    for (int i = 0; i < x.n; i++)
                        if (x.text[i] == '"') printf("\\\"");
                        else if (x.text[i] == '\\') printf("\\\\");
                        else if (x.text[i] == '\n') printf("\\n");
                        else putchar(x.text[i]);
                    putchar('"');
                } else fwrite(x.text, 1, x.n, stdout);
                break;
    case SYM:   printf("%s", x.text); break;
    case CONS:  putchar('(');
                for (i = x; consp(i); i = cdr(i)) {
                    pr(car(i), true);
                    if (consp(cdr(i))) putchar(' ');
                }
                if (!nullp(i)) {
                    printf(" . ");
                    pr(i, true);
                }
                putchar(')');
                break;
    case FN:    printf(";(%s ", lambdasym.text);
                pr(x.fn->ps, true);
                putchar(' ');
                pr(x.fn->body, true);
                putchar(')');
                break;
    case SPEC:  printf("%s", specs[x.spec].id); break;
    }
    return x;
}

obj semantic(obj c, char *msg) {
    printf("lisp: %s: semantic error: %s on ",
           c.loc? c.loc: "(unknown location)",
           msg);
    pr(c, true);
    puts("");
    exit(1);
}

obj match(obj pattern, obj value, obj env) {
    if (equal(car(pattern), quotesym))
        return equal(second(pattern), value)? env: nil;
    if (consp(pattern)) {
        env = match(car(pattern), car(value), env);
        return nullp(env)? nil: match(cdr(pattern), cdr(value), env);
    }
    if (symbolp(pattern))
        return cons(cons(pattern, value), env);
    return equal(pattern, value)? env: nil;
}

obj bindargs(obj call, obj params, obj args, obj env, obj newenv, bool doeval) {
    for ( ; consp(params); params = cdr(params), args = cdr(args)) {
        if (nullp(args))
            semantic(call, "too few args");
        obj x = doeval? eval(car(args), env): car(args);
        newenv = cons(cons(car(params), x), newenv);
    }

    if (!nullp(params)) {
        obj rest = nil;
        obj *ptr = &rest;
        for ( ; consp(args); args = cdr(args)) {
            obj x = doeval? eval(car(args), env): car(args);
            *ptr = cons(x, nil);
            ptr = &ptr->cons->cdr;
        }
        newenv = cons(cons(params, rest), newenv);
    }
    else if (!nullp(args))
        semantic(call, "too many args");

    return newenv;
}

obj checkprocargs(obj call, enum special spec, obj args) {
    if (specs[spec].arity > 0) {
        obj a = args;
        for (int n = specs[spec].arity; n-- > 0; a = cdr(a))
            if (!consp(a)) semantic(call, "too few args");
        if (!nullp(a)) semantic(call, "too many args");
    }
}

obj doprocedure(obj call, enum special spec, obj args, obj env, bool doeval) {
    int nargs = specs[spec].arity;
    obj x = nargs > 0? doeval? eval(first(args), env): first(args): nil;
    obj y = nargs > 1? doeval? eval(second(args), env): second(args): nil;
    obj z = nargs > 2? doeval? eval(third(args), env): third(args): nil;
    obj out;
    obj *lastp;

    switch (spec) {
    case STYPEOF:   return (obj){SYM, .text=typenames[x.type]};
    case SPRINT:
    case SREPR:     for (x = nil; consp(args); args = cdr(args)) {
                        x = eval(car(args), env);
                        pr(x, spec == SREPR);
                    }
                    return x;
    case SCAR:      return car(x);
    case SCDR:      return cdr(x);
    case SCONS:     return cons(x, y);
    case SLIST:     out = nil;
                    lastp = &out;
                    for ( ; consp(args); args = cdr(args)) {
                        *lastp = cons(eval(car(args), env), nil);
                        lastp = &lastp->cons->cdr;
                    }
                    return out;
    case SADD:      return intp(x) && intp(y)? num(x.n + y.n): nil;
    case SSUB:      return intp(x) && intp(y)? num(x.n - y.n): nil;
    case SMUL:      return intp(x) && intp(y)? num(x.n * y.n): nil;
    case SDIV:      return intp(x) && intp(y)? num(x.n / y.n): nil;
    case SREM:      return intp(x) && intp(y)? num(x.n % y.n): nil;
    case SORD:      return stringp(x)? num(x.text[0]): nil;
    case SCHR:      return intp(x)? chars[x.n & 0xff]: nil;
    case SCHARAT:   if (stringp(x) && intp(y)) {
                        int i = y.n < 0? y.n + x.n: y.n;
                        return 0 <= i && i < x.n ? chars[x.text[i]] : nil;
                    }
                    return nil;
    case SSTRLEN:   return stringp(x)? num(x.n): nil;
    case SSUBSTR:   if (stringp(x) && intp(y)) {
                        int i = y.n < 0? y.n + x.n: y.n;
                        int j = z.n < 0? z.n + x.n + 1: z.n;
                        if (0 <= i && i < x.n && i <= j && j <= x.n) {
                            char *buf = malloc(j - i + 1);
                            memcpy(buf, x.text + i, j - i);
                            buf[j - i] = 0;
                            return string(buf, j - i);
                        }
                        return nil;
                    }
                    return nil;
    case SIMPLODE:  {
                        char    *str = 0;
                        int     len = 0;
                        for (obj lst = x; consp(lst); lst = cdr(lst))
                            len += stringp(car(lst))? car(lst).n: 0;
                        if (len == 0)
                            return emptystring;
                        str = malloc(len + 1);
                        len = 0;
                        for (obj lst = x; consp(lst); lst = cdr(lst)) {
                            x = car(lst);
                            if (!stringp(x))
                                continue;
                            memcpy(str + len, x.text, x.n);
                            len += x.n;
                        }
                        str[len] = 0;
                        return string(str, len);
                    }
    case STOSYMBOL: return stringp(x)? symbol(x.text): nil;
    case STOSTRING: return symbolp(x)? string(x.text, strlen(x.text)): nil;
    case SASSOC:    return assoc(x, y);
    case SEQUAL:    return equal(x, y)? _true: nil;
    case SLT:       if (stringp(x) && stringp(y))
                        return strcmp(x.text, y.text) < 0? _true: nil;
                    return (intp(x) && intp(y) && x.n < y.n)? _true: nil;
    case SLE:       if (stringp(x) && stringp(y))
                        return strcmp(x.text, y.text) <= 0? _true: nil;
                    return (intp(x) && intp(y) && x.n <= y.n)? _true: nil;

    case SRDFILE:   if (!stringp(x)) return nil;
                    {
                        FILE *f = fopen(x.text, "rb");
                        if (!f) return nil;
                        fseek(f, 0L, SEEK_END);
                        int len = ftell(f);
                        rewind(f);
                        char *buf = malloc(len + 1);
                        fread(buf, 1, len, f);
                        fclose(f);
                        return string(buf, len);
                    }
    case SEXIT:     if (!intp(x)) { pr(x, false); puts(""); }
                    exit(intp(x)? x.n: 1);
    case SAPPLY:    if (functionp(x)) {
                        env = bindargs(call, x.fn->ps, y, env, x.fn->env, false);
                        return eval(x.fn->body, env);
                    } else if (specialp(x)) {
                        checkprocargs(call, x.spec, y);
                        return doprocedure(call, x.spec, y, env, false);
                    } else {
                        semantic(call, "non-function");
                        return nil;
                    }
    case SSETCAR:   return consp(x)? setcar(x, y): y;
    case SSETCDR:   return consp(x)? setcdr(x, y): y;
    default:        semantic(call, "special forms cannot be applied");
                    return nil;
    }
}
void resolvefn(obj fns, obj env) {
    for ( ; consp(fns); fns = cdr(fns))
        car(fns).fn->env = env;
}
obj eval(obj c, obj env) {
    obj f, newenv, ps, as, x, y, z;

top:
    switch (c.type) {

    case NIL: case INT: case STR: case FN: case SPEC:
        return c;

    case SYM:
        while (true)
            if (nullp(env)) semantic(c, "undefined");
            else if (equal(caar(env), c)) return cdar(env);
            else env = cdr(env);

    case CONS:
        f = eval(car(c), env);
        as = cdr(c);

        if (functionp(f)) {
            env = bindargs(c, f.fn->ps, as, env, f.fn->env, true);
            c = f.fn->body;
            goto top;
        }

        if (!specialp(f))
            semantic(c, "non-function");

        checkprocargs(c, f.spec, as);

        switch (f.spec) {

        default:        return doprocedure(c, f.spec, as, env, true);

        case SQUOTE:    return car(as);
        case SQUASI:    if (!consp(car(as))) return car(as);
                        {
                            obj lst = nil;
                            obj *lastp = &lst;
                            for (as = car(as); consp(as); as = cdr(as)) {
                                x = equal(caar(as), unquotesym)
                                    ? eval(car(cdar(as)), env)
                                    : car(as);
                                *lastp = cons(x, nil);
                                lastp = &lastp->cons->cdr;
                            }
                            return lst;
                        }
        case SBEGIN:    f = nil; // Keep track of functions
                        for ( ; consp(cdr(as)); as = cdr(as))
                            if (equal(definesym, caar(as))) {
                                obj id = car(cdar(as));
                                obj val = cdr(cdar(as));

                                if (consp(id)) { // Function definition
                                    val = nullp(cdr(val)) // wrap with BEGIN
                                        ? car(val)
                                        : cons(special(SBEGIN), val);
                                    val = lambda(cdr(id), val, env);
                                    id = car(id);
                                    f = cons(val, f);
                                } else
                                    val = eval(car(val), env);

                                env = cons(cons(id, val), env);
                            } else {
                                resolvefn(f, env);
                                eval(car(as), env);
                            }
                        resolvefn(f, env);
                        c = car(as);
                        goto top;
        case SIF:       c = nullp(eval(first(as), env))? third(as): second(as);
                        goto top;
        case SLET:      newenv = env;
                        for (c = cdr(c); consp(cdr(c)); c = cddr(c)) {
                            x = first(c);
                            y = second(c);
                            if (consp(x)) {
                                y = lambda(cdr(x), y, env);
                                x = car(x);
                            } else
                                y = eval(y, env);
                            newenv = cons(cons(x, y), newenv);
                        }
                        env = newenv;
                        c = car(c);
                        goto top;
        case SLETSTAR:  for (c = cdr(c); consp(cdr(c)); c = cddr(c)) {
                            x = first(c);
                            y = second(c);
                            if (consp(x)) {
                                y = lambda(cdr(x), y, env);
                                x = car(x);
                            } else
                                y = eval(y, env);
                            env = cons(cons(x, y), env);
                        }
                        c = car(c);
                        goto top;
        case SLETREC:   newenv = env;
                        for (c = cdr(c); consp(cdr(c)); c = cddr(c)) {
                            if (!consp(car(c)))
                                semantic(c, "LETREC only defines functions");
                            y = lambda(cdar(c), second(c), newenv);
                            newenv = cons(cons(caar(c), y), newenv);
                        }
                        for (obj ep = newenv; ep.cons != env.cons; ep = cdr(ep))
                            cdar(ep).fn->env = newenv;
                        env = newenv;
                        c = car(c);
                        goto top;
        case SLAMBDA:   x = car(as);
                        y = nullp(cddr(as))
                            ? cadr(as)
                            : cons(special(SBEGIN), cdr(as));
                        return lambda(x, y, env);
        case SCASE:     x = eval(car(as), env);
                        for (as = cdr(as); consp(as); as = cddr(as)) {
                            newenv = match(first(as), x, env);
                            if (!nullp(newenv)) {
                                env = newenv;
                                c = second(as);
                                goto top;
                            }
                        }
                        return nil;
        case SCOND:     for ( ; consp(as); as = cddr(as))
                            if (!nullp(eval(car(as), env))) {
                                c = cadr(as);
                                goto top;
                            }
                        return nil;
        case SAND:      for ( ; consp(cdr(as)); as = cdr(as))
                            if (nullp(eval(car(as), env))) return nil;
                        c = car(as);
                        goto top;
        case SOR:       for ( ; consp(cdr(as)); as = cdr(as))
                            if (!nullp((x = eval(car(as), env)))) return x;
                        c = car(as);
                        goto top;
        case SSET:      x = car(as);
                        y = eval(cadr(as), env);
                        while (true)
                            if (nullp(env)) semantic(x, "undefined");
                            else if (equal(caar(env), x))
                                return setcdr(car(env), y);
                            else env = cdr(env);
        }
    }
}

obj readtop() {
    obj body = nil;
    obj *p = &body;
    while (readtoken()) {
        *p = cons(readobj(token), nil);
        p = &p->cons->cdr;
    }
    return cons(special(SBEGIN), body);
}

int main(int argc, char **argv) {
    setvbuf(stdout, 0, _IONBF, 0);

    for (char **i = typenames; *i; i++) *i = intern(*i);
    for (int i = 0; specs[i].id; i++) specs[i].id = intern(specs[i].id);
    for (int i = 0; i < 256; i++) chars[i] = string(intern((char[2]){i, 0}), 1);

    _true = symbol("true");
    lambdasym = symbol("lambda");
    quotesym = symbol("quote");
    quasisym = symbol("quasiquote");
    unquotesym = symbol("unquote");
    definesym = symbol("define");
    emptystring = string("", 0);

    obj env = nil;
    for (int i = 0; specs[i].id; i++)
        env = cons(cons(symbol(specs[i].id), special(i)), env);

    for (char **fn = argv + 1; *fn; fn++) {
        opensource("std.lisp");
        obj code = readtop();

        opensource(*fn);
        obj yours = readtop();
        obj tail = code;
        while (consp(cdr(tail))) tail = cdr(tail);
        setcdr(tail, cons(yours, nil));

        obj x = eval(code, env);
        // printf("> "), pr(x, true), puts("");
    }
    puts("lisp: done.");
}
