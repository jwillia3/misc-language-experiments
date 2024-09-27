#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct { int len; char txt[]; } string;
typedef struct obj obj;

struct obj {
    enum { NIL, INT, SYM, STR, CONS } type;
    union {
        int num;
        string *str, *sym;
        struct cons *cons;
    };
};

struct cons { obj car, cdr; };

obj nil, quote, lambda, tr, cond, let, letrec, begin;
obj carsym, cdrsym, conssym, prsym, equalsym, addsym;
int ninterns;
string *interns[65536];
char source[65536];
char *srcp = source;
int line = 1;

string *mkstr(const char *txt, int len) {
    if (len < 0) len = strlen(txt);
    string *str = malloc(sizeof *str + len + 1);
    str->len = len;
    str->txt[len] = 0;
    if (txt) memcpy(str->txt, txt, len);
    return str;
}

string *intern(const char *txt, int len) {
    if (len < 0) len = strlen(txt);
    for (int i = 0; i < ninterns; i++)
        if (interns[i]->len == len && !memcmp(interns[i]->txt, txt, len))
            return interns[i];
    return interns[ninterns++] = mkstr(txt, len);
}

obj newint(int num) { return (obj) { INT, .num = num }; }
obj newsym(string *sym) { return (obj) { SYM, .sym = sym }; }
obj newstr(string *str) { return (obj) { STR, .str = str }; }

obj car(obj o) { return o.type == CONS? o.cons->car: nil; }
obj cdr(obj o) { return o.type == CONS? o.cons->cdr: nil; }
obj caar(obj o) { return car(car(o)); }
obj cadr(obj o) { return car(cdr(o)); }
obj cdar(obj o) { return cdr(car(o)); }
obj cddr(obj o) { return cdr(cdr(o)); }
obj cadar(obj o) { return car(cdar(o)); }
obj cddar(obj o) { return cdr(cdar(o)); }
obj caddr(obj o) { return car(cddr(o)); }
obj cdddr(obj o) { return cdr(cddr(o)); }
obj caadr(obj o) { return car(cadr(o)); }
obj cdadr(obj o) { return cdr(cadr(o)); }

obj cons(obj car, obj cdr) {
    struct cons *cons = malloc(sizeof *cons);
    *cons = (struct cons) { car, cdr };
    return (obj) { CONS, .cons = cons };
}

int pr(obj o) {
    obj l;

    switch (o.type) {
    case NIL:
        return fputs("()", stdout);
    case INT:
        return printf("%d", o.num);
    case SYM:
        return fputs(o.sym->txt, stdout);
    case STR:
        return fwrite(o.sym->txt, 1, o.sym->len, stdout);
    case CONS:
        fputc('(', stdout);

        for (l = o; l.type == CONS; l = cdr(l)) {
            pr(car(l));
            if (cdr(l).type == CONS) fputc(' ', stdout);
        }

        if (l.type != NIL)
            fputs(" . ", stdout),
            pr(l);

        return fputc(')', stdout);
    }
    return 0;
}

bool equal(obj a, obj b) {
    if (a.type != b.type) return false;

    switch (a.type) {
    case NIL:
        return true;
    case INT:
        return a.num == b.num;
    case SYM:
        return a.sym == b.sym;
    case STR:
        if (a.str == b.str) return true;
        if (a.str->len != b.str->len) return false;
        return memcmp(a.str->txt, b.str->txt, a.str->len);
    case CONS:
        return equal(car(a), car(b)) && equal(cdr(a), cdr(b));
    }
    return false;
}

obj assoc(obj ls, obj key) {
    if (ls.type != CONS) return nil;
    if (equal(caar(ls), key)) return cdar(ls);
    return assoc(cdr(ls), key);
}

void fatal(char *msg) {
    printf("error %d: %s\n", line, msg);
    exit(1);
}

int space(void) {
    while (isspace(*srcp) || *srcp == ';')
        if (*srcp == '\n') srcp++, line++;
        else if (*srcp == ';') while (*srcp && *srcp != '\n') srcp++;
        else srcp++;
    return *srcp;
}

obj read(void) {

    retry:

    space();

    char *base = srcp;

    if (*srcp == 0) return nil;

    if (isdigit(srcp[*srcp == '-']))
        return newint(strtol(srcp, &srcp, 10));

    while (*srcp && !strchr("()[]\"'. \t\n", *srcp)) srcp++;
    if (srcp != base)
        return newsym(intern(base, srcp - base));

    if (*srcp == '"') {
        char *base = srcp++;
        char *p = base;

        while (true)
            if (*srcp == '"') {
                srcp++;
                return newstr(intern(base, p - base));
            }
            else if (*srcp == 0) fatal("unclosed string");
            else if (*srcp == '\\')
                switch ((srcp +=2)[-1]) {
                case 'a': *p++ = '\a'; break;
                case 'b': *p++ = '\b'; break;
                case 'e': *p++ = '\033'; break;
                case 'f': *p++ = '\f'; break;
                case 'n': *p++ = '\n'; break;
                case 'r': *p++ = '\r'; break;
                case 't': *p++ = '\t'; break;
                case 'v': *p++ = '\v'; break;
                default: *p++ = srcp[-1];
                }
            else *p++ = *srcp++;

    }

    if (*srcp == '\'') {
        srcp++;
        return cons(quote, cons(read(), nil));
    }

    if (*srcp == '(') {
        obj ls = nil;
        obj *p = &ls;

        srcp++;

        while (space()) {
            if (*srcp == ')') break;

            if (*srcp == '.') {
                srcp++;
                space();
                *p = read();
                space();
                break;
            }

            *p = cons(read(), nil);
            p = &(*p).cons->cdr;
        }

        if (*srcp != ')') fatal("missing paren");
        srcp++;

        return ls;
    }

    return fatal("bad token"), nil;
}

obj eval(obj c, obj env);

void eval_ls(obj as, obj env, obj *out, int n) {
    for (int i = 0; i < n; i++) {
        out[i] = eval(car(as), env);
        as = cdr(as);
    }
    for ( ; as.type == CONS; as = cdr(as))
        eval(car(as), env);
}

obj definition(obj def, obj env) {
    if (car(def).type == CONS) { // ((<FN-ID> <PARAMS>) <BODY>...)
        obj id = caar(def);
        obj ps = cdar(def);
        obj body = cddr(def).type == NIL? cadr(def): cons(begin, cdr(def));
        obj val = cons(lambda, cons(env, cons(ps, cons(body, nil))));
        return cons(cons(id, val), env);
    }
    return cons(cons(car(def), eval(cadr(def), env)), env);
}

obj eval(obj c, obj env) {

    tail_call:

    switch (c.type) {
    case NIL:
    case INT:
    case STR:
        return c;

    case SYM:
        return assoc(env, c);

    case CONS:
        {
            obj xs[4];
            obj f = eval(car(c), env);
            obj as = cdr(c);

            if (f.type == SYM) { // Special form.

                if (f.sym == quote.sym) // (QUOTE <X>)
                    return car(as);

                if (f.sym == lambda.sym) // (LAMBDA <PARAMS> <BODY>)
                    return cons(f, cons(env, as));

                if (f.sym == equalsym.sym) // (EQUAL <VALUE> <VALUE>)
                    return eval_ls(as, env, xs, 2), equal(*xs, xs[1])? tr: nil;

                if (f.sym == addsym.sym) { // (ADD <VALUE>...)
                    int sum = 0;
                    for ( ; as.type == CONS; as = cdr(as)) {
                        obj x = eval(car(as), env);
                        if (x.type == INT)
                            sum += x.num;
                    }
                    return newint(sum);
                }

                if (f.sym == cond.sym) { // (COND (<TEST> <BODY>) ...)
                    for ( ; as.type == CONS; as = cdr(as))
                        if (eval(caar(as), env).type != NIL) {
                            c = cadar(as);
                            goto tail_call;
                        }
                    return nil;
                }

                if (f.sym == begin.sym) { // (BEGIN <VALUE>...)
                    for ( ; cdr(as).type != NIL; as = cdr(as))
                        eval(car(as), env);
                    c = car(as);
                    goto tail_call;
                }

                if (f.sym == let.sym) { // (LET (<NAME> <VALUE>)... <BODY>)
                    for ( ; cdr(as).type != NIL; as = cdr(as))
                        env = definition(car(as), env);
                    c = car(as);
                    goto tail_call;
                }

                if (f.sym == letrec.sym) { // (LETREC (<NAME> <VALUE>)... <BODY>)
                    int n = 0;
                    for ( ; cdr(as).type != NIL; as = cdr(as)) {
                        n++;
                        env = definition(car(as), env);
                    }

                    for (obj i = env; n--; i = cdr(i)) {
                        obj x = cdar(i);
                        if (equal(car(x), lambda))
                            cdr(x).cons->car = env;
                    }


                    c = car(as);
                    goto tail_call;
                }

                if (f.sym == carsym.sym) // (CAR <CONS>)
                    return eval_ls(as, env, xs, 1), car(*xs);

                if (f.sym == cdrsym.sym) // (CDR <CONS>)
                    return eval_ls(as, env, xs, 1), cdr(*xs);

                if (f.sym == conssym.sym) // (CONS <CAR> <CDR>)
                    return eval_ls(as, env, xs, 2), cons(xs[0], xs[1]);

                if (f.sym == prsym.sym) // (PR <VALUE>)
                    return eval_ls(as, env, xs, 1), pr(*xs), *xs;
            }

            if (f.type != CONS)
                return nil;

            obj newenv = cadr(f);
            obj ps = caddr(f);
            obj body = cadr(cddr(f));

            for ( ; ps.type == CONS && as.type == CONS; ps=cdr(ps), as=cdr(as))
                newenv = cons(cons(car(ps), eval(car(as), env)), newenv);

            if (ps.type != NIL) { // Rest args.
                obj ls = nil;
                obj *p = &ls;
                for ( ; as.type == CONS; as = cdr(as)) {
                    *p = cons(eval(car(as), env), nil);
                    p = &(*p).cons->cdr;
                }
                newenv = cons(cons(ps, ls), newenv);
            }

            for ( ; ps.type == CONS; ps = cdr(ps)) // Fill missing args.
                newenv = cons(cons(car(ps), nil), newenv);
            for ( ; as.type == CONS; as = cdr(as)) // Exhaust excess args.
                eval(car(as), env);

            c = body;
            env = newenv;
            goto tail_call;
        }
    }
    return nil;
}

int main(int argc, char **argv) {
    quote = newsym(intern("QUOTE", -1));
    lambda = newsym(intern("LAMBDA", -1));
    carsym = newsym(intern("CAR", -1));
    cdrsym = newsym(intern("CDR", -1));
    conssym = newsym(intern("CONS", -1));
    cond = newsym(intern("COND", -1));
    let = newsym(intern("LET", -1));
    letrec = newsym(intern("LETREC", -1));
    begin = newsym(intern("BEGIN", -1));
    tr = newsym(intern("TRUE", -1));
    prsym = newsym(intern("PR", -1));
    equalsym = newsym(intern("EQUAL", -1));
    addsym = newsym(intern("ADD", -1));

    obj env = nil;

    env = cons(cons(newsym(intern("FALSE", -1)), nil), env);
    env = cons(cons(newsym(intern("NIL", -1)), nil), env);
    env = cons(cons(tr, tr), env);
    env = cons(cons(prsym, prsym), env);
    env = cons(cons(equalsym, equalsym), env);
    env = cons(cons(addsym, addsym), env);
    env = cons(cons(quote, quote), env);
    env = cons(cons(lambda, lambda), env);
    env = cons(cons(carsym, carsym), env);
    env = cons(cons(cdrsym, cdrsym), env);
    env = cons(cons(conssym, conssym), env);
    env = cons(cons(cond, cond), env);
    env = cons(cons(let, let), env);
    env = cons(cons(letrec, letrec), env);
    env = cons(cons(begin, begin), env);

    fread(source, 1, sizeof source, stdin);

    eval(read(), env);
}
