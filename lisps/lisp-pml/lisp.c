#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define new(t,...) memcpy(malloc(sizeof(t)), &(t){__VA_ARGS__}, sizeof(t))

typedef struct string { int len; char chars[]; } string;
typedef struct obj obj;
struct obj {
    enum { NIL, BOOL, INT, CHAR, STR, SYM, CONS, FN, PROC } type;
    union {
        int x;
        string *s;
        struct cons *c;
        struct fn *f;
    };
};
struct cons { obj car, cdr; };
struct fn { obj params, body, env; };
enum { PADD, PSUB, PMUL, PDIV, PREM, PLESS, PLESSEQ, PPR, PQUOTE,
       PENLIST, PBEGIN, PFN, PSET, PAND, POR, PIF, PCOND, PEQUAL,
       PCAR, PCDR, PCONS, PSIZE, PCHARAT, PJOIN, PSUBSTR, PORD, PCHR,
       PVARIABLES, PEXIT, PREADF, };
struct proc_def { char *id; int flag; } proc_def[] = {
            {"+",1}, {"-",1}, {"*",1}, {"/",1}, {"rem",1}, {"<",1},
            {"<=",1},{"pr",1},{"quote",0},{"enlist",0},{"begin",2},
            {"fn",0},{"set",2},{"and",2},{"or",2},{"if",2},
            {"cond",2},{"equal?",1},{"car",1},{"cdr",1},
            {"cons",1},{"size",1},{"char-at",1},{"join",1},
            {"substr",1},{"ord",1},{"chr",1},{"variables",1},
            {"exit",1},{"read-file",1},{0}
        };

char    source[65536];
char    tokbuf[sizeof source];
char    *src;
char    *pun = "()[].' \t\n;";
char    *esc = "0\0" "a\a" "b\b" "e\033" "f\f" "n\n" "r\r" "t\t";
char    *sname;
int     ln;
int     token;
int     tokint;
string  *tokstr;
int     peeked;
string  *interns[65536];
int     ninterns;
obj     nil={NIL}, _true={BOOL, .x=1}, _false={BOOL, .x=0};
obj     quote, enlist, begin, define;

string *mkstr(char *chars, int len) {
    string *out = malloc(sizeof *out + len + 1);
    if (chars) memcpy(out->chars, chars, len);
    out->len = len;
    out->chars[len] = 0;
    return out;
}
string *intern(char *chars, int len) {
    if (len < 0) len = strlen(chars);
    for (string **i = interns; i < interns + ninterns; i++)
        if (len == (*i)->len && !memcmp((*i)->chars, chars, len)) return *i;
    return interns[ninterns++] = mkstr(chars, len);
}
obj newint(int x) { return (obj){INT, .x=x}; }
obj newchar(int x) { return (obj){CHAR, .x=x}; }
obj newstr(string *s) { return (obj){STR, .s=s}; }
obj newsym(string *s) { return (obj){SYM, .s=s}; }
obj cons(obj car, obj cdr) {
    return (obj){CONS, .c=new(struct cons, car, cdr)};
}
obj newfn(obj params, obj body, obj env) {
    return (obj){FN, .f=new(struct fn, params, body, env)};
}
obj newproc(int x) { return (obj){PROC, .x=x}; }
#define null(o) ((o).type == NIL)
#define boolp(o) ((o).type == BOOL)
#define charp(o) ((o).type == CHAR)
#define intp(o) ((o).type == INT)
#define strp(o) ((o).type == STR)
#define consp(o) ((o).type == CONS)
#define fnp(o) ((o).type == FN)
#define car(o) (consp(o)? (o).c->car: nil)
#define cdr(o) (consp(o)? (o).c->cdr: nil)
#define caar(o) car(car(o))
#define cadr(o) car(cdr(o))
#define cdar(o) cdr(car(o))
#define cddr(o) cdr(cdr(o))
bool truth(obj o) { return !null(o) && (!boolp(o) || o.x); }

void pr(obj o) {
    switch (o.type) {
    case NIL:   fputs("()", stdout); break;
    case BOOL:  fputs(o.x? "true": "false", stdout); break;
    case CHAR:  putchar(o.x); break;
    case INT:   printf("%d", o.x); break;
    case STR:   fwrite(o.s->chars, o.s->len, 1, stdout); break;
    case SYM:   fwrite(o.s->chars, o.s->len, 1, stdout); break;
    case CONS:  putchar('(');
                for ( ; consp(o); o = cdr(o))
                    pr(car(o)),
                    (consp(cdr(o))? putchar(' '): 0);
                if (!null(o)) fputs(" . ", stdout), pr(o);
                putchar(')'); break;
    case FN:    fputs("#fn", stdout); break;
    case PROC:  fputs(proc_def[o.x].id, stdout); break;
    }
}
bool equal(obj a, obj b) {
    if (a.type != b.type) return false;
    switch (a.type) {
    case NIL:   return true;
    case BOOL:  return a.x == b.x;
    case CHAR:  return a.x == b.x;
    case INT:   return a.x == b.x;
    case STR:   if (a.s == b.s) return true;
                return a.s->len == b.s->len &&
                       !memcmp(a.s->chars, b.s->chars, a.s->len);
    case SYM:   return a.s == b.s;
    case CONS:  return equal(car(a), car(b)) && equal(cdr(a), cdr(b));
    case FN:    return a.f == b.f;
    case PROC:  return a.x == b.x;
    }
}
int compare(obj a, obj b) {
    if (a.type != b.type) return a.type - b.type;
    switch (a.type) {
    case NIL:   return 0;
    case BOOL:  return a.x - b.x;
    case CHAR:  return a.x - b.x;
    case INT:   return a.x - b.x;
    case STR:   if (a.s == b.s) return true;
                int r, len;
                len = a.s->len < b.s->len? a.s->len: b.s->len;
                if ((r = memcmp(a.s->chars, b.s->chars, len))) return r;
                return a.s->len - b.s->len;
    case SYM:   return a.s - b.s;
    case CONS:  return a.c - b.c;
    case FN:    return a.f - b.f;
    case PROC:  return a.x - b.x;
    }
}

bool open_source(char *fn) {
    src = source, ln = 1, sname = intern(fn, -1)->chars, peeked = false;
    FILE *file = fopen(fn, "rb");
    if (!file) return false;
    source[fread(source, 1, sizeof source, file)] = 0;
    return fclose(file), true;
}
int chr(void) {
    if (!*src) printf("lisp: %s:%d: unclosed string\n", sname, ln), exit(1);
    if (*src == '\n') return ln++, *src++;
    if (*src != '\\') return *src++;
    src++;
    for (char *e = esc; e[0]; e += 2) if (*src == e[0]) return *src++, e[1];
    return *src++;
}
int next(void) {
    char    *s = tokbuf;
    if (peeked) return peeked = false, token;
    while (true)
        if (*src == '\n') ln++, src++;
        else if (isspace(*src)) src++;
        else if (*src == ';') while (*src && *src != '\n') src++;
        else break;
    if (!*src) return token = 0;
    if (strchr(pun, *src)) return token = *src++;
    if (isdigit(src[*src == '-']))
        return tokint = strtol(src, &src, 10), token = 'i';
    if (*src == '#') return src++, tokint = chr(), token = 'c';
    if (*src == '"')
        for (src++; ; )
            if (*src == '"') return src++,
                                    tokstr = intern(tokbuf, s - tokbuf),
                                    token = 's';
            else *s++ = chr();
    for (s = src; *src && !strchr(pun, *src); src++);
    tokstr = intern(s, src - s);
    if (!strcmp("true", tokstr->chars)) return token = 't';
    if (!strcmp("false", tokstr->chars)) return token = 'f';
    return token = 'a';
}
bool want(int tok) { return !(peeked = next() != tok); }
obj read(void);
obj read_list(int delim) {
    if (want(0)) printf("lisp: %s:%d: unclosed list\n", sname, ln), exit(1);
    if (want(delim)) return nil;
    obj car = read();
    if (want('.')) {
        obj cdr = read();
        if (!want(delim)) printf("lisp: %s:%d: bad dot\n", sname, ln), exit(1);
        return cons(car, cdr);
    }
    return cons(car, read_list(delim));
}
obj read(void) {
    switch (next()) {
    case 't': return _true;
    case 'f': return _false;
    case 'i': return newint(tokint);
    case 'c': return newchar(tokint);
    case 's': return newstr(tokstr);
    case 'a': return newsym(tokstr);
    case '(': return read_list(')');
    case '[': return cons(enlist, read_list(']'));
    case '\'': return cons(quote, cons(read(), nil));
    default:  printf("lisp: %s:%d: misplaced %c\n", sname, ln, token), exit(1);
    }
}
obj eval(obj c, obj env);
obj optional_begin(obj c) { return null(cdr(c))? car(c): cons(begin, c); }
obj _join(obj list) {
    int     size = 0, o = 0;
    for (obj i = list; consp(i); i = cdr(i))
        size += strp(car(i))? car(i).s->len:
                charp(car(i))? 1: 0;
    string  *out = mkstr(0, size);
    for (obj i = list; consp(i); i = cdr(i)) {
        if (charp(car(i))) out->chars[o++] = car(i).x;
        if (!strp(car(i))) continue;
        string *s = car(i).s;
        memcpy(out->chars + o, s->chars, s->len);
        o += s->len;
    }
    return newstr(out);
}
obj _substr(string *s, int i, int j) {
    if (i < 0 || j < i || i >= s->len || j > s->len) return nil;
    return newstr(mkstr(s->chars + i, j - i));
}
obj _readf(string *filename) {
    static char tmp[65536];
    FILE *file = fopen(filename->chars, "rb");
    if (!file) return nil;
    int len = fread(tmp, 1, sizeof tmp, file);
    fclose(file);
    return newstr(mkstr(tmp, len));
}
obj eval_proc(int proc, obj *as, int n, obj env) {
    #define A as[0]
    #define B as[1]
    #define C as[2]
    obj     out, *p, i, body;
    switch (proc) {
    case PADD:  return newint(A.x + B.x);
    case PSUB:  return newint(A.x - B.x);
    case PMUL:  return newint(A.x * B.x);
    case PDIV:  return newint(A.x / B.x);
    case PREM:  return newint(A.x % B.x);
    case PLESS: return compare(A, B) < 0? _true: _false;
    case PLESSEQ:return compare(A, B) <= 0? _true: _false;
    case PPR:   return pr(A), A;
    case PQUOTE:return car(A);
    case PENLIST:   out = nil, p = &out;
                    for (i = A; consp(i); i = cdr(i))
                        *p = cons(eval(car(i), env), nil),
                        p = &(*p).c->cdr;
                    if (!null(i)) *p = eval(i, env);
                    return out;
    case PFN:   return newfn(car(A), optional_begin(cdr(A)), env);
    case PEQUAL:return equal(A, B)? _true: _false;
    case PCAR:  return car(A);
    case PCDR:  return cdr(A);
    case PCONS: return cons(A, B);
    case PSIZE: return strp(A)? newint(A.s->len): nil;
    case PCHARAT: return strp(A) && intp(B) && B.x < A.s->len
                    ? newchar(A.s->chars[B.x]): nil;
    case PJOIN: return _join(A);
    case PSUBSTR:
        return strp(A) && intp(B) && intp(C)? _substr(A.s, B.x, C.x): nil;
    case PORD:  return charp(A)? newint(A.x): nil;
    case PCHR:  return intp(A)? newchar(A.x): nil;
    case PVARIABLES: return env;
    case PEXIT: exit(intp(A)? A.x: 0);
    case PREADF: return strp(A)? _readf(A.s): nil;
    }
}
bool handle_def(obj c, obj *env) {
    if (!consp(c)) return false;
    if (!equal(car(c), define)) return false;
    obj a = cdr(c);
    obj id = consp(car(a))? caar(a): car(a);
    obj value = consp(car(a))? newfn(cdar(a), optional_begin(cdr(a)), *env)
                             : eval(cadr(a), *env);
    *env = cons(cons(id, value), *env);
    return true;
}
obj eval(obj c, obj env) {
    top:
    if (c.type == SYM) {
        for (obj e = env; consp(e); e = cdr(e))
            if (equal(c, caar(e))) return cdar(e);
        return nil;
    }
    if (c.type != CONS) return c;
    obj     f = eval(car(c), env);
    obj     a = cdr(c);
    if (f.type == PROC) {
        obj     as[8], out, x, id;
        int     n = 0;
        switch (proc_def[f.x].flag) {
        case 0:     return eval_proc(f.x, &a, n, env);
        case 1:     for (n = 0; consp(a) && n < 8; a = cdr(a))
                        as[n++] = eval(car(a), env);
                    while (consp(a)) eval(car(a), env);
                    return eval_proc(f.x, as, n, env);
        case 2:     break;
        }
        switch (f.x) {
        case PIF:       c = truth(eval(car(a), env))? cadr(a): car(cddr(a));
                        goto top;
        case PCOND:     for ( ; consp(a); a = cdr(a))
                            if (truth(eval(caar(a), env))) {
                                c = optional_begin(cdar(a));
                                goto top;
                            }
                        return nil;
        case PBEGIN:    if (null(a)) return nil;
                        out = nil;
                        while (consp(cdr(a)))
                            if (handle_def(car(a), &env)) {
                                obj old = cdr(env);
                                a = cdr(a);
                                while (handle_def(car(a), &env)) a = cdr(a);
                                for (obj i = env; i.c != old.c; i = cdr(i))
                                    if (fnp(cdar(i))) cdar(i).f->env = env;
                            } else
                                eval(car(a), env),
                                a = cdr(a);
                        c = car(a);
                        goto top;
        case PSET:      id = car(a), x = eval(cadr(a), env);
                        for (obj e = env; consp(e); e = cdr(e))
                            if (equal(id, caar(e))) return car(e).c->cdr = x;
                        return x;
        case PAND:      for ( ; consp(cdr(a)); a = cdr(a))
                            if (!truth(x = eval(car(a), env))) return x;
                        c = consp(a)? car(a): _true;
                        goto top;
        case POR:       for ( ; consp(cdr(a)); a = cdr(a))
                            if (truth(x = eval(car(a), env))) return x;
                        c = consp(a)? car(a): _false;
                        goto top;
        }
    }
    if (f.type != FN) return nil;
    obj     p = f.f->params;
    obj     old = env;
    env = f.f->env;
    c = f.f->body;
    for ( ; consp(p) && consp(a); p = cdr(p), a = cdr(a))
        env = cons(cons(car(p), eval(car(a), old)), env);
    for ( ; consp(a); a = cdr(a)) eval(car(a), old);
    for ( ; consp(p); p = cdr(p)) cons(cons(car(p), nil), env);
    goto top;
}

int main(int argc, char **argv) {
    obj env = nil;
    quote = newsym(intern("quote", -1));
    enlist = newsym(intern("enlist", -1));
    begin = newsym(intern("begin", -1));
    define = newsym(intern("define", -1));
    for (int i = 0; proc_def[i].id; i++) {
        string *id = intern(proc_def[i].id, -1);
        proc_def[i].id = id->chars;
        env = cons(cons(newsym(id), newproc(i)), env);
    }
    if (!open_source(argv[1])) printf("lisp: %s: cannot open\n", argv[1]),
                                exit(1);
    obj code = cons(begin, nil), *p = &code.c->cdr;
    while (peeked = next())
        *p = cons(read(), nil), p = &(*p).c->cdr;
    eval(code, env);
    puts("done.");
}
