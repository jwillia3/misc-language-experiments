#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define new(type, ...)\
    memcpy(malloc(sizeof(obj)), &(obj){type, __VA_ARGS__}, sizeof(obj))
typedef struct obj {
    enum { UNDEF,NIL,INT,STR,SYM,DATA,CONS,CAT,APP,FN,REC,RULE,PROC, } type;
    union { int n; char *s; struct { void *a, *b, *c; };
        struct { char *pname; struct obj *(*proc)(struct obj *x); };
    };
} obj;

typedef enum { TEOF,TLP,TRP,TLB,TRB,TCOMMA,TSEMI,TFN,TINT,TSTR,TCNAME,TNAME,
        TIF,TTHEN,TELSE,TLET,TREC,TIN,TCASE,TARR,TBAR,TEQ,TCOLON,TCAT,TAND,
        } Token;
char    *toks[] = {"eof","(",")","[","]",",",";","\\","int","str","cname",
        "name","if","then","else","let","rec","in","case","->","|","=",":","^",
        "and", 0};
char    source[65536], tokbuf[65536], *interns[65536], *src, *tokstr;
int     line, ninterns, token, peeked, tokint;
obj     *undef, *nil, *tru, *fal, *chars[256];

void *syntax(char *msg) {printf("error %d: %s\n", line, msg); exit(1); }
char *intern(char *s) {
    for (int i = 0; i < ninterns; i++)
        if (!strcmp(interns[i], s)) return interns[i];
    return interns[ninterns++] = strdup(s);
}
int next() {
    char *t = tokbuf, *opchr = "!$%&*+-./:<=>?@^|~";
    if (peeked) return peeked = false, token;
    for ( ; isspace(*src) || *src == '#'; src++)
        if (*src == '\n') line++;
        else if (*src == '#') while (src[1] && src[1] != '\n') src++;
    if (!*src) return token = TEOF;
    for (Token i = TEOF + 1; i < TINT; i++)
        if (*src == toks[i][0]) return src++, token = i;
    if (isdigit(src[*src == '-']))
        return tokint = strtol(src, &src, 0), token = TINT;
    if (*src == '"')
        for (char quote = *src++; ; src++)
            if (*src == 0 || *src == '\n') syntax("unclosed string");
            else if (*src == quote)
                return src++, *t = 0, tokstr = intern(tokbuf), token = TSTR;
            else if (*src != '\\') *t++ = *src;
            else switch (*++src) {
            case 'n':   *t++ = '\n'; break;
            case 't':   *t++ = '\t'; break;
            default:    *t++ = *src;
            }
    while (isalnum(*src) || *src == '_' || *src == '\'') *t++ = *src++;
    if (t == tokbuf) while (*src && strchr(opchr, *src)) *t++ = *src++;
    if (t == tokbuf) syntax("bad token");
    *t = 0, tokstr = intern(tokbuf);
    for (Token i = TNAME + 1; toks[i]; i++)
        if (!strcmp(toks[i], tokstr)) return token = i;
    return token = isupper(*tokstr)? TCNAME: TNAME;
}
bool peek(Token t) { return peeked = next(), token == t; }
bool want(Token t) { return peeked = next() != t, token == t; }
void need(Token t, char *msg) { if (!want(t)) syntax(msg); }
obj *suffix(obj *x, int t, char *msg) { need(t, msg); return x; }
obj *num(int n) { return new(INT, .n=n); }
obj *str(char *s) { return new(STR, .s=s); }
obj *sym(char *s) { return new(SYM, .s=s); }
obj *data(char *tag, obj *arg) { return new(DATA, .a=tag, arg); }
obj *cons(obj *hd, obj *tl) { return new(CONS, .a=hd, tl); }
obj *app(obj *f, obj *x) { return new(APP, .a=f, x); }
obj *rec(obj *id, obj *x, obj *in) { return new(REC, .a=id, x, in); }
obj *fn(obj *p, obj *e, obj *env) { return new(FN, .a=p, e, env); }
obj *rule(obj *l, obj *r, obj *n) { return new(RULE, .a=l, r, n); }
obj *proc(char *id, obj *(p)(obj*)) { return new(PROC, .pname=id, p); }
bool is(int type, obj *o) { return o->type == type; }
obj *hd(obj *o) { return is(CONS, o)? o->a: undef; }
obj *tl(obj *o) { return is(CONS, o)? o->b: undef; }
obj *expr(), *bexpr(bool required);
obj *fexpr(int delim) {
    if (want(delim)) return expr();
    obj *par = bexpr(true);
    obj *body = fexpr(delim);
    return fn(par, body, nil);
}
obj *lexpr(obj *lhs, Token delim) {
    if (want(TCOMMA)) return cons(lhs, lexpr(expr(), delim));
    return need(delim, "seq not closed"), delim == TRB? cons(lhs, nil): lhs;
}
obj *bexpr(bool required) {
    if (want(TCNAME)) return data(tokstr, undef);
    if (want(TNAME)) return sym(tokstr);
    if (want(TINT)) return num(tokint);
    if (want(TSTR)) return str(tokstr);
    if (want(TLP)) return want(TRP)? nil: lexpr(expr(), TRP);
    if (want(TLB)) return want(TRB)? nil: lexpr(expr(), TRB);
    if (want(TFN)) return fexpr(TARR);
    return required? syntax("need expression"): 0;
}
obj *cexpr() {
    obj     *f = bexpr(true), *x;
    while ((x = bexpr(false))) f = app(f, x);
    return  want(TCOLON)? cons(f, cexpr()):
            want(TCAT)? new(CAT, .a=f, cexpr()):
            want(TSEMI)? app(fn(sym(intern("_")), cexpr(), nil), f):
            f;
}
obj *rules() {
    if (!want(TBAR)) return undef;
    obj     *lhs = suffix(bexpr(true), TARR, "need ->");
    obj     *rhs = expr();
    return rule(lhs, rhs, rules());
}
obj *definition() {
    bool    recur = want(TREC);
    obj     *lhs = bexpr(true);
    obj     *rhs = fexpr(TEQ);
    obj     *in = want(TAND)? definition(): (need(TIN, "need in"), expr());
    return recur? rec(lhs, rhs, in): app(fn(lhs, in, nil), rhs);
}
obj *expr() {
    if (want(TIF)) {
        obj     *subject = suffix(expr(), TTHEN, "then");
        obj     *t = suffix(expr(), TELSE, "else");
        obj     *f = expr();
        return app(rule(tru, t, rule(fal, f, undef)), subject);
    } else if (want(TCASE)) {
        obj     *subject = expr();
        return app(rules(), subject);
    } else if (want(TLET)) return definition();
    else return cexpr();
}
obj *pr(obj *o, bool paren) {
    if (paren && o->type >= CONS)
        return putchar('('), pr(o, false), putchar(')'), o;
    switch (o->type) {
    case UNDEF: printf("undef"); break;
    case NIL:   printf("()"); break;
    case INT:   printf("%d", o->n); break;
    case STR:   printf("\"%s\"", o->s); break;
    case SYM:   printf("%s", o->s); break;
    case CONS:  for ( ; is(CONS, o); o = tl(o)) pr(hd(o), true), putchar(':');
                pr(o, true); break;
    case CAT:   pr(o->a, true), printf(" "), pr(o->b, !is(CAT, o->b)); break;
    case DATA:  printf("%s", o->a);
                if (!is(UNDEF, o->b)) printf(" "), pr(o->b, 1); break;
    case APP:   pr(o->a, !is(APP, o->a)), putchar(' '), pr(o->b, 1); break;
    case FN:    printf("\\"), pr(o->a, true), printf("->");
                pr(o->b, !is(FN, o->b)); break;
    case REC:   printf("let rec "), pr(o->a, true);
                printf(" = "), pr(o->b, false);
                printf(" in "), pr(o->c, false); break;
    case RULE:  printf(" | "), pr(o->a, true);
                printf(" -> "), pr(o->b, true);
                if (is(RULE, o->c)) pr(o->c, false); break;
    case PROC:  printf("%s", o->pname); break;
    } return o;
}
static inline obj *assoc(obj *c, obj *e) {
    return  is(NIL, e)? undef:
            hd(hd(e))->s == c->s? tl(hd(e)):
            assoc(c, tl(e));
}
obj *equal(obj *a, obj *b) {
    return  a->type != b->type? fal:
            is(UNDEF, a)? tru:
            is(NIL, a)? tru:
            is(INT, a) && a->n == b->n? tru:
            is(STR, a) && !strcmp(a->s, b->s)? tru:
            is(SYM, a) && a->s == b->s? tru:
            is(CONS, a) && equal(hd(a), hd(b)) && equal(tl(a), tl(b))? tru:
            is(DATA, a) && a->a == b->a && equal(a->b, b->b)? tru:
            fal;
}
obj *bindval(obj *p, obj *x, obj *env) {
    return  env == undef? undef:
            is(SYM, p)? cons(cons(p, x), env):
            is(CONS, p) && is(CONS, x)?
                bindval(tl(p), tl(x), bindval(hd(p), hd(x), env)):
            is(CAT, p) && is(STR, x) && *x->s?
                bindval(p->a, chars[*x->s], bindval(p->b, str(x->s + 1), env)):
            is(APP, p)? bindval(data(((obj*)p->a)->a, p->b), x, env):
            is(DATA, p)? bindval(p->b, x->b, p->a == x->a? env: undef):
            equal(p, x) == tru? env: undef;
}
obj *eval(obj *c, obj *env) {
    top:
    if (is(SYM, c)) return assoc(c, env);
    else if (is(CONS, c)) return cons(eval(hd(c), env), eval(tl(c), env));
    else if (is(FN, c)) return fn(c->a, c->b, env);
    else if (is(APP, c)) {
        obj *f = eval(c->a, env), *x = eval(c->b, env);
        if (is(UNDEF, x)) return x;
        if (is(FN, f)) { env = bindval(f->a, x, f->c); c = f->b; goto top; }
        if (is(RULE, f))
            for (obj *r = f; is(RULE, r); r = r->c) {
                obj *newenv = bindval(r->a, x, env);
                if (!is(UNDEF, newenv)) { env = newenv; c = r->b; goto top; }
            }
        if (is(PROC, f)) return f->proc(x);
        if (is(DATA, f) && is(UNDEF, f->b)) return data(f->a, x);
        return undef;
    } else if (is(REC, c)) {
        env = cons(cons(c->a, undef), env);
        hd(env)->b = eval(c->b, env);
        c = c->c; goto top;
    } else if (is(CAT, c)) {
        obj *a = eval(c->a, env), *b = eval(c->b, env);
        if (!is(STR, a) || !is(STR, b)) return undef;
        int lena = strlen(a->s), lenb = strlen(b->s);
        return str(strcat(strcpy(malloc(lena + lenb + 1), a->s), b->s));
    } else return c;
}
obj *_add(obj *x) {
    return is(INT, hd(x)) && is(INT, tl(x))? num(hd(x)->n + tl(x)->n): undef;
}
obj *_equal(obj *x) { return equal(hd(x), tl(x)); }
obj *_less(obj *x) {
    return is(INT,hd(x)) && is(INT,tl(x))? hd(x)->n < tl(x)->n? tru: fal: undef;
}
obj *_ord(obj *x) { return is(STR, x)? num((unsigned) *x->s): undef; }
obj *_chr(obj *x) { return is(INT, x)? chars[x->n & 255]: undef; }
obj *_pr(obj *x) {
    if (is(STR, x)) printf("%s", x->s); else pr(x, false);
    fflush(stdout);
    return x;
}
obj *def(char *id, obj *p(obj*), obj *env) {
    return cons(cons(sym(intern(id)), proc(intern(id), p)), env);
}
int main(int argc, char **argv) {
    for (int i = 0; i < 256; i++) chars[i] = str(intern((char[]){ i, 0 }));
    undef = new(UNDEF), nil = new(NIL),
    tru = data(intern("True"), undef), fal = data(intern("False"), undef);
    FILE *file = fopen(argv[1], "r");
    if (!file) syntax("cannot open source");
    fread(src = source, line = 1, sizeof source, file);
    obj *env = def("_add", _add, nil);
    env = def("_equal", _equal, env);
    env = def("_less", _less, env);
    env = def("ord", _ord, env);
    env = def("chr", _chr, env);
    env = def("pr", _pr, env);
    obj *o = suffix(expr(), 0, "end of file");
    pr(eval(o, env), false), puts("");
}
