#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define new(t,...) memcpy(malloc(sizeof(t)), &(t){__VA_ARGS__}, sizeof(t))
#define copy(n, xs) memcpy(malloc(n * sizeof *xs), xs, n * sizeof *xs)
#define ast(f,pos,...) new(ast, f, pos, __VA_ARGS__)

typedef struct string { int len; char text[]; } string;
typedef struct pos { string *fn; int ln; } pos;
typedef struct ast ast;
typedef struct type type;
typedef struct rule { ast *lhs, *rhs; } rule;
typedef struct def { string *id; ast *val; } def;
typedef struct infix { string *id; int lhs, rhs; struct infix *next; } infix;
typedef struct ctor { string *id; type *type; } ctor;
typedef struct env { string *id; type *type; struct env *next; } env;
struct type {
    enum { BASETYPE, TYPEVAR, FNTYPE, TUPLETYPE } form;
    string  *id;
    type    *inst;
    int     n;
    type    **ts;
};
struct ast {
    enum { ENIL,EINT,ECHR,ESTR,EVAR,ECTOR,ETUP,ECONS,
           EFN,EAPP,ESEQ,ELET,EREC,ECASE,EIF,ECRASH} form;
    pos pos;
    type *type;
    union {
        int     i;
        string  *str;
        string  *id;
        struct  { int n; ast **es; } tup;
        struct  { ast *lhs, *rhs; } app,cons,seq;
        def     fn;
        struct  { string *id; ast *val, *e; } let;
        struct  { ast *lhs, *rhs, *e; } de;
        struct  { int n; def *d; ast *e; } rec;
        struct  { ast *e; int n; rule *r; } _case;
        struct  { ast *a, *b, *c; } _if;
    };
};

enum token { TEOF,TINT,TCHAR,TSTR,TCID,TLP,TRP,TLB,TRB,TCOMMA,TSEMI,
           TID,TEQ,TLET,TREC,TAND,TIN,TFN,TARROW,TBAR,TIF,TTHEN,
           TELSE,TCASE,TINL,TINR,TDATA };
static char *tokens[] = {
            "eof","int","char","string","cid","(",")","[","]",
            ",",";","id","=","let","rec","and","in","fn","->",
            "|","if","then","else","case","infixl","infixr",
            "datatype",0};
static char *opchr="!$%&*+-./:<=>?@^|~";

static char     source[65536];
static char     *src;
static pos      srcpos;
static bool     peeked;
static int      tokint;
static char     tokbuf[sizeof source];
static string   *tokstr;
static int      token;
static string   **interns;
static int      ninterns;
static string   *tuple_star;
static string   *list_id, *eq_id, *proj_id, *hd_id, *tl_id;
static infix    *infixes;
static ast      *dummy;
static type     *booltype, *inttype, *chrtype, *strtype;
static type     *types[65536];
static int      ntypes;
static type     *nongenerics[65536];
static int      nnongenerics;
static ctor     ctors[65536];
static int      nctors;

void *pr(char *msg, ...);
ast *expr(void);
ast *aexpr(bool required);
type *ty(void);
ast *cvt_case(ast *e);
type *typecheck(ast *e, env *env);

string *mkstr(char *text, int n) {
    if (n<0) n = strlen(text);
    string *s = malloc(sizeof *s + n + 1);
    s->text[s->len = n] = 0;
    if (text) memcpy(s->text, text, n);
    return s;
}
string *intern(char *text, int n) {
    for (int i=0; i<ninterns; i++)
        if (interns[i]->len == n && !memcmp(interns[i]->text, text, n))
            return interns[i];
    interns = realloc(interns, ++ninterns * sizeof *interns);
    return interns[ninterns - 1] = mkstr(text, n);
}
string *unique(void) {
    static int uid;
    char buf[16];
    sprintf(buf, "$%d", ++uid);
    return intern(buf, -1);
}

void *syntax(char *msg, ...) {
    va_list ap; va_start(ap, msg);
    pr("boot: error %s:%d: %*\n", srcpos.fn->text, srcpos.ln, msg, &ap);
    exit(1);
}
void *semantic(ast *e, char *msg, ...) {
    va_list ap; va_start(ap, msg);
    pr("boot: error %s:%d: %*\n", e->pos.fn->text, e->pos.ln, msg, &ap);
    exit(1);
}
void opensrc(char *fname) {
    srcpos = (pos) {intern(fname, -1), 1};
    src = source, peeked = 0;
    FILE *file = fopen(fname, "rb");
    if (!file) syntax("cannot open source");
    source[fread(source, 1, sizeof source, file)] = 0;
    fclose(file);
}
int charlit(void) {
    if (*src != '\\')
        return *src++;
    int c = (src += 2)[-1];
    return c == 't'? '\t': c == 'n'? '\n': c;
}
int next(void) {
    char *t=tokbuf;

    if (peeked) return (peeked=false), token;
    while (*src == '#' || isspace(*src))
        if (*src == '\n') srcpos.ln++, src++;
        else if (isspace(*src)) src++;
        else if (*src == '#') while (*src && *src != '\n') src++;

    if (!*src) return (token=0);
    if (isdigit(src[*src=='-']))
        return (tokint=strtol(src, &src, 10)), (token = TINT);
    for (int i = TCID + 1; i < TID; i++) // Single-character punctuation
        if (*src == *tokens[i]) return (++src), (token = i);
    if (*src=='\'') {
        src++, tokint = charlit();
        if (*src++ != '\'') syntax("unclosed char");
        return token = TCHAR;
    }
    if (*src=='"')
        for (++src; ; )
            if (!*src || *src=='\n') syntax("unclosed string");
            else if (*src=='"')
                return src++, tokstr = intern(tokbuf, t-tokbuf), token = TSTR;
            else *t++ = charlit();

    while (isalnum(*src) || *src == '_' || *src == '\'') *t++ = *src++;
    if (t == tokbuf) while (*src && strchr(opchr, *src)) *t++ = *src++;
    if (t == tokbuf) syntax("not a token: %c", *src);
    tokstr = intern(tokbuf, t - tokbuf);
    for (char **i = tokens + TID + 1; *i; i++)
        if (*i == tokstr->text) return (token = i - tokens);
    return token = isupper(*tokbuf)? TCID: TID;
}
bool peek(int t) { return (next(), peeked = true, t == token); }
bool want(int t) { return !(peeked = next() != t); }
bool peek_id(string *id) { return peek(TID) && id == tokstr; }
bool want_id(string *id) { return peek_id(id) && next(); }
void need(int t) { if (!want(t)) syntax("need %s", tokens[t]); }
void *suffix(void *x, int t) { need(t); return x; }

infix *findinfix(void) {
    if (peek(TID))
        for (infix *i = infixes; i; i = i->next)
            if (i->id == tokstr) return i;
    return 0;
}
type *prune(type *t) { return t->inst? (t->inst = prune(t->inst)): t; }
type *typevar(string *id) { return new(type, TYPEVAR, .id=id, .inst=0); }
type *basetype(string *id, int n, type **ts) {
    return new(type, BASETYPE, .id=id, .n=n, .ts=copy(n, ts));
}
type *tupletype(int n, type **ts) {
    return new(type, TUPLETYPE, .n=n, .ts=copy(n, ts));
}
type *listtype(type *t) { return basetype(list_id, 1, new(type*[1], t)); }
type *fntype(type *lhs, type *rhs) {
    return new(type, FNTYPE, .n=2, .ts=copy(2, ((type*[]) {lhs, rhs})));
}
type *nametype(type *t, int *uid) {
    t = prune(t);
    if (t->form == TYPEVAR && !t->id)
        t->id = intern((char[]) {++*uid + 'a' - 1, 0}, -1);
    for (int i = 0; i < t->n; i++) nametype(t->ts[i], uid);
    return t;
}
ast *eint(pos pos, int i) { return ast(EINT, pos, .i=i); }
ast *evar(pos pos, string *id) { return ast(EVAR, pos, .id=id); }
ast *eapp(ast *lhs, ast *rhs) { return ast(EAPP, lhs->pos, .app={lhs, rhs}); }
ast *eif(pos pos, ast *a, ast *b, ast *c) {
    return ast(EIF, pos, ._if={a, b, c});
}
ast *elet(pos pos, string *id, ast *val, ast *e) {
    return ast(ELET, pos, .let={id, val, e});
}

ast *fexpr(string *id, enum token delim) {
    int     nrules = 0;
    int     nparams = 0;
    rule    *rules = 0;
    bool    complex = false;
    pos     org = srcpos;
    do {
        pos     org = srcpos;
        ast     *p, **params = 0;
        int     n = 0;

        while ((p = aexpr(false))) {
            params = realloc(params, ++n * sizeof *params);
            params[n - 1] = p;
            complex |= p->form != EVAR;
        }
        need(delim);

        if (nrules > 0 && n != nparams)
            syntax("wrong number of params on this clause");
        else if (nrules == 0)
            nparams = n;

        ast *body = expr();
        ast *tuple = ast(ETUP, org, .tup={n, params});
        rules = realloc(rules, ++nrules * sizeof *rules);
        rules[nrules - 1] = (rule) { tuple, body };
    } while (want(TBAR) && (!id || want_id(id) || syntax("need id: %S", id)));

    ast *body = rules[0].rhs;
    ast **canon = rules[0].lhs->tup.es;

    if (nrules > 1 || complex) {
        // Make canonical param variables and put rules into case.
        canon = malloc(nparams * sizeof *canon);
        for (int i = 0; i < nparams; i++)
            canon[i] = ast(EVAR, org, .id=unique());
        ast *subject = ast(ETUP, org, .tup={nparams, canon});
        body = ast(ECASE, org, ._case={subject, nrules, rules});
    }

    for (int i = nparams; i-- > 0; )
        body = ast(EFN, org, .fn={canon[i]->id, body});
    return body;
}

ast *aexpr(bool required) {
    pos org = srcpos;
    if (!required && findinfix()) return 0; // avoid eating operator as arg
    if (want(TINT))     return ast(EINT, org, .i=tokint);
    if (want(TCHAR))    return ast(ECHR, org, .i=tokint);
    if (want(TSTR))     return ast(ESTR, org, .str=tokstr);
    if (want(TID))      return ast(EVAR, org, .id=tokstr);
    if (want(TCID))     return ast(ECTOR, org, .id=tokstr);
    if (want(TLP)) {
        ast **es = 0;
        int n = 0;
        do {
            if (peek(TRP)) break;
            es = realloc(es, ++n * sizeof *es);
            es[n - 1] = expr();
        } while (want(TCOMMA));
        need(TRP);
        return ast(ETUP, org, .tup={n, es});
    }
    if (want(TLB)) {
        ast *list = 0, **p = &list;
        do {
            if (peek(TRB)) break;
            ast *x = expr();
            *p = ast(ECONS, x->pos, .cons={x, 0});
            p = &(*p)->cons.rhs;
        } while (want(TCOMMA));
        need(TRB);
        *p = ast(ENIL, org);
        return list;
    }
    if (want(TFN)) return fexpr(0, TARROW);
    if (required) syntax("need expression");
    return 0;
}
ast *letdefs(bool need_in) {
    bool    rec = want(TREC);
    rule    *r = 0;
    int     n = 0;
    want(TAND);
    do {
        ast *lhs = aexpr(true);
        ast *rhs = fexpr(lhs->form==EVAR? lhs->id: 0, TEQ);
        if (rec && (lhs->form != EVAR || rhs->form != EFN))
            syntax("let rec can only define functions");
        r = realloc(r, ++n * sizeof *r);
        r[n - 1] = (rule) {lhs, rhs};
    } while (want(TAND));

    ast *e = need_in? (need(TIN), expr()): dummy;
    pos org = srcpos;
    if (rec) {  // Definitions in let rec must remain mutually recursive.
        def *d = malloc(n * sizeof *d);
        for (int i = 0; i < n; i++)
            d[i] = (def) { r[i].lhs->id, r[i].rhs };
        return ast(EREC, org, .rec={n, d, e});
    }
    // Simplify let with `and` into single definitions.
    for (rule *i = r + n; i-- > r; )
        e = i->lhs->form == EVAR
            ? ast(ELET, i->lhs->pos, .let={i->lhs->id, i->rhs, e})
            : ast(ECASE, i->lhs->pos, ._case={i->rhs, 1, new(rule, i->lhs, e)});
    return e;
}
ast *iexpr(int lvl) {
    pos org = srcpos;
    if (lvl > 11) {
        if (want(TIF)) {
            ast *a = expr();
            ast *b = (need(TTHEN), expr());
            ast *c = (need(TELSE), expr());
            return ast(EIF, org, ._if={a, b, c});
        } else if (want(TCASE)) {
            ast *e = expr();
            rule *r = 0;
            int  n = 0;
            while (want(TBAR)) {
                ast *lhs = expr();
                ast *rhs = (need(TARROW), expr());
                r = realloc(r, ++n * sizeof *r);
                r[n - 1] = (rule) {lhs, rhs};
            }
            return ast(ECASE, org, ._case={e, n, r});
        } else if (want(TLET))
            return letdefs(true);
        else {
            ast *rhs, *lhs = aexpr(true);
            while ((rhs = aexpr(false)))
                lhs = eapp(lhs, rhs);
            return lhs;
        }
    } else {
        ast *lhs = iexpr(lvl + 1);
        infix *i;
        while (((i = findinfix()) && i->lhs == lvl) || (peek(TSEMI) && !lvl))
            if (want(TSEMI))
                lhs = ast(ESEQ, lhs->pos, .seq={lhs, iexpr(1)});
            else {
                ast *op = evar(srcpos, i->id);
                ast *rhs = (next(), iexpr(i->rhs));
                lhs = eapp(eapp(op, lhs), rhs);
            }
        return lhs;
    }
}
ast *expr(void) { return iexpr(0); }
type *findtype(string *id, bool required) {
    for (type **i = types; i < types + ntypes; i++)
            if ((*i)->id == id) return *i;
    if (required) syntax("undefined type: %S", id);
    return 0;
}
ctor *findctor(string *id, bool required) {
    for (ctor *i = ctors; i < ctors + nctors; i++)
            if (i->id == id) return i;
    if (required) syntax("undefined ctor: %S", id);
    return 0;
}
env *findvar(ast *e, string *id, env *env) {
    for (struct env *i = env; i; i = i->next)
        if (i->id == id) return i;
    return semantic(e, "undefined %S", id);
}
type *aty(void) {
    type *t;
    if (want(TID)) {
        t = findtype(tokstr, true);
        if (t->n) syntax("type needed args: %S", t->id);
    } else if (want(TLP)) {
        type    **ts = 0;
        int     n = 0;
        do {
            if (peek(TRP)) break;
            ts = realloc(ts, ++n * sizeof *ts);
            ts[n - 1] = ty();
        } while (want(TCOMMA));
        need(TRP);
        if (n == 0) t = tupletype(0, 0);
        else if (n == 1) t = ts[0];
        else {
            if (peek_id(tuple_star) || !want(TID))
                syntax("need type name after ()");
            type *c = findtype(tokstr, true);
            if (c->n != n) syntax("wrong type arity: %S", c->id);
            t = basetype(c->id, n, ts);
        }
    } else syntax("need type");
    while (!peek_id(tuple_star) && want(TID)) {
        type *c = findtype(tokstr, true);
        if (c->n != 1) syntax("wrong type arity: %S", c->id);
        t = basetype(c->id, 1, new(type*, t));
    }
    return t;
}
type *ty(void) {
    type *t = aty();
    if (want(TARROW)) return fntype(t, ty());
    if (want_id(tuple_star)) {
        type    **ts = new(type*, t);
        int     n = 1;
        do {
            ts = realloc(ts, ++n * sizeof *ts);
            ts[n - 1] = ty();
        } while (want_id(tuple_star));
        return tupletype(n, ts);
    }
    return t;
}
ast **innermost_let_body(ast **p) {
    if (!*p) return p;
    if ((*p)->form == ELET) return innermost_let_body(&(*p)->let.e);
    if ((*p)->form == EREC) return innermost_let_body(&(*p)->rec.e);
    if ((*p)->form == ECASE) return innermost_let_body(&(*p)->_case.r[0].rhs);
    return p;
}
void toplevel(ast **p) {
    while (!peek(TEOF))
        if (want(TLET)) {
            *p = letdefs(false);
            p = innermost_let_body(p);
        } else if (want(TINL) || want(TINR)) {
            int bias = token == TINL? 1: 0;
            int lhs = (need(TINT), tokint);
            while (want(TID))
                infixes = new(infix, tokstr, lhs, lhs + bias, infixes);
        } else if (want(TDATA)) {
            type    **args = 0;
            int     n = 0;
            if (want(TLP)) {
                do {
                    if (peek(TRP)) break;
                    args = realloc(args, ++n * sizeof *args);
                    args[n - 1] = typevar((need(TID), tokstr));
                } while (want(TCOMMA));
                need(TRP);
            }
            type    *datatype = basetype((need(TID), tokstr), n, args);
            if (findtype(datatype->id, false))
                syntax("type already defined: %S", datatype->id);

            // Add type to global list; add args last so they can be popped
            types[ntypes++] = datatype;
            for (int i = 0; i < n; i++) types[ntypes++] = args[i];

            need(TEQ), want(TBAR);
            do {
                string *id = (need(TCID), tokstr);
                if (findctor(id, false)) syntax("ctor already defined: %s", id);
                type   *arg = (peek(TLP) || peek(TID)) ? ty(): 0;
                type   *ctype = arg? fntype(arg, datatype): datatype;
                ctors[nctors++] = (ctor) { id, ctype };
            } while (want(TBAR));

            ntypes -= n; // Pop the type args
        } else
            syntax("need top-level definition");
}
void cvt_case2(ast **a, ast **b) { *a = cvt_case(*a); *b = cvt_case(*b); }
ast *cvt_case_pat(ast *e, ast *val, ast *good, ast *bad) {
    switch (e->form) {
    case ENIL: case EINT: case ECHR: case ESTR: case ECTOR:
        return eif(e->pos, eapp(eapp(evar(e->pos, eq_id), val), e), good, bad);
    case EVAR:  return elet(e->pos, e->id, val, good);
    case ETUP:  for (int i = e->tup.n; i-- > 0 ;) {
                    ast *item = e->tup.es[i];
                    ast *proj = eapp(eapp(evar(item->pos, proj_id), val),
                                     eint(item->pos, i));
                    good = cvt_case_pat(item, proj, good, bad);
                }
                return good;
    case ECONS: good = cvt_case_pat(e->cons.rhs,
                                   eapp(evar(e->cons.rhs->pos, tl_id), val),
                                   good, bad);
                return cvt_case_pat(e->cons.lhs,
                                    eapp(evar(e->cons.lhs->pos, hd_id), val),
                                    good, bad);
    case EAPP:
    case EFN: case ESEQ: case ELET: case EREC:
    case ECASE: case EIF: case ECRASH:
        return semantic(e, "invalid pattern");
    }
    return 0;
}
ast *cvt_case(ast *e) {
    ast *bad, *sub;
    switch (e->form) {
    case ENIL: case EINT: case ECHR: case ESTR: case EVAR: case ECTOR:
        return e;
    case ETUP:  for (int i = 0; i < e->tup.n; i++)
                    e->tup.es[i] = cvt_case(e->tup.es[i]);
                return e;
    case ECONS: cvt_case2(&e->cons.lhs, &e->cons.rhs); return e;
    case EFN:   e->fn.val = cvt_case(e->fn.val);
                return e;
    case EAPP:  cvt_case2(&e->app.lhs, &e->app.rhs); return e;
    case ESEQ:  cvt_case2(&e->app.lhs, &e->app.rhs); return e;
    case ELET:  cvt_case2(&e->let.val, &e->let.e); return e;
    case EREC:  for (int i = 0; i < e->rec.n; i++)
                    e->rec.d[i].val = cvt_case(e->rec.d[i].val);
                e->rec.e = cvt_case(e->rec.e);
                return e;
    case ECASE: // Wrap in let if the subject is not a variable
                if (e->_case.e->form != EVAR) {
                    string *id = unique();
                    ast *all = ast(ELET, e->pos, .let={id, e->_case.e, e});
                    e->_case.e = evar(e->_case.e->pos, id);
                    return cvt_case(all);
                }
                bad = ast(ECRASH, e->pos);
                sub = e->_case.e;
                for (rule *i = e->_case.r; i < e->_case.r + e->_case.n; i++)
                    bad = cvt_case_pat(i->lhs, sub, cvt_case(i->rhs), bad);
                return bad;
    case EIF:   cvt_case2(&e->_if.a, &e->_if.b);
                e->_if.c = cvt_case(e->_if.c);
                return e;
    case ECRASH: return e;
    }
    return 0;
}

bool occurs_in(type *var, type *t) {
    t = prune(t);
    if (t == var) return true;
    for (int i = 0; i < t->n; i++)
        if (occurs_in(var, t->ts[i])) return true;
    return false;
}
bool unifies(type *t, type *u) {
    t = prune(t);
    u = prune(u);
    if (t->form == TYPEVAR) return occurs_in(t, u)? t == u: (t->inst = u, true);
    if (u->form == TYPEVAR) return unifies(u, t);
    if (t->id != u->id || t->n != u->n) return false;
    for (int i=0; i < t->n; i++) if (!unifies(t->ts[i], u->ts[i])) return false;
    return true;
}
type *unify(ast *e, type *t, type *u) {
    if (!unifies(t, u)) semantic(e, "type mismatch:\n> %t\n> %t", t, u);
    return prune(u);
}

type *_typecheck(ast *e, env *env) {
    type    **ts, *t, *u;
    switch (e->form) {
    case ENIL:  return listtype(typevar(0));
    case EINT:  return inttype;
    case ECHR:  return chrtype;
    case ESTR:  return strtype;
    case EVAR:  return findvar(e, e->id, env)->type;
    case ECTOR: return findctor(e->id, true)->type;
    case ETUP:  ts = malloc(e->tup.n * sizeof *ts);
                for (int i = 0; i < e->tup.n; i++)
                    ts[i] = typecheck(e->tup.es[i], env);
                return tupletype(e->tup.n, ts);
    case ECONS: t = typecheck(e->cons.lhs, env);
                u = typecheck(e->cons.rhs, env);
                return unify(e, listtype(t), u);
    case EFN:   t = typevar(0);
                u = typecheck(e->fn.val, new(struct env, e->fn.id, t, env));
                return fntype(t, u);
    case EAPP:  t = typecheck(e->app.lhs, env);
                u = typecheck(e->app.rhs, env);
                return unify(e, t, fntype(u, typevar(0)))->ts[1];
    case ESEQ:  t = typecheck(e->app.lhs, env);
                return typecheck(e->app.rhs, env);
    case ELET:  t = typecheck(e->let.val, env);
                return typecheck(e->let.e, new(struct env, e->let.id, t, env));
    case EREC:  for (def *i = e->rec.d; i < e->rec.d + e->rec.n; i++) {
                    t = nongenerics[nnongenerics++] = typevar(0);
                    env = new(struct env, i->id, t, env);
                }
                for (def *i = e->rec.d; i < e->rec.d + e->rec.n; i++)
                    unify(i->val, findvar(i->val, i->id, env)->type,
                          typecheck(i->val, env));
                nnongenerics -= e->rec.n;
                return typecheck(e->rec.e, env);
    case ECASE: break; // does not exist at this stage
    case EIF:   unify(e->_if.a, booltype, typecheck(e->_if.a, env));
                t = typecheck(e->_if.b, env);
                u = typecheck(e->_if.c, env);
                return unify(e->_if.c, t, u);
    case ECRASH: return typevar(0);
    }
    return semantic(e, "NOT TYPECHECKED %e", e);
}
type *typecheck(ast *e, env *env) { return e->type = _typecheck(e, env); }

void *prexpr(ast *e) {
    switch (e->form) {
    case ENIL:  return pr("[]");
    case EINT:  return pr("%d", e->i);
    case ECHR:  return pr("'%c'", e->i);
    case ESTR:  return pr("\"%S\"", e->str);
    case EVAR:  return pr("%S", e->id);
    case ECTOR: return pr("%S", e->id);
    case ETUP:  pr("(");
                for (int i = 0; i < e->tup.n; i++)
                    pr("%s%e", i? ", ": "", e->tup.es[i]);
                return pr(")");
    case ECONS: return pr("(%e:%e)", e->cons.lhs, e->cons.rhs);
    case EFN:   return pr("(fn %S->%e)", e->fn.id, e->fn.val);
    case EAPP:  return pr("(%e %e)", e->cons.lhs, e->cons.rhs);
    case ESEQ:  return pr("(%e; %e)", e->seq.lhs, e->seq.rhs);
    case ELET:  return pr("let %S=%e in %e", e->let.id, e->let.val, e->let.e);
    case EREC:  pr("let rec");
                for (def *i = e->rec.d; i < e->rec.d + e->rec.n; i++)
                    pr("%s%S=%e", i != e->rec.d? " and ": " ", i->id, i->val);
                return pr(" in %e", e->rec.e);
    case ECASE: pr("case %e", e->_case.e);
                for (rule *i = e->_case.r; i < e->_case.r + e->_case.n; i++)
                    pr(" | %e -> %e", i->lhs, i->rhs);
                return 0;
    case EIF:   return pr("if %e then %e else %e",e->_if.a, e->_if.b, e->_if.c);
    case ECRASH: return pr("(CRASH)");
    }
    return 0;
}
void *prtype(type *t, bool paren) {
    t = prune(t);
    if (paren && (t->form==TUPLETYPE || t->form==FNTYPE)) return pr("(%t)", t);
    switch (t->form) {
    case BASETYPE:  if (t->n == 0) return pr("%S", t->id);
                    if (t->n == 1) return pr("%T %S", *t->ts, t->id);
                    for (type **i = t->ts; i < t->ts + t->n; i++)
                        pr("%s%t", i == t->ts? "(": ", ", *i);
                    return pr(") %S", t->id);
    case TYPEVAR:   return pr("%S", t->id);
    case FNTYPE:    return pr(t->ts[1]->form == FNTYPE? "%T -> %t": "%T -> %T",
                              t->ts[0], t->ts[1]);
    case TUPLETYPE: for (type **i = t->ts; i < t->ts + t->n; i++)
                        pr("%s%t", i == t->ts? "": " * ", *i);
                    return pr(t->n? "": "()");
    }
    return 0;
}
void *vpr(char *msg, va_list ap) {
    int uid = 0;
    for (char *s = msg; *s; s++)
        if (*s != '%') putchar(*s);
        else switch (*++s) {
        case 'c': putchar(va_arg(ap, int)); break;
        case 'd': printf("%d", va_arg(ap, int)); break;
        case 'e': prexpr(va_arg(ap, ast *)); break;
        case 's': fputs(va_arg(ap, char *), stdout); break;
        case 'S': fputs(va_arg(ap, string *)->text, stdout); break;
        case 't': prtype(nametype(va_arg(ap, type *), &uid), false); break;
        case 'T': prtype(nametype(va_arg(ap, type *), &uid), true); break;
        case '*': { char *msg2 = va_arg(ap, char *);
                    vpr(msg2, *va_arg(ap, va_list *));
                  } break;
        }
    va_end(ap);
    return 0;
}
void *pr(char *msg, ...) {
    va_list ap; va_start(ap, msg);
    return vpr(msg, ap);
}

int main(int argc, char **argv) {
    setvbuf(stdout, 0, _IONBF, 0);

    dummy = ast(ETUP, srcpos, .tup={0, 0});
    tuple_star = intern("*", -1);
    list_id = intern("list", -1);
    eq_id = intern("==", -1);
    proj_id = intern("$proj", -1);
    hd_id = intern("hd", -1);
    tl_id = intern("tl", -1);
    types[ntypes++] = booltype = basetype(intern("bool", -1), 0, 0);
    types[ntypes++] = inttype = basetype(intern("int", -1), 0, 0);
    types[ntypes++] = chrtype = basetype(intern("char", -1), 0, 0);
    types[ntypes++] = strtype = basetype(intern("string", -1), 0, 0);
    types[ntypes++] = basetype(list_id, 1, new(type*, typevar(0)));
    ctors[nctors++] = (ctor) {intern("False", -1), booltype};
    ctors[nctors++] = (ctor) {intern("True", -1), booltype};

    for (char **i = tokens; *i; i++) *i = intern(*i, -1)->text;

    for (argv++; *argv; argv++) {
        opensrc(*argv);
        ast *all = 0;
        toplevel(&all);
        pr("# %e\n\n", all);
        all = cvt_case(all);
        pr("# %e\n", all);
        typecheck(all, 0);
        if (!all) syntax("need at least one definition");
    }
    puts("done.");
}
