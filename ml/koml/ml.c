#define new(t,...) memcpy(malloc(sizeof(t)), &(t){__VA_ARGS__}, sizeof(t))
#define tree(t,p,...) new(tree, .type=t, .pos=p, __VA_ARGS__)

#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {int n; char s[];} String;
struct pos { char *fn; int ln; };

enum token {
    TEOF, TINT, TCHAR, TSTRING, TCOMMA, TLP, TRP, TLB, TRB, TID,
    TEQ, TLET, TREC, TAND, TIN, TIF, TTHEN, TELSE, TCASE, TBAR,
    TARROW, TFN, TTRUE, TFALSE,
};
char *toks[] = {
    "e.o.f.", "int", "char", "string", ",", "(", ")", "[", "]",
    "id", "=", "let", "rec", "and", "in", "if", "then", "else",
    "case", "|", "->", "fn", "true", "false", 0
};

struct infix { char *id; int lhs, rhs, oper; struct infix *next; };

typedef struct value {
    enum { BOOLE, INT, CHAR, STRING, TUPLE, LIST, FN } type;
    union {
        int             i;
        String          *s;
        struct tuple    *tuple;
        struct cons     *cons;
        struct fn       *fn;
    };
} value;

struct tuple {int n; struct value xs[];};
struct cons { value hd, tl; };
struct values { value x; struct values *next; };
struct fn { struct tree *body; struct values *env; };

enum oper {
    ONOP, // Make first oper != 0 so that `struct vars.oper`==0 is no operator
    OHD, OTL, OLEN, OTLEN, OISBOOL, OISINT, OISCHAR, OISSTRING,
    OISTUPLE, OISLIST, OISFN, OISCONS, OPRINT,
    OBINARY, // Indicate binary operators for `xform_known()`
    OEQ, ONE, OLT, OGT, OLE, OGE, OADD, OSUB, OMUL, ODIV, OREM,
    OCONS, OINDEX, ONTH, OSEQ,
};
char *opers[] = {
    "", "hd", "tl", "length", "tuplelength", "isbool", "isint",
    "ischar", "isstring", "istuple", "islist", "isfn", "iscons",
    "print", "", "==", "<>", "<", ">", "<=", ">=", "+", "-",
    "*", "/", "rem", ":", "~index~", "@", ";", 0
};

typedef struct tree {
    enum {
        ELIT, EID, ETUPLE, ELIST, EFN, EAPP, EPRIM,
        EOPER, EIF, ECASE, ELET, ECRASH
    } type;
    struct pos  pos;
    char        *id;
    int         index;
    enum oper   oper;
    int         n;
    struct tree **xs;
    struct tree *cond, *yes, *no;
    struct tree *lhs, *rhs;
    struct tree *whole;
    struct tree *subject;
    struct rules *rules;
    bool        recursive;
    value       val;
} tree;

typedef struct rules { tree *lhs, *rhs; struct rules *next; } rules;

typedef struct vars {
    char        *id;
    enum oper   oper;   // is part of the ground environment
    struct vars *next;
} vars;

char            source[128 * 1024];
char            tokbuf[sizeof source];
char            *src;
String          *toktxt;
int             tokint;
struct pos      pos;
enum token      token;
bool            peeked;
struct infix    *infixes;
String          **interns;
int             ninterns;
int             uid;
value           unit = {TUPLE, 0};
value           nil = {LIST, 0};

void print(char *msg, ...);
tree *expr();
tree *aexpr(bool required);

String *intern(char *s, int n) {
    if (n == -1) n = strlen(s);

    for (int i = 0; i < ninterns; i++)
        if (n == interns[i]->n && !memcmp(s, interns[i]->s, n))
            return interns[i];

    interns = realloc(interns, ++ninterns * sizeof *interns);
    String *out = malloc(sizeof *out + n + 1);
    memcpy(out->s, s, n);
    out->s[n] = 0;
    out->n = n;
    return interns[ninterns - 1] = out;
}

bool isnil(value x) { return x.cons == 0; }
bool iscons(value x) { return x.type == LIST && !isnil(x); }
bool isint(value x) { return x.type == INT; }
value boole(bool x) { return (value) {BOOLE, .i=x}; }
value integer(int x) { return (value) {INT, .i=x}; }
value string(String *s) { return (value) {STRING, .s=s}; }
value cons(value hd, value tl) {
    return (value) {LIST, .cons=new(struct cons, hd, tl)};
}

bool equal(value a, value b) {
    if (a.type != b.type) return false;

    switch (a.type) {
    case BOOLE:     return a.i == b.i;
    case INT:       return a.i == b.i;
    case CHAR:      return a.i == b.i;
    case STRING:    return a.s == b.s ||
                            (a.s->n == b.s->n && !strcmp(a.s->s, b.s->s));

    case TUPLE:     if (a.tuple->n != b.tuple->n) return false;
                    for (int i = 0; i < a.tuple->n; i++)
                        if (!equal(a.tuple->xs[i], b.tuple->xs[i])) return 0;
                    return true;

    case LIST:      if (isnil(a)) return isnil(b);
                    if (isnil(b)) return isnil(a);
                    while (!isnil(a) && !isnil(b))
                        if (!equal(a.cons->hd, b.cons->hd)) return false;
                    return equal(a, b);

    case FN:        return a.fn == b.fn;
    }
}

void printexpr(tree *e) {
    if (e == 0) { print("(NULL EXPRESSION)"); return; }
    switch (e->type) {
    case ELIT:      print("%V", e->val); break;
    case EID:       print("%s*%d", e->id, e->index); break; // QQQ REMOVE INDEX
    case ETUPLE:
    case ELIST:     print(e->type == ETUPLE? "(": "[");
                    for (int i = 0; i < e->n; i++)
                        print("%s%e", i? ", ": "", e->xs[i]);
                    print(e->type == ETUPLE? ")": "]");
                    break;
    case EFN:       print("(fn %e->%e)", e->lhs, e->rhs); break;
    case EAPP:      print("(%e %e)", e->lhs, e->rhs); break;
    case EPRIM:     print("((%s %e))", opers[e->oper], e->rhs); break;
    case EOPER:     print("(%e %s %e)", e->lhs, opers[e->oper], e->rhs); break;
    case EIF:       print("(if %e then %e else %e)",
                        e->cond, e->yes, e->no); break;
    case ECASE:     print("(case %e", e->subject);
                    for (rules *r = e->rules; r; r = r->next)
                        print("|%e->%e", r->lhs, r->rhs);
                    print(")");
                    break;
    case ELET:      print("(%s ", e->recursive? "let rec": "let");
                    for (rules *r = e->rules; r; r = r->next)
                        print("%e=%e %s", r->lhs, r->rhs, r->next? "and ": "");
                    print("in %e)", e->subject);
                    break;
    case ECRASH:    print("(*CRASH*)"); break;
    default:        print("(EXPR_%d_NOT_PRINTED)%!", e->type);
    }
}

void printvalue(value x, bool repr) {
    switch (x.type) {
    case BOOLE:     print("%s", x.i? "true": "false"); break;
    case INT:       print("%d", x.i); break;
    case CHAR:      print("%c", x.i); break;
    case STRING:    if (repr) {
                        print("\"");
                        for (int i = 0; i < x.s->n; i++)
                            if (x.s->s[i] == '\n') print("\\n");
                            else if (x.s->s[i] == '\t') print("\\t");
                            else print("%c", x.s->s[i]);
                        print("\"");
                    } else print("%s", x.s->s);
                    break;
    case TUPLE:     print("(");
                    for (int i = 0; i < x.tuple->n; i++)
                        print("%s%v", i? ", ": "", x.tuple->xs[i]);
                    print(")");
                    break;
    case LIST:      print("[");
                    for (value i = x; !isnil(i); i = i.cons->tl)
                        print("%v%s", i.cons->hd, isnil(i.cons->tl)? "": ", ");
                    print("]");
                    break;
    case FN:        print("(fn)"); break;
    }
}

void vprint(char *msg, va_list ap) {
    char *s;
    for ( ; *msg; msg++)
        if (*msg == '%')
            switch (*++msg) {
            case 's':   s = va_arg(ap, char*);
                        if (s) printf("%s", s);
                        break;
            case 'c':   printf("%c", va_arg(ap, int)); break;
            case 'd':   printf("%d", va_arg(ap, int)); break;
            case 'e':   printexpr(va_arg(ap, tree*)); break;
            case 'v':   printvalue(va_arg(ap, value), false); break;
            case 'V':   printvalue(va_arg(ap, value), true); break;
            case '!':   puts(""); exit(1); break;
            case '*':   s = va_arg(ap, char*);
                        vprint(s, *va_arg(ap, va_list*));
                        break;
            }
        else putchar(*msg);
    va_end(ap);
}

void print(char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    vprint(msg, ap);
}

void *syntax(char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    print("ml: error %s:%d: %* at %s.%!", pos.fn, pos.ln, msg, &ap, toks[token]);
}

void *semantic(tree *e, char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    print("ml: error %s:%d: %*\nexpr: %e%!", e->pos.fn, e->pos.ln, msg, &ap, e);
}

void opensource(char *fn) {
    pos = (struct pos) {strdup(fn), 1};
    FILE *file = fopen(fn, "rb");
    if (!file) syntax("cannot open file");
    src = source;
    source[fread(source, 1, sizeof source, file)] = 0;
    fclose(file);
}

int escaped() {
    if (*src == '\\')
        switch (*++src) {
        case 'n': return '\n';
        case 't': return '\t';
        }
    return *src;
}

enum token next() {
    char    *opchrs = "!%&$*+-/:<=>?@~`^|";
    char    *t = tokbuf;

    if (peeked) return peeked = false, token;

    while (isspace(*src) || *src == '#')
        if (*src == '\n') src++, pos.ln++;
        else if (*src == '#') while (*src && *src != '\n') src++;
        else src++;

    if (!*src) return token = TEOF; // End of file
    for (int i = TCOMMA; i <= TRB; i++) // Single-character punctuation
        if (*src == toks[i][0]) return src++, token = i;
    if (isdigit(src[*src == '-'])) // Number
        return tokint = strtoul(src, &src, 10), token = TINT;
    if (*src == ';')
        return *t = *src++, toktxt = intern(t, 1), token = TID;

    if (*src == '\'') { // Character literal
        src++;
        tokint = escaped();
        if (*++src != '\'') syntax("unclosed character");
        return src++, token = TCHAR;
    }
    if (*src == '"') // String literal
        for (src++; ; src++)
            if (!*src || *src == '\n') syntax("unclosed string");
            else if (*src == '"') {
                *t = 0, toktxt = intern(tokbuf, t - tokbuf);
                return src++, token = TSTRING;
            } else *t++ = escaped();

    // Identifiers and keywords
    while (isalnum(*src) || *src == '_' || *src == '\'') *t++ = *src++;
    if (t == tokbuf) while (*src && strchr(opchrs, *src)) *t++ = *src++;
    if (t == tokbuf) syntax("bad character %c", *src);

    *t = 0, toktxt = intern(tokbuf, t - tokbuf);
    for (int i = TID + 1; toks[i]; i++)
        if (toktxt->s == toks[i]) return token = i;
    return token = TID;
}
bool peek(enum token t) { return peeked = next(), t == token; }
bool want(enum token t) { return !(peeked = next() != t); }
void need(enum token t) { if (!want(t)) syntax("need %s", toks[t]); }
struct infix *infix() {
    if (!want(-1) && token == TID)
        for (struct infix *i = infixes; i; i = i->next)
            if (toktxt->s == i->id) return i;
    return 0;
}

tree *lit(value x, struct pos pos) {
    return tree(ELIT, pos, .val=x);
}
tree *var(char *id, struct pos pos) {
    return tree(EID, pos, .id=intern(id, -1)->s, .index=-1);
}
tree *uniquevar(struct pos pos) {
    char s[8];
    sprintf(s, "~%d", ++uid);
    return var(s, pos);
}
tree *let(char *id, tree *rhs, tree *body, struct pos pos) {
    rules *rules = new(struct rules, .lhs=var(id, pos), .rhs=rhs, .next=0);
    return tree(ELET, pos, .rules=rules, .subject=body, .recursive=false);
}
tree *app(tree *lhs, tree *rhs) {
    return tree(EAPP, lhs->pos, .lhs=lhs, .rhs=rhs);
}
tree *prim(enum oper f, tree *rhs) {
    return tree(EPRIM, rhs->pos, .oper=f, .rhs=rhs);
}
tree *oper(tree *lhs, enum oper oper, tree *rhs) {
    return tree(EOPER, lhs->pos, .lhs=lhs, .oper=oper, .rhs=rhs);
}
tree *cond(tree *cond, tree *yes, tree *no) {
    return tree(EIF, cond->pos, .cond=cond, .yes=yes, .no=no);
}
bool issingle(tree *e) {
    return e->rules && !e->rules->next && e->rules->lhs->type == EID;
}
tree *function(enum token separator) {
    if (want(separator)) return expr();
    tree *lhs = aexpr(true);
    tree *rhs = function(separator);
    return tree(EFN, pos, .lhs=lhs, .rhs=rhs);
}
rules *clauses() {
    if (!want(TBAR)) return 0;
    tree *lhs = aexpr(true);
    tree *rhs = (need(TARROW), expr());
    rules *next = clauses();
    return new(rules, .lhs=lhs, .rhs=rhs, .next=next);
}
rules *decls() {
    tree *lhs = aexpr(true);
    tree *rhs = function(TEQ);
    rules *next = want(TAND)? decls(): 0;
    return new(rules, .lhs=lhs, .rhs=rhs, .next=next);
}
tree *aexpr(bool required) {
    struct pos start = pos;

    if (!required && peek(TID) && infix()) return 0; // avoid reading op as arg

    if (want(TINT))     return lit(integer(tokint), start);
    if (want(TFALSE))   return lit((value){BOOLE, .i=0}, start);
    if (want(TTRUE))    return lit((value){BOOLE, .i=1}, start);
    if (want(TCHAR))    return lit((value){CHAR, .i=tokint}, start);
    if (want(TSTRING))  return lit(string(toktxt), start);
    if (want(TID))      return var(toktxt->s, start);
    if (want(TLP) || want(TLB)) {
        int     end = token == TLP? TRP: TRB;
        int     n = 0;
        tree    **xs = 0;
        do {
            if (peek(end)) break;
            xs = realloc(xs, ++n * sizeof *xs);
            xs[n - 1] = expr();
        } while (want(TCOMMA));
        need(end);
        if (n == 0) return lit(end == TRP? unit: nil, start);
        if (n == 1 && end == TRP) return xs[0];
        if (end == TRP) return tree(ETUPLE, start,  .n=n, .xs=xs);
        return tree(ELIST, start, .n=n, .xs=xs);
    }
    if (want(TFN)) return function(TARROW);
    if (required) syntax("need expression");
    return 0;
}
tree *iexpr(int level) {
    if (level == 11) {
        tree *lhs = aexpr(true);
        for (tree *rhs; (rhs = aexpr(false)); )
            lhs = app(lhs, rhs);
        return lhs;
    } else {
        tree *lhs = iexpr(level + 1);
        for (struct infix *i; (i = infix()) && i->lhs == level; ) {
            next();
            tree *operator = var(i->id, lhs->pos);
            tree *rhs = iexpr(i->rhs);
            lhs = app(app(operator, lhs), rhs);
        }
        return lhs;
    }
}
tree *expr() {
    struct pos start = pos;

    if (want(TIF)) {
        tree *cond = expr();
        tree *yes = (need(TTHEN), expr());
        tree *no = (need(TELSE), expr());
        return tree(EIF, start, .cond=cond, .yes=yes, .no=no);
    }
    if (want(TCASE)) {
        tree *subject = expr();
        rules *rules = clauses();
        return tree(ECASE, start, .subject=subject, .rules=rules);
    }
    if (want(TLET)) {
        bool recursive = want(TREC);
        rules *rules = (want(TAND), decls());
        tree *subject = (need(TIN), expr());
        return tree(ELET, start, .subject=subject, .rules=rules, .recursive=recursive);
    }
    return iexpr(0);
}

vars *find(char *id, vars *env, int *index) {
    *index = 0;
    for (vars *v = env; v; v = v->next, (*index)++)
        if (v->id == id) return v;
    return 0;
}

/*

    Transformations

    Simplify expressions into easy-to-execute forms.
    Notably, break down case expressions into a sequence of ifs.
    Also, recognise applications of built-in operators and functions.

    Checklist:
    - Tag each new tree created with a position
    - Make sure each newly created tree has `xform()` called upon it
    - Only `xform()` should call itself. Others should just build the tree.

*/

tree *xform_known(tree *e, vars *env) {
    bool unary = e->type==EAPP && e->lhs->type==EID;
    bool binary = e->type==EAPP && e->lhs->type==EAPP && e->lhs->lhs->type==EID;
    int dummy = 0;

    if (unary) {
        vars *v = find(e->lhs->id, env, &dummy);
        return v && v->oper && v->oper < OBINARY
            ? prim(v->oper, e->rhs):
            e;
    } else if (binary) {
        vars *v = find(e->lhs->lhs->id, env, &dummy);
        return v && v->oper && v->oper > OBINARY
            ? oper(e->lhs->rhs, v->oper, e->rhs)
            : e;
    } else return e;
}

tree *xform_pat(tree *e, tree *val, tree *yes, tree *no, vars *env) {
    switch (e->type) {

    case ELIT:      return cond(oper(val, OEQ, e), yes, no);

    case EID:       if (!strcmp(e->id, "_")) return yes;
                    return let(e->id, val, yes, e->pos);

    case ETUPLE:    for (int i = e->n; i-- > 0; ) {
                        tree *sel = oper(val, OINDEX, lit(integer(i), e->pos));
                        yes = xform_pat(e->xs[i], sel, yes, no, env);
                    }
                    yes = cond(oper(prim(OTLEN, val),
                                    OEQ,
                                    lit(integer(e->n), e->pos)),
                               yes,
                               no);
                    yes = cond(prim(OISTUPLE, val), yes, no);
                    return yes;

    case ELIST:     for (int i = e->n; i-- > 0; ) {
                        tree *sel = oper(val, ONTH, lit(integer(i), e->pos));
                        yes = xform_pat(e->xs[i], sel, yes, no, env);
                    }
                    yes = cond(
                        oper(prim(OLEN, val), OEQ, lit(integer(e->n), e->pos)),
                        yes,
                        no);
                    return yes;

    case EOPER:     if (e->oper == OCONS) {
                        yes = xform_pat(e->rhs, prim(OTL, val), yes, no, env);
                        yes = xform_pat(e->lhs, prim(OHD, val), yes, no, env);
                        yes = cond(prim(OISCONS, val), yes, no);
                        return yes;
                    } else goto bad;

    case EAPP:      e = xform_known(e, env);
                    if (e->type == EAPP) goto bad;
                    else return xform_pat(e, val, yes, no, env);

    default:        bad: semantic(e, "illegal pattern");
    }
}

tree *xform_cases(tree *whole, tree *subject, rules *rules, vars *env) {
    if (!rules) return tree(ECRASH, whole->pos, .whole=whole);
    tree *cont = xform_cases(whole, subject, rules->next, env);
    return xform_pat(rules->lhs, subject, rules->rhs, cont, env);
}

tree *xform_defs(rules *rules, tree *final, vars *env) {
    if (!rules) return final;
    tree *bad = tree(ECRASH, rules->lhs->pos, .whole=rules->lhs);
    tree *cont = xform_defs(rules->next, final, env);
    return xform_pat(rules->lhs, rules->rhs, cont, bad, env);
}

void xform(tree **eptr, vars *env) {
    tree *e = *eptr;
    tree *subject;
    vars *v;

static int uid=0;
print("XFORM[%d]: %e\n", ++uid, *eptr);
    switch (e->type) {

    case ELIT:      break;

    case EID:       v = find(e->id, env, &e->index);
                    if (!v) semantic(e, "undefined symbol");
                    if (v->oper) semantic(e, "must call primitive");
                    break;

    case ETUPLE:
    case ELIST:     for (int i = 0; i < e->n; i++)
                        xform(&e->xs[i], env);
                    break;

    case EFN:       if (e->lhs->type == EID) { // Simple var parameter
                        env = new(vars, .id=e->lhs->id, .next=env);
                        xform(&e->rhs, env);
                    } else {
                        tree *par = uniquevar(e->pos);
                        tree *bad = tree(ECRASH, e->lhs->pos, .whole=e->lhs);
                        tree *body = xform_pat(e->lhs, par, e->rhs, bad, env);
                        *eptr = tree(EFN, e->pos, .lhs=par, .rhs=body);
                        xform(eptr, env);
                    }
                    break;

    case EAPP:      e = xform_known(e, env);
                    if (e->type == EAPP) {
                        xform(&e->lhs, env);
                        xform(&e->rhs, env);
                    } else { // re-process as new type
                        *eptr = e;
                        xform(eptr, env);
                    }
                    break;

    case EOPER:     xform(&e->lhs, env);
                    xform(&e->rhs, env);
                    break;

    case EPRIM:     xform(&e->rhs, env);
                    break;

    case EIF:       xform(&e->cond, env);
                    xform(&e->yes, env);
                    xform(&e->no, env);
                    break;

    case ECASE:     xform(&e->subject, env);
                    subject = var("*it*", e->subject->pos);
                    *eptr = xform_cases(e, subject, e->rules, env);
                    *eptr = let("*it*", e->subject, *eptr, subject->pos);
                    xform(eptr, env);
                    break;

    case ELET:      // Recursive function definitions.
                    // Define all functions first.
                    // Then check all of the r.h.s. and body.
                    if (e->recursive) {
                        for (rules *r = e->rules; r; r = r->next) {
                            if (r->lhs->type != EID)
                                semantic(r->lhs, "l.h.s. must be fun name");
                            if (r->rhs->type != EFN)
                                semantic(r->rhs, "r.h.s. must be fun");
                            env = new(vars, .id=r->lhs->id, .next=env);
                        }
                        for (rules *r = e->rules; r; r = r->next)
                            xform(&r->rhs, env);
                        xform(&e->subject, env);
                    }

                    // Single simple variable definition.
                    // Check r.h.s.
                    // Check body w/ env of l.h.s. (always a variable).
                    else if (issingle(e)) {
                        xform(&e->rules->rhs, env);
                        env = new(vars, .id=e->rules->lhs->id, .next=env);
                        xform(&e->subject, env);
                    }

                    // Definition with pattern matching rules.
                    // Transform definitions as case matching.
                    // Each ELET will be a single variable definition.
                    // Re-transform the whole expression afterwards.
                    else {
                        *eptr = xform_defs(e->rules, e->subject, env);
                        xform(eptr, env);
                    }
                    break;

    case ECRASH:    break;
    default:        semantic(e, "UNTRANSFORMED_EXPR");
    }
print("-----[%d]: %e\n", uid--, *eptr);
}

void addinfix(char *id, int lhs, int rhs, enum oper oper) {
    infixes = new(struct infix, intern(id, -1)->s, lhs, rhs, oper, infixes);
}

value evaloper(tree *e, value x, value y) {
    int a, b;

    switch (e->oper) {

    case OADD: case OSUB: case OMUL: case ODIV: case OREM:
    case OLT: case OGT: case OLE: case OGE:

        if (!isint(x) || !isint(y))
            semantic(e, "NON_INTEGER %V %s %V", x, opers[e->oper], y);

        switch (e->oper) {
        case OADD: return integer(a + b);
        case OSUB: return integer(a - b);
        case OMUL: return integer(a * b);
        case ODIV: return integer(a / b);
        case OREM: return integer(a % b);
        case OLT:   return boole(a < b);
        case OGT:   return boole(a > b);
        case OLE:   return boole(a <= b);
        case OGE:   return boole(a >= b);
        }

    case OEQ:   return boole(equal(x, y));
    case ONE:   return boole(!equal(x, y));

    case OCONS:
        if (y.type != LIST) semantic(e, "NON_LIST %V:%V", x, y);
        return y.type == LIST? cons(x, y): unit;

    case OINDEX:
        if (x.type != TUPLE) semantic(e, "NON_TUPLE %V ~index~ %V", x, y);
        if (y.type != INT) semantic(e, "NON_INTEGER %V ~index~ %V", x, y);
        if (y.i < 0 || y.i >= x.tuple->n) semantic(e, "BOUNDS %V ~index~ %V", x, y);
        return x.tuple->xs[y.i];

    case ONTH:  for (int i = y.i; i && iscons(x); i--)
                    x = x.cons->tl;
                return iscons(x)? x.cons->hd: unit;

    // case OSEQ:  // handled especially

    case OHD:
        if (isnil(x)) semantic(e, "EMPTY");
        if (!iscons(x)) semantic(e, "NON_LIST %V", x);
        return x.cons->hd;

    case OTL:
        if (isnil(x)) semantic(e, "EMPTY");
        if (!iscons(x)) semantic(e, "NON_LIST %V", x);
        return x.cons->tl;

    case OLEN:
        if (x.type != LIST) semantic(e, "NON_LIST %V", x);
        for (int n = 0; ; x = x.cons->tl, n++)
            if (isnil(x)) return integer(n);

    case OTLEN:
        if (x.type != TUPLE) semantic(e, "NON_TUPLE %V", x);
        return integer(x.tuple->n);

    case OISBOOL:   return boole(x.type == BOOLE);
    case OISINT:    return boole(x.type == INT);
    case OISCHAR:   return boole(x.type == CHAR);
    case OISSTRING: return boole(x.type == STRING);
    case OISTUPLE:  return boole(x.type == TUPLE);
    case OISLIST:   return boole(x.type == LIST);
    case OISFN:     return boole(x.type == FN);
    case OISCONS:   return boole(iscons(x));

    case OPRINT:
        printvalue(x, false);
        return x;

    default: semantic(e, "UNHANDLED_ARITH");
    }
}

value eval(tree *e, struct values *env) {
    struct values *ie;
    struct tuple *tuple;
    value x, y;

top:

    switch (e->type) {
    case ELIT:      return e->val;
    case EID:       ie = env;
print("%s=%d", e->id, e->index);
print("{");for (struct values *i=env; i; i=i->next) print("%V,",i->x);print("}\n");
                    for (int i = e->index; i; i--) ie = ie->next;
                    return ie->x;

    case ETUPLE:    tuple = malloc(sizeof *tuple + e->n * sizeof *tuple->xs);
                    tuple->n = e->n;
                    for (int i = 0; i < e->n; i++)
                        tuple->xs[i] = eval(e->xs[i], env);
                    return (value) {TUPLE, .tuple=tuple};

    case ELIST:     {
                        value first = nil;
                        value *p = &first;
                        for (int i = 0; i < e->n; i++) {
                            *p = cons(eval(e->xs[i], env), nil);
                            p = &p->cons->tl;
                        }
                        return first;
                    }

    case EFN:       return (value) {FN, .fn=new(struct fn, .body=e->rhs, .env=env)};

    case EAPP:      x = eval(e->lhs, env);
                    if (x.type != FN) semantic(e, "NON_FUN! %V", x);
                    y = eval(e->rhs, env);
                    e = x.fn->body;
                    env = new(struct values, y, x.fn->env);
                    goto top;

    case EOPER:     if (e->oper == OSEQ) { // Tail call sequence
                        eval(e->lhs, env);
                        e = e->rhs;
                        goto top;
                    }
                    x = eval(e->lhs, env);
                    y = eval(e->rhs, env);
                    return evaloper(e, x, y);

    case EPRIM:     return evaloper(e, eval(e->rhs, env), unit);

    case EIF:       x = eval(e->cond, env);
                    if (x.type != BOOLE) semantic(e, "NON_BOOLEAN! %V", x);
                    e = x.i? e->yes: e->no;
                    goto top;

    case ELET:      if (e->recursive) {
                        struct values *newenv = env;
                        for (rules *r = e->rules; r; r = r->next)
                            newenv = new(struct values, eval(r->rhs, 0), newenv);
                        for (struct values *i = newenv; i != env; i = i->next)
                            i->x.fn->env = newenv;
                        env = newenv;
                        e = e->subject;
                        goto top;
                    } else {
                        // All non-recursive lets are single-variable.
                        x = eval(e->rules->rhs, env);
                        env = new(struct values, x, env);
                        e = e->subject;
                        goto top;
                    }

    case ECRASH:    semantic(e->whole, "NO_MATCH!");

    // case ECASE:  // No longer exists

    default:        bad: semantic(e, "UNEVALED");
    }

}

int main(int argc, char **argv) {
    setvbuf(stdout, 0, _IONBF, 0);

    for (int i = 0; toks[i]; i++) toks[i] = intern(toks[i], -1)->s;

    addinfix("*",   7, 8, OMUL);
    addinfix("/",   7, 8, ODIV);
    addinfix("rem", 7, 8, OREM);
    addinfix("+",   6, 7, OADD);
    addinfix("-",   6, 7, OSUB);
    addinfix(":",   5, 5, OCONS);
    addinfix("@",   5, 6, ONTH);
    addinfix("==",  4, 5, OEQ);
    addinfix("<>",  4, 5, ONE);
    addinfix("<",   4, 5, OLT);
    addinfix(">",   4, 5, OGT);
    addinfix("<=",  4, 5, OLE);
    addinfix(">=",  4, 5, OGE);
    addinfix(";",   1, 1, OSEQ);

    unit.tuple = calloc(1, sizeof *unit.tuple);

    vars *ground = 0;
    for (int i = 0; opers[i]; i++)
        ground = new(vars, .id=intern(opers[i], -1)->s, .oper=i, .next=ground);

    for (argv++; *argv; argv++) {
        opensource(*argv);
        tree *e = expr();
        // print("# %e\n", e);
        xform(&e, ground);
        print("# %e\n", e);
        value x = eval(e, 0);
        print("> %V\n", x);
    }
    puts("done.");
}
