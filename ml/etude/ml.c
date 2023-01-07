#ifdef GC
#include <gc.h>
#define malloc GC_malloc
#define realloc GC_realloc
#endif

#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define new(t,...) memcpy(malloc(sizeof(t)), &(t){__VA_ARGS__}, sizeof(t))

typedef struct {int n; char s[];} String;
typedef struct {char *fn; int ln;} Pos;
struct part {Pos pos; bool rec; struct rule *defs;};
struct infix {char *id; int lhs, rhs; struct infix *next;};

typedef const struct _ast ast;

enum token {
    TEOF,TINT,TCHAR,TSTRING,TLP,TRP,TLB,TRB,TCOMMA,TSEMI,TID,TEQUAL,
    TIF,TTHEN,TELSE,TCASE,TBAR,TARROW,TLET,TREC,TAND,TIN,TFN,
    TTRUE,TFALSE,TDEREF,
};
char *tokens[] = {
    "end of file","int","char","string","(",")","[","]",",",";","id",
    "=","if","then","else","case","|","->","let","rec","and",
    "in","fn","true","false","!",0
};

enum op { // Primitive operations.
    OISBOOL,OISINT,OISSTR,OISCONS,OISLIST,OISTUP,OISFN,OSLEN,
    OCHR,OORD,OIMPL,OLEN,OTLEN,OHD,OTL,OREF,ODEREF,OPRINT,
    OREADFILE,OEXIT,OBINARY,OEQ,ONE,OLT,OGT,OLE,OGE,OCONS,OCAT,
    OADD,OSUB,OMUL,ODIV,OREM,OIDX,OTIDX,OCHARAT,OSET,OSEQ,OTOTAL
};
char *ops[] = {
    "isbool","isint","isstring","iscons","islist","istuple",
    "isfn","strlen","chr","ord","implode","length",
    "tuplelength","hd","tl","ref","!","print","readfile","exit",
    "","==","<>","<",">","<=",">=",":","^","+","-","*","/",
    "rem","@","@*","char_at",":=",";",0
};

typedef struct value {
    enum {BOOLE,CHAR,INT,STRING,TUPLE,LIST,REF,FN} type;
    union {
        int             i;
        String          *s;
        struct tuple    *t;
        struct list     *l;
        struct fn       *f;
        struct value    *ref;
    };
} value;

struct list {value hd, tl;};
struct tuple {int n; value xs[];};
typedef struct values {value x; struct values *next;} values;
struct finfo {char *id; int got;};
struct fn {ast *e; values *vars; struct finfo *info;};

struct _ast {
    enum {
        ELIT,EID,ETUPLE,ELIST,EFN,EAPP,EBIN,EUN,EIF,ECASE,ELET,
        ECRASH
    } form;
    Pos     pos;
    union {
        value x;
        struct {char *id; int index;};
        struct {int n; ast **es;};
        struct {ast *lhs, *rhs; enum op op; struct finfo *info;};
        struct {ast *cond, *yes, *no;};
        struct {ast *sub; struct rule *cases;};
        struct {bool rec; struct rule *defs; ast *in;};
        struct {ast *ctx, *offender;};
    };
};
struct rule {ast *lhs, *rhs; struct rule *next;};

typedef struct varenv {char *id; struct varenv *next;} varenv;

char            source[65536];
char            *src;
enum token      token;
bool            peeked;
int             tokint;
char            tokbuf[sizeof source];
String          *toktxt;
String          **interns;
int             ninterns;
Pos             srcpos;
struct infix    *infixes;
struct part     *parts = 0;
int             nparts = 0;
value           opvals[OTOTAL];     // Function values for primitives.
const value     nil = {LIST, .l=0};
const value     unit = {TUPLE, .t=&(struct tuple) {0}};

#define ast(f,p,...) new(struct _ast, .form=(f), .pos=(p), __VA_ARGS__)
ast *expr();
ast *aexpr(bool required);
void pr(char *msg, ...);

String *newstring(char *txt, int n) {
    if (n < 0) n = strlen(txt);

    String *str = malloc(sizeof *str + n + 1);
    if (txt) memcpy(str->s, txt, n);
    str->s[n] = 0;
    str->n = n;
    return str;
}

String *intern(char *txt, int n) {
    if (n < 0) n = strlen(txt);

    for (int i = 0; i < ninterns; i++)
        if (interns[i]->n == n && !memcmp(interns[i]->s, txt, n))
            return interns[i];

    interns = realloc(interns, (ninterns + 1) * sizeof *interns);
    return interns[ninterns++] = newstring(txt, n);
}

bool isnil(value x) { return x.type == LIST && !x.l; }
bool iscons(value x) { return x.type == LIST && x.l; }
bool isbool(value x) { return x.type == BOOLE; }
bool isint(value x) { return x.type == INT; }
bool ischar(value x) { return x.type == CHAR; }
bool isstring(value x) { return x.type == STRING; }
bool islist(value x) { return x.type == LIST; }
bool istuple(value x) { return x.type == TUPLE; }
bool isref(value x) { return x.type == REF; }
bool isfn(value x) { return x.type == FN; }
value boole(bool x) { return (value) {BOOLE, .i=x}; }
value integer(int i) { return (value) {INT, .i=i}; }
value character(int i) { return (value) {CHAR, .i=i}; }
value string(String *s) { return (value) {STRING, .s=s}; }
value ref(value x) { return (value) {REF, .ref=new(value[1], x)}; }
value newtuple(int n) {
    struct tuple *t = malloc(sizeof *t + n * sizeof *t->xs);
    t->n = n;
    return (value) {TUPLE, .t=t};
}
value cons(value hd, value tl) {
    return (value) {LIST, .l=new(struct list, hd, tl)};
}
value fn(ast *e, values *vars, struct finfo *info) {
    return (value) {FN, .f=new(struct fn, .e=e, .vars=vars, .info=info)};
}
value catenate(String *a, String *b) {
    String *c = newstring(0, a->n + b->n);
    memcpy(c->s, a->s, a->n);
    memcpy(c->s + a->n, b->s, b->n);
    return string(c);
}

bool equal(value x, value y) {
    if (x.type != y.type) return false;

    switch (x.type) {
    case BOOLE:     return x.i == y.i;
    case CHAR:      return x.i == y.i;
    case INT:       return x.i == y.i;
    case STRING:    return x.s == y.s ||
                        x.s->n == y.s->n && !memcmp(x.s->s, y.s->s, x.s->n);
    case TUPLE:     if (x.t->n != y.t->n) return false;
                    for (int i = 0; i < x.t->n; i++)
                        if (!equal(x.t->xs[i], y.t->xs[i])) return false;
                    return true;
    case LIST:      for ( ; iscons(x) && iscons(y); x=x.l->tl, y=y.l->tl)
                        if (!equal(x.l->hd, y.l->hd));
                    return isnil(x) && isnil(y);
    case REF:       return equal(*x.ref, *y.ref);
    case FN:        return x.f == y.f;
    }
    return false;
}

void printexpr(ast *e) {
    switch (e->form) {
    case ELIT:      pr("%v", e->x); break;
    case EID:       pr("%s", e->id); break;
    case ETUPLE:
    case ELIST:     pr(e->form == ETUPLE? "(": "[");
                    for (int i = 0; i < e->n; i++)
                        pr("%s%e", i? ", ": "", e->es[i]);
                    pr(e->form == ETUPLE? ")": "]");
                    break;
    case EFN:       pr("(fn %e->%e)", e->lhs, e->rhs); break;
    case EUN:       pr("(%s %e)", ops[e->op], e->rhs); break;
    case EBIN:      pr("(%e %s %e)", e->lhs, ops[e->op], e->rhs); break;
    case EAPP:      pr("(%e %e)", e->lhs, e->rhs); break;
    case EIF:       pr("(if %e then %e else %e)", e->cond, e->yes, e->no);break;
    case ECASE:     pr("(case %e", e->sub);
                    for (struct rule *i = e->cases; i; i = i->next)
                        pr(" | %e->%e", i->lhs, i->rhs);
                    pr(")");
                    break;
    case ELET:      pr("let %s", e->rec? "rec ": "");
                    for (struct rule *i = e->defs; i; i = i->next)
                        pr("%e=%e %s", i->lhs, i->rhs, i->next? "and ": "");
                    pr("in %e", e->in);
                    break;
    case ECRASH:    pr("(*CRASH*)"); break;
    }
}

void printvalue(value x, bool decorate) {
    switch (x.type) {
    case BOOLE:     pr(x.i? "true": "false"); break;
    case CHAR:      pr(decorate? "'%C'": "%c", x.i); break;
    case INT:       pr("%d", x.i); break;
    case STRING:    if (decorate) {
                        pr("\"");
                        for (int i = 0; i < x.s->n; i++) pr("%C", x.s->s[i]);
                        pr("\"");
                    } else pr("%s", x.s->s);
                    break;
    case TUPLE:     pr("(");
                    for (int i = 0; i < x.t->n; i++)
                        pr("%s%v", i? ", ": "", x.t->xs[i]);
                    pr(")");
                    break;
    case LIST:      pr("[");
                    for (value i = x; !isnil(i); i = i.l->tl)
                        pr("%v%s", i.l->hd, isnil(i.l->tl)? "": ", ");
                    pr("]");
                    break;
    case REF:       pr("(ref %v)", *x.ref); break;
    case FN:        {
                        int n = x.f->info->got;
                        if (n) pr("(");
                        pr("%s", x.f->info->id? x.f->info->id: "(fn)");
                        for (int i = 0; i < n; i++) {
                            values *v = x.f->vars;
                            for (int j = 0; j < n - i - 1; j++) v = v->next;
                            pr(" %v", v->x);
                        }
                        if (n) pr(")");
                        break;
                    }
    }

}

void vpr(char *msg, va_list ap) {
    char c, *msg2;
    for (char *s = msg; *s; s++)
        if (*s != '%') putchar(*s);
        else switch (*++s) {
        case 'C': c = va_arg(ap, int);
                  if (c == '\n') printf("\\n");
                  else if (c == '\t') printf("\\t");
                  else if (c == '\"') printf("\\\"");
                  else putchar(c);
                  break;
        case 'c': putchar(va_arg(ap, int)); break;
        case 'd': printf("%d", va_arg(ap, int)); break;
        case 's': printf("%s", va_arg(ap, char *)); break;
        case 'e': printexpr(va_arg(ap, ast *)); break;
        case 'v': printvalue(va_arg(ap, value), true); break;
        case 'V': printvalue(va_arg(ap, value), false); break;
        case '!': putchar('\n'); exit(1); break;
        case '*': msg2 = va_arg(ap, char *);
                    vpr(msg2, *va_arg(ap, va_list *));
                    break;
        }
}

void pr(char *msg, ...) {
    va_list ap; va_start(ap, msg);
    vpr(msg, ap);
    va_end(ap);
}

void syntax(char *msg, ...) {
    va_list ap; va_start(ap, msg);
    pr("ml: error %s:%d: %* before %s%!",
        srcpos.fn, srcpos.ln, msg, &ap, tokens[token]);
}

void *semantic(ast *e, char *msg, ...) {
    va_list ap; va_start(ap, msg);
    pr("ml: error %s:%d: %*\nexpr: %e%!", e->pos.fn, e->pos.ln, msg, &ap, e);
    return 0;
}

void opensrc(char *fn) {
    srcpos = (Pos) {strdup(fn), 1};
    src = source;
    peeked = false;

    FILE *file = fopen(fn, "rb");
    if (!file) syntax("cannot open file");
    source[fread(source, 1, sizeof source, file)] = 0;
    fclose(file);
}

int esc() {
    if (*src == '\\')
        switch (*++src) {
        case 'n':   return '\n'; break;
        case 't':   return '\t'; break;
        default:    return *src;
        }
    return *src;
}

enum token next() {
    char *t = tokbuf;
    char *opchr = "!$%&*+-./:<=>?@^|~";

    if (peeked) return peeked = false, token;

    while (isspace(*src) || *src == '#')
        if (*src == '#') while (*src && *src != '\n') src++;
        else if (*src == '\n') src++, srcpos.ln++;
        else src++;

    if (!*src) return token = 0;
    if (*src == ';')
        return src++, toktxt = intern(";", 1), token = TSEMI;
    for (int i = TLP; i <= TCOMMA; i++)
        if (*src == tokens[i][0]) return src++, token = i;
    if (isdigit(src[*src == '-']))
        return tokint = strtol(src, &src, 10), token = TINT;
    if (*src == '\'') {
        src++;
        tokint = esc();
        if (*++src != '\'') syntax("unclosed character");
        return src++, token = TCHAR;
    }
    if (*src == '"')
        for (src++; ; src++)
            if (!*src || *src == '\n') syntax("unclosed string");
            else if (*src == '"') {
                src++;
                return toktxt = intern(tokbuf, t - tokbuf), token = TSTRING;
            } else
                *t++ = esc();

    while (isalnum(*src) || *src == '_' || *src == '\'') *t++ = *src++;
    if (t == tokbuf) while (*src && strchr(opchr, *src)) *t++ = *src++;
    if (t == tokbuf) syntax("not a token: %c", *src);
    toktxt = intern(tokbuf, t - tokbuf);

    for (char **i = tokens + TID + 1; *i; i++)
        if (*i == toktxt->s) return token = i - tokens;
    return token = TID;
}

bool peek(enum token t) { peeked = next(); return token == t; }
bool want(enum token t) { peeked = next() != t; return !peeked; }
void need(enum token t) { if (!want(t)) syntax("need %s", tokens[t]); }

ast *lit(Pos pos, value x) { return ast(ELIT, pos, .x=x); }
ast *app(ast *f, ast *x) { return ast(EAPP, f->pos, .lhs=f, .rhs=x); }
ast *binary(ast *lhs, enum op op, ast *rhs) {
    return ast(EBIN, lhs->pos, .lhs=lhs, .rhs=rhs, .op=op);
}
ast *unary(enum op op, ast *rhs) {
    return ast(EUN, rhs->pos, .rhs=rhs, .op=op);
}
ast *var(Pos pos, char *id, int index) {
    return ast(EID, pos, .id=intern(id, -1)->s, .index=index);
}
ast *uniquevar(Pos pos) {
    static int count;
    char buf[16];
    sprintf(buf, "__%d", ++count);
    return var(pos, buf, -1);
}
ast *_if(ast *cond, ast *yes, ast *no) {
    return ast(EIF, cond->pos, .cond=cond, .yes=yes, .no=no);
}
ast *let(Pos pos, char *id, ast *x, ast *in) {
    struct rule *defs = new(struct rule, .lhs=var(pos, id,-1), .rhs=x, .next=0);
    return ast(ELET, pos, .rec=false, .defs=defs, .in=in);
}
ast *crash(ast *ctx, ast *offender) {
    return ast(ECRASH, ctx->pos, .ctx=ctx, .offender=offender);
}

bool istrivial(ast *e) { return e->form == ELIT || e->form == EID; }

struct infix *findinfix() {
    if (peek(TID) || peek(TSEMI))
        for (struct infix *i = infixes; i; i = i->next)
            if (i->id == toktxt->s) return i;
    return 0;
}

ast *funexpr(enum token separator, char *id, int got) {
    if (want(separator)) return expr();
    ast *lhs = aexpr(true);
    ast *rhs = funexpr(separator, id, got + 1);
    struct finfo *info = new(struct finfo, .id=id, .got=got);
    return ast(EFN, lhs->pos, .lhs=lhs, .rhs=rhs, .info=info);
}

ast *aexpr(bool required) {
    Pos pos = (peek(0), srcpos);
    if (!required && findinfix()) return 0; // Avoid eating operator as an arg.
    if (want(TDEREF))   return unary(ODEREF, aexpr(true));
    if (want(TINT))     return lit(pos, integer(tokint));
    if (want(TCHAR))    return lit(pos, character(tokint));
    if (want(TSTRING))  return lit(pos, string(toktxt));
    if (want(TTRUE))    return lit(pos, boole(true));
    if (want(TFALSE))   return lit(pos, boole(false));
    if (want(TID))      return var(pos, toktxt->s, -1);
    if (want(TLP) || want(TLB)) {
        enum token end = token == TLP? TRP: TRB;
        int n = 0;
        ast **es = 0;
        do {
            if (peek(end)) break;
            es = realloc(es, (n + 1) * sizeof *es);
            es[n++] = expr();
        } while (want(TCOMMA));
        need(end);

        if (n == 0) return lit(pos, end == TRP? unit: nil);
        if (end == TRP && n == 1) return es[0];
        return ast(end == TRP? ETUPLE: ELIST, pos, .n=n, .es=es);
    }
    if (want(TFN)) return funexpr(TARROW, 0, 0);
    if (required) syntax("need expression");
    return 0;
}

struct rule *caserules() {
    if (!want(TBAR)) return 0;
    ast *lhs = expr();
    ast *rhs = (want(TARROW), expr());
    struct rule *next = caserules();
    return new(struct rule, lhs, rhs, next);
}

struct rule *letdefs() {
    ast *lhs = aexpr(true);
    ast *rhs = funexpr(TEQUAL, lhs->form == EID? lhs->id: 0, 0);
    struct rule *next = want(TAND)? letdefs(): 0;
    return new(struct rule, lhs, rhs, next);
}

ast *cexpr(int level) {

    if (level == 0) {
        Pos pos = (peek(0), srcpos);

        if (want(TIF)) {
            ast *cond = expr();
            ast *yes = (need(TTHEN), expr());
            ast *no = (need(TELSE), expr());
            return _if(cond, yes, no);
        }
        if (want(TCASE)) {
            ast *sub = expr();
            struct rule *rules = caserules();
            return ast(ECASE, sub->pos, .sub=sub, .cases=rules);
        }
        if (want(TLET)) {
            bool rec = want(TREC);
            struct rule *defs = (want(TAND), letdefs());
            ast *in = (need(TIN), expr());
            return ast(ELET, pos, .rec=rec, .defs=defs, .in=in);
        }
    }

    if (level == 11) {
        ast *lhs = aexpr(true);
        ast *rhs;
        while ((rhs = aexpr(false))) // Function application
            lhs = app(lhs, rhs);
        return lhs;
    }

    ast *lhs = cexpr(level + 1);
    struct infix *op;
    while ((op = findinfix()) && op->lhs == level) { // Binary operator
        next();
        ast *rhs = cexpr(op->rhs);
        if (!strcmp(op->id, "&&"))
            lhs = _if(lhs, rhs, lit(rhs->pos, boole(0)));
        else if (!strcmp(op->id, "||"))
            lhs = _if(lhs, lit(rhs->pos, boole(1)), rhs);
        else
            lhs = app(app(var(lhs->pos, op->id, -1), lhs), rhs);
    }
    return lhs;
}

ast *expr() { return cexpr(0); }

void readscript() {
    while (!peek(TEOF)) {
        Pos pos = srcpos;
        need(TLET);
        bool rec = want(TREC);
        struct rule *defs = (want(TAND), letdefs());

        parts = realloc(parts, (++nparts) * sizeof *parts);
        parts[nparts - 1] = (struct part) {pos, rec, defs};
    }
}

varenv *find(char *id, varenv *vars, int *index) {
    *index = 0;
    for (varenv *i = vars; i; i = i->next, (*index)++)
        if (i->id == id) return i;
    return 0;
}

int findop(char *id) {
    for (int i = 0; ops[i]; i++) if (ops[i] == id) return i;
    return -1;
}

ast *xform_app(ast *e) {
    // Try to directly apply unary.
    if (e->lhs->form == EID) {
        int i = findop(e->lhs->id);
        if (i >= 0 && i < OBINARY)
            return unary(i, e->rhs);
    }

    // Try to directly apply binary.
    if (e->lhs->form == EAPP && e->lhs->lhs->form == EID) {
        int i = findop(e->lhs->lhs->id);
        if (i >= 0 && i > OBINARY)
            return binary(e->lhs->rhs, i, e->rhs);
    }

    return e;
}

ast *xform_pat(ast *e, ast *x, ast *yes, ast *no) {
    ast *tmp;

    switch (e->form) {

    case ELIT:      return _if(binary(e, OEQ, x), yes, no);

    case EID:       return !strcmp("_", e->id) && istrivial(x)
                             ? yes
                             : let(e->pos, e->id, x, yes);

    case ETUPLE:    for (int i = e->n; i-- > 0; ) {
                        ast *ie = binary(x, OTIDX, lit(x->pos, integer(i)));
                        yes = xform_pat(e->es[i], ie, yes, no);
                    }
                    tmp = binary(unary(OTLEN, x),
                                 OEQ,
                                 lit(e->pos, integer(e->n)));
                    yes = _if(tmp, yes, no);
                    return _if(unary(OISTUP, x), yes, no);

    case ELIST:    for (int i = e->n; i-- > 0; ) {
                        ast *ie = binary(x, OIDX, lit(x->pos, integer(i)));
                        yes = xform_pat(e->es[i], ie, yes, no);
                    }
                    tmp = binary(unary(OLEN, x),
                                 OEQ,
                                 lit(e->pos, integer(e->n)));
                    yes = _if(tmp, yes, no);
                    return _if(unary(OISCONS, x), yes, no);

    case EBIN:      if (e->op == OCONS) {
                        yes = xform_pat(e->rhs, unary(OTL, x), yes, no);
                        yes = xform_pat(e->lhs, unary(OHD, x), yes, no);
                        return _if(unary(OISCONS, x), yes, no);
                    }
                    goto bad;

    case EAPP:      tmp = xform_app(e);
                    if (tmp->form == EAPP) goto bad;
                    return xform_pat(tmp, x, yes, no);

    default:        bad: return semantic(e, "illegal pattern");
    }
}

ast *xform_cases(ast *x, struct rule *r, ast *no) {
    // x is the subject of the case, which is always a variable
    return r? xform_pat(r->lhs, x, r->rhs, xform_cases(x, r->next, no)): no;
}

ast *xform_defs(struct rule *r, ast *yes) {
    if (!r) return yes;

    // If l.h.s. is a decomposition form, we need to save the value.
    // If we didn't, each sub-test would recalculate r.h.s.
    if (r->lhs->form != EID && !istrivial(r->rhs)) {
        ast *var = uniquevar(r->rhs->pos);
        ast *no = crash(r->lhs, var);
        ast *cont = xform_defs(r->next, yes);
        ast *body = xform_pat(r->lhs, var, cont, no);
        return let(r->lhs->pos, var->id, r->rhs, body);
    }
    ast *no = crash(r->lhs, r->rhs);
    return xform_pat(r->lhs, r->rhs, xform_defs(r->next, yes), no);
}

struct rule *copyrules(struct rule *r) {
    return r? new(struct rule, r->lhs, r->rhs, copyrules(r->next)): 0;
}

ast *xform(ast *e, varenv *vars) {
    varenv  *v;
    ast    *tmp, *x, *y, **es;
    int     index;

    switch (e->form) {
    case ELIT:      return e;

    case EID:       // Find de Bruijn index for variable.
                    if ((v = find(e->id, vars, &index))) {
                        // Create new object with correct index.
                        // DO NOT UPDATE, EID objects can be shared.
                        // (e.g. a temp created for a case expression)
                        return var(e->pos, e->id, index);
                    }

                    // Reverence static primitive, if so
                    if ((index = findop(e->id)) >= 0)
                        return lit(e->pos, opvals[index]);

                    // Otherwise, the variable is undefined.
                    return semantic(e, "undefined symbol");

    case ETUPLE:
    case ELIST:     es = (void*) malloc(e->n * sizeof *es);
                    for (int i = 0; i < e->n; i++)
                        es[i] = xform(e->es[i], vars);
                    return ast(e->form, e->pos, .n=e->n, .es=es);

    case EFN:       // Simple one-variable function.
                    // Check r.h.s. with parameter added to variables.
                    if (e->lhs->form == EID) {
                        tmp = xform(e->rhs, new(varenv, e->lhs->id, vars));
                        return ast(EFN, e->pos, .lhs=e->lhs,
                                   .rhs=tmp, .info=e->info);
                    }

                    // Otherwise, introduce named parameter and xform pattern.
                    else {
                        ast *uid = uniquevar(e->lhs->pos);
                        ast *body = xform_pat(e->lhs, uid, e->rhs, crash(e, uid));
                        e = ast(EFN, e->pos, .lhs=uid, .rhs=body, .info=e->info);
                        return xform(e, vars);
                    }

    case EUN:       return ast(EUN, e->pos, .op=e->op, .rhs=xform(e->rhs, vars));

    case EBIN:      x = xform(e->lhs, vars);
                    y = xform(e->rhs, vars);
                    return ast(EBIN, e->pos, .lhs=x, .op=e->op, .rhs=y);

    case EAPP:      tmp = xform_app(e);
                    if (tmp->form == EAPP) {
                        x = xform(e->lhs, vars);
                        y = xform(e->rhs, vars);
                        return app(x, y);
                    } else
                        return xform(tmp, vars);

    case EIF:       tmp = xform(e->cond, vars);
                    x = xform(e->yes, vars);
                    y = xform(e->no, vars);
                    return _if(tmp, x, y);

    case ECASE:     if (e->sub->form == EID) {
                        x = xform_cases(e->sub, e->cases, crash(e, e->sub));
                        return xform(x, vars);
                    } else {
                        // Create a temporary to hold the subject value.
                        ast *sub = uniquevar(e->sub->pos);
                        ast *in = xform_cases(sub, e->cases, crash(e, sub));
                        ast *out = let(sub->pos, sub->id, e->sub, in);
                        return xform(out, vars);
                    }

    case ELET:      if (e->rec) {
                        struct rule *defs = copyrules(e->defs);

                        // Define all functions first.
                        // All definitions will have IDs on the l.h.s.
                        for (struct rule *i = defs; i; i = i->next) {
                            if (i->lhs->form != EID)
                                return semantic(i->lhs, "l.h.s. must be var");
                            if (i->rhs->form != EFN)
                                return semantic(i->rhs, "r.h.s. must be fn");
                            vars = new(varenv, i->lhs->id, vars);
                        }

                        // Transform copy of rules.
                        for (struct rule *i = defs; i; i = i->next)
                            i->rhs = xform(i->rhs, vars);

                        tmp = xform(e->in, vars);
                        return ast(ELET, e->pos, .rec=1, .defs=defs, .in=tmp);
                    }
                    // Single variable let definition (`let x = ... in ...`).
                    // Every non-recursive let will end up in this form.
                    else if (!e->defs->next && e->defs->lhs->form == EID) {
                        varenv *newvars = new(varenv, e->defs->lhs->id, vars);
                        struct rule *defs = copyrules(e->defs);
                        defs->rhs = xform(defs->rhs, vars);
                        tmp = xform(e->in, newvars);
                        return ast(ELET, e->pos, .rec=0, .defs=defs, .in=tmp);
                    }
                    // Break down multiple and/or pattern defs into singles.
                    // Reprocess once broken down.
                    else
                        return xform(xform_defs(e->defs, e->in), vars);

    case ECRASH:    tmp = xform(e->offender, vars); // Index offender var
                    return ast(ECRASH, e->pos, .ctx=e->ctx, .offender=tmp);

    default:        return semantic(e, "UNTRANSFORMED");
    }
}

void addinfix(int lhs, int rhs, char **ids) {
    for (char **i = ids; *i; i++)
        infixes = new(struct infix, intern(*i, -1)->s, lhs, rhs, infixes);
}

value eval_op(ast *e, value x, value y) {
    int     n;
    value   i;
    char    *buf;

    switch (e->op) {

    case OISBOOL:   return boole(isbool(x));
    case OISINT:    return boole(isint(x));
    case OISSTR:    return boole(isstring(x));
    case OISCONS:   return boole(iscons(x));
    case OISLIST:   return boole(islist(x));
    case OISTUP:    return boole(istuple(x));
    case OISFN:     return boole(isfn(x));

    case OSLEN:     if (!isstring(x)) semantic(e, "NON_STRING_ORD %v", x);
                    return integer(x.s->n);

    case OCHR:      if (!isint(x)) semantic(e, "NON_INTEGER_CHR %v", x);
                    return character(x.i);

    case OORD:      if (!ischar(x)) semantic(e, "NON_CHAR_ORD %v", x);
                    return integer(x.i);

    case OIMPL:     if (!islist(x))
                        not_chars: semantic(e, "NOT_CHAR_LIST %v", x);
                    for (n = 0, i = x; iscons(i); i = i.l->tl)
                        if (!ischar(i.l->hd)) goto not_chars;
                        else n++;
                    buf = malloc(n + 1);
                    for (n = 0, i = x; iscons(i); i = i.l->tl)
                        buf[n++] = i.l->hd.i;
                    return string(newstring(buf, n));

    case OLEN:      if (!islist(x)) semantic(e, "NOT_LIST %v", x);
                    for (n = 0; !isnil(x); x = x.l->tl) n++;
                    return integer(n);

    case OTLEN:     if (!istuple(x)) semantic(e, "NOT_TUPLE %v", x);
                    return integer(x.t->n);

    case OHD:       if (!iscons(x)) semantic(e, "HD_NOT_LIST %v", x);
                    return x.l->hd;

    case OTL:       if (!iscons(x)) semantic(e, "TL_NOT_LIST %v", x);
                    return x.l->tl;

    case OREF:      return ref(x);

    case ODEREF:    if (!isref(x)) semantic(e, "NOT_REF %v", x);
                    return *x.ref;

    case OPRINT:    pr("%V", x);
                    return x;

    case OREADFILE: {
                        if (!isstring(x)) semantic(e, "NOT_STRING %v", x);
                        FILE *file = fopen(x.s->s, "rb");
                        if (!file) return unit;
                        fseek(file, 0, SEEK_END);
                        int len = ftell(file);
                        rewind(file);
                        buf = malloc(len);
                        fread(buf, 1, len, file);
                        fclose(file);
                        return string(newstring(buf, len));
                    }
    case OEXIT:     exit(isint(x)? x.i: 0);

    case OCONS:
        if (!islist(y)) semantic(e, "NON_LIST_TAIL %v", y);
        return cons(x, y);
    case OCAT:
        if (!isstring(x) || !isstring(y))
            semantic(e, "NON_STRING_CAT %v ^ %v", x, y);
        return catenate(x.s, y.s);

    case OIDX:
        if (!islist(x)) semantic(e, "NON_LIST_BASE %v", x);
        if (!isint(y)) semantic(e, "NON_INTEGER_INDEX %v", y);
        for (i = x, n = y.i; n-- > 0 && iscons(i); ) i = i.l->tl;
        if (isnil(i)) semantic(e, "BOUNDS %v @ %v", x, y);
        return i.l->hd;

    case OTIDX:
        if (!istuple(x)) semantic(e, "NON_TUPLE_BASE %v", x);
        if (!isint(y)) semantic(e, "NON_INTEGER_INDEX %v", y);
        if (y.i < 0 || y.i >= x.t->n) semantic(e, "BOUNDS %v @* %v", x, y);
        return x.t->xs[y.i];

    case OCHARAT:
        if (!isstring(x)) semantic(e, "NON_STRING_BASE %v", x);
        if (!isint(y)) semantic(e, "NON_INTEGER_INDEX %v", y);
        if (y.i < 0 || y.i >= x.s->n) semantic(e, "BOUNDS (char_at %v %v)", x, y);
        return character((int) x.s->s[y.i] & 255);

    case OSET:
        if (!isref(x)) semantic(e, "NON_REF %v", x);
        *x.ref = y;
        return x;

    case OEQ:       return boole(equal(x, y));
    case ONE:       return boole(!equal(x, y));

    case OLT: case OGT: case OLE: case OGE: case OADD: case OSUB:
    case OMUL: case ODIV: case OREM:

        if (!isint(x) || !isint(y))
            semantic(e, "NON_INTEGER %v %s %v", x, ops[e->op], y);

        switch (e->op) {
        case OLT:   return boole(x.i < y.i);
        case OGT:   return boole(x.i > y.i);
        case OLE:   return boole(x.i <= y.i);
        case OGE:   return boole(x.i >= y.i);
        case OADD:  return integer(x.i + y.i);
        case OSUB:  return integer(x.i - y.i);
        case OMUL:  return integer(x.i * y.i);
        case ODIV:  return integer(x.i / y.i);
        case OREM:  return integer(x.i % y.i);
        default:    return semantic(e, "UNEVALUATED_OP"), unit;
        }

    default:    return semantic(e, "UNEVALUATED_OP"), unit;
    }
}

value eval(ast *e, values *vars) {
    value   x, y, *ptr;

    top:
    switch (e->form) {
    case ELIT:      return e->x;

    case EID:       for (int i = e->index; i-- > 0 && vars; ) vars = vars->next;
                    if (!vars || e->index<0)
                        semantic(e, "MISTAKEN_INDEX %d", e->index);
                    return vars->x;

    case ETUPLE:    x = newtuple(e->n);
                    for (int i = 0; i < e->n; i++)
                        x.t->xs[i] = eval(e->es[i], vars);
                    return x;

    case ELIST:     x = nil;
                    ptr = &x;
                    for (int i = 0; i < e->n; i++) {
                        *ptr = cons(eval(e->es[i], vars), nil);
                        ptr = &ptr->l->tl;
                    }
                    return x;

    case EFN:       return fn(e->rhs, vars, e->info);

    case EAPP:      x = eval(e->lhs, vars);
                    if (x.type != FN) semantic(e, "NON_FUNCTION %v", x);
                    y = eval(e->rhs, vars);
                    vars = new(values, y, x.f->vars);
                    e = x.f->e;
                    goto top;

    case EBIN:      if (e->op == OSEQ) {
                        eval(e->lhs, vars);
                        e = e->rhs;
                        goto top;
                    }
                    x = eval(e->lhs, vars);
                    y = eval(e->rhs, vars);
                    return eval_op(e, x, y);

    case EUN:       return eval_op(e, eval(e->rhs, vars), unit);

    case EIF:       x = eval(e->cond, vars);
                    if (!isbool(x)) semantic(e, "NON_BOOL %v", x);
                    e = x.i? e->yes: e->no;
                    goto top;

    case ELET:      if (e->rec) {
                        values *bottom = vars;
                        for (struct rule *i = e->defs; i; i = i->next)
                            vars = new(values, eval(i->rhs, vars), vars);
                        for (values *i = vars; i != bottom; i = i->next)
                            i->x.f->vars = vars;
                        e = e->in;
                        goto top;
                    } else {
                        // All lets are single-variable now.
                        x = eval(e->defs->rhs, vars);
                        vars = new(values, x, vars);
                        e = e->in;
                        goto top;
                    }

    case ECRASH:    x = eval(e->offender, vars);
                    semantic(e->ctx, "NO_MATCH: %v", x);
                    return unit;

    case ECASE:     // does not exist
    default:        return semantic(e, "UNEVALUATED (%d)", e->form), unit;
    }
}

int main(int argc, char **argv) {
    setvbuf(stdout, 0, _IONBF, 0);

    for (char **i = tokens; *i; i++) *i = intern(*i, -1)->s;
    for (char **i = ops; *i; i++) *i = intern(*i, -1)->s;

    Pos no = {"(builtin)", 0};
    char *p1 = intern("x", -1)->s;
    char *p2 = intern("y", -1)->s;
    for (int i = 0; i < OBINARY; i++) {
        char *id = intern(ops[i], -1)->s;
        struct finfo *info = new(struct finfo, .id=id, .got=0);
        opvals[i] = fn(unary(i, var(no, p1, 0)), 0, info);
    }
    for (int i = OBINARY + 1; ops[i]; i++) {
        char *id = intern(ops[i], -1)->s;
        struct finfo *i1 = new(struct finfo, .id=id, .got=0);
        struct finfo *i2 = new(struct finfo, .id=id, .got=1);
        ast *body = binary(var(no, p1, 1), i, var(no, p2, 0));
        ast *inner = ast(EFN, no, .lhs=var(no, p1, -1), .rhs=body, .info=i2);
        opvals[i] = fn(inner, 0, i1);
    }

    addinfix(9, 9, (char*[]){".",0});
    addinfix(7, 8, (char*[]){"*","/","rem",0});
    addinfix(6, 7, (char*[]){"+","-",0});
    addinfix(6, 6, (char*[]){"^",0});
    addinfix(5, 6, (char*[]){"@",0});
    addinfix(5, 5, (char*[]){":",0});
    addinfix(4, 5, (char*[]){"==","<>","<",">","<=",">=",0});
    addinfix(3, 3, (char*[]){"&&",0});
    addinfix(2, 2, (char*[]){"||",0});
    addinfix(1, 1, (char*[]){":=","$",0});
    addinfix(0, 0, (char*[]){";",0});

    for (argv++; *argv; argv++) {
        nparts = 0;

        opensrc("prelude.ml");
        readscript();

        opensrc(*argv);
        readscript();

        ast *e = lit(no, unit);
        for (struct part *p = parts + nparts; p-- > parts; )
            e = ast(ELET, p->pos, .rec=p->rec, .defs=p->defs, .in=e);

        // pr("# %e\n", e);
        e = xform(e, 0);
        // pr("# %e\n", e);
        eval(e, 0);
        pr("done.\n");
    }
}
