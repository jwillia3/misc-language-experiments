enum { CODE_SIZE=64*1024, STACK_SIZE=256*1024 };

#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define new(t,...) memcpy(malloc(sizeof(t)), &(t){__VA_ARGS__}, sizeof(t))
#define ast(f,pos,...) new(ast, f, pos, __VA_ARGS__)

typedef struct string { int len; char text[]; } string;
typedef struct pos { string *fn; int ln; } pos;
typedef struct ast ast;
typedef struct value value;

enum opcode {
    IHALT,ILOAD,IVAR,IFN,IRET,ILET,IREC,IDROP,IBRF,IBRA,IAND,
    IOR,IAPP,ITAIL,IPOP,IMUL,IDIV,IREM,IADD,ISUB,IAT,ICAT,INTH,
    ICONS,IEQU,INEQ,ILT,ILE,IGT,IGE,ISHD,IHD,ITL,IPR,IORD,ICHR,
    ISIZE,IRDF,IEXIT,
};
enum    op {OMUL,ODIV,OREM,OADD,OSUB,OAT,OCAT,ONTH,OCONS,OEQU,
            ONEQ,OLT,OLE,OGT,OGE,OAND,OOR,OSHD,OAPP,OSEQ};
struct infix { char *id; int lhs, rhs, opcode; } ops[] = {
    {"*",8,9,IMUL},{"/",8,9,IDIV},{"rem",8,9,IREM},
    {"+",7,8,IADD},{"-",7,8,ISUB},{"@",7,8,IAT},{"^",7,7,ICAT},
    {"nth",6,7,INTH},{":",6,6,ICONS},{"==",5,5,IEQU},
    {"<>",5,5,INEQ},{"<",5,5,ILT},{"<=",5,5,ILE},{">",5,5,IGT},
    {">=",5,5,IGE},{"&&",4,4,0},{"||",3,3,0},{"<-",2,3,ISHD},
    {"$",1,1,IAPP},{";",0,0,0},{0,0,0,0}};

struct value {
    enum {NIL, BOOLE, INT, STR, CONS, FN} type;
    union {
        int _int;
        string *str;
        struct cons *cons;
        struct fn *fn;
    };
};
typedef struct env {string *id; value *val; struct env *next;} env;
typedef struct values { value val; struct values *next; } values;
typedef struct dump { int pc; value *S; values *E; struct dump *D; } dump;
struct cons { value hd, tl; };
struct fn { int pc; struct values *env; string *id; };

typedef struct decl {string *id; ast *val;} decl;
struct ast {
    enum {ELIT,EVAR,EFN,ELET,EIF,EBIN} form;
    struct pos pos;
    union {
        value   lit;
        string *id;
        struct { string *id, *param; ast *body; } fn;
        struct { enum op op; ast *lhs, *rhs; } app;
        struct { bool rec; decl *decls; int n; ast *in; } let;
        struct { ast *a, *b, *c; } _if;
    };
};

enum tok { TEOF,TINT,TSTR,TLP,TRP,TLB,TRB,TCOMMA,TSEMI,TBACK,
           TID,TEQ,TLET,TREC,TAND,TIN,TWHERE,TFN,TARROW,TIF,
           TTHEN,TELSE,TTRUE,TFALSE, };
char    *toks[] = {"eof","int","string","(",")","[","]",",",";",
                   "`","id","=","let","rec","and","in","where",
                   "fn","->","if","then","else","true","false",
                   0};
char    *opchr="!$%&*+-./:<=>?@^|~";

char    source[65536];
char    *src;
pos     srcpos;
bool    peeked;
int     tokint;
char    tokbuf[sizeof source];
string  *tokstr;
int     token;
string  **interns;
int     ninterns;
pos     codepos[CODE_SIZE];
int     code[CODE_SIZE];
int     vpc;
int     pc;
value   stack[STACK_SIZE];
value   *S = stack-1;
values  *E;
dump    *D;
value   constants[65536];
int     nconstants;
value   nil = {NIL};
value   chars[256];

void *pr(char *msg, ...);
ast *expr();
decl *readdecls(int *nptr);
void compile(ast *e, env *env, bool tail);

string *mkstr(char *text, int n) {
    if (n<0) n = strlen(text);
    string *s = malloc(sizeof *s + n + 1);
    s->text[s->len = n] = 0;
    if (text) memcpy(s->text, text, n);
    return s;
}
string *intern(char *text, int n) {
    for (int i=0; i<ninterns; i++)
        if (interns[i]->len==n && !memcmp(interns[i]->text, text, n))
            return interns[i];
    interns = realloc(interns, ++ninterns * sizeof *interns);
    return interns[ninterns - 1] = mkstr(text, n);
}

value _int(int n) { return (value) {INT, ._int=n}; }
value boole(bool n) { return (value) {BOOLE, ._int=n}; }
value str(string *s) { return (value) {STR, .str=s}; }
value cons(value hd, value tl) {
    return (value) {CONS, .cons=new(struct cons, hd, tl)};
}
value fn(int pc, values *env, string *id) {
    return (value) {FN,.fn=new(struct fn, pc, env, id)};
}
bool equal(value a, value b) {
    if (a.type!=b.type) return false;
    switch (a.type) {
    case NIL:   return true;
    case BOOLE: return a._int==b._int;
    case INT:   return a._int==b._int;
    case STR:   return a.str->len==b.str->len &&
                        !memcmp(a.str->text, b.str->text, a.str->len);
    case CONS:  return equal(a.cons->hd, b.cons->hd) &&
                       equal(a.cons->tl, b.cons->tl);
    case FN:    return a.fn==b.fn;
    }
    return false;
}

void *printvalue(value x) {
    switch (x.type) {
    case NIL:   return pr("[]");
    case BOOLE: return pr(x._int? "true": "false");
    case INT:   return pr("%d", x._int);
    case STR:   return pr("%S", x.str);
    case CONS:  pr("[");
                for ( ; x.type==CONS; x=x.cons->tl)
                    pr("%v%s", x.cons->hd, x.cons->tl.type==CONS? ", ":"");
                return pr("]");
    case FN:    return pr(x.fn->id? "%S": "(fn)", x.fn->id);
    }
    return pr("VALUE_NOT_PRINTED");
}
bool needparen(ast *e) {
    switch (e->form) {
    case ELIT: case EVAR:               return false;
    case ELET: case EIF: case EBIN:     return true;
    case EFN: return needparen(e->fn.body);
    }
    return true;
}
void *printexpr(ast *e, bool paren) {
    if (paren && needparen(e)) return pr("(%e)", e);
    switch (e->form) {
    case ELIT:  return pr("%v", e->lit);
    case EVAR:  return pr("%S", e->id);
    case EFN:   return pr("fn %S->%E", e->fn.param, e->fn.body);
    case ELET:  pr(e->let.rec? "let rec": "let");
                for (int i=0; i<e->let.n; i++)
                    pr("%s%S=%e", i? " and ": " ", e->let.decls[i]);
                return pr(" in %e", e->let.in);
    case EIF:   return pr("if %e then %e else %e", e->_if.a, e->_if.b, e->_if.c);
    case EBIN:  return pr("%E %s %E", e->app.lhs, ops[e->app.op].id, e->app.rhs);
    }
    return 0;
}
void *vpr(char *msg, va_list ap) {
    for (char *s=msg; *s; s++)
        if (*s!='%') putchar(*s);
        else switch (*++s) {
        case 'c': putchar(va_arg(ap, int)); break;
        case 'd': printf("%d", va_arg(ap, int)); break;
        case 's': printf("%s", va_arg(ap, char *)); break;
        case 'S': { string *s = va_arg(ap, string *);
                    printf("%s", s? s->text: "(null)"); break;
                  }
        case 'e': printexpr(va_arg(ap, ast *), false); break;
        case 'E': printexpr(va_arg(ap, ast *), true); break;
        case 'v': printvalue(va_arg(ap, value)); break;
        case '*': { char *msg2 = va_arg(ap, char *);
                    vpr(msg2, *va_arg(ap, va_list *));
                    break;
                  }
        case '!': putchar('\n'); exit(1);
        }
    return 0;
}
void *pr(char *msg, ...) {
    va_list ap; va_start(ap, msg);
    vpr(msg, ap);
    va_end(ap);
    return 0;
}

void *syntax(char *msg, ...) {
    va_list ap; va_start(ap, msg);
    pr("boot: error %S:%d: %* before %s\n",
       srcpos.fn, srcpos.ln, msg, &ap, toks[token]);
    exit(1);
}
void opensrc(char *fname) {
    srcpos = (pos) {intern(fname, -1), 1};
    (src = source), (peeked = 0);
    FILE *file = fopen(fname, "rb");
    if (!file) syntax("cannot open source");
    source[fread(source, 1, sizeof source, file)] = 0;
    fclose(file);
}
int next() {
    char *t=tokbuf;

    if (peeked) return (peeked=false), token;
    while (*src=='#' || isspace(*src))
        if (*src=='\n') srcpos.ln++, src++;
        else if (isspace(*src)) src++;
        else if (*src=='#') while (*src && *src!='\n') src++;

    if (!*src) return (token=0);
    if (isdigit(src[*src=='-']))
        return (tokint=strtol(src, &src, 10)), (token=TINT);
    for (int i=TSTR+1; i<TID; i++)
        if (*src==*toks[i]) return (++src), (token=i);
    if (*src=='"')
        for (++src; ; src++)
            if (!*src || *src=='\n') syntax("unclosed string");
            else if (*src=='"')
                return src++, tokstr=intern(tokbuf, t-tokbuf), token=TSTR;
            else if (*src!='\\') *t++ = *src;
            else switch (*++src) {
            case 't': *t++ = '\t'; break;
            case 'n': *t++ = '\n'; break;
            default: *t++ = *src; break;
            }

    while (isalnum(*src) || *src=='_' || *src=='\'') *t++ = *src++;
    if (t==tokbuf) while (*src && strchr(opchr, *src)) *t++ = *src++;
    if (t==tokbuf) syntax("not a token: %c", *src);
    tokstr = intern(tokbuf, t-tokbuf);
    for (char **i=toks+TID+1; *i; i++)
        if (*i==tokstr->text) return (token=i-toks);
    return (token=TID);
}
bool peek(int t) { return (next(), peeked=true, t==token); }
bool want(int t) { return !(peeked=next()!=t); }
void need(int t) { if (!want(t)) syntax("need %s", toks[t]); }
void *suffix(const void *x, int t) { need(t); return (void*) x; }

struct infix *findop() {
    if (peek(TID))
        for (struct infix *i=ops; i->id; i++) if (i->id==tokstr->text) return i;
    return 0;
}
ast *fnexpr(string *id, int delim) {
    if (want(delim)) {
        ast *body = expr();
        if (delim == TEQ && want(TWHERE)) {
            pos     org=srcpos;
            int     n = 0;
            decl    *decls = readdecls(&n);
            return ast(ELET, org, .let={true, decls, n, body});
        }
        return body;
    }
    pos org = srcpos;
    string  *param = (need(TID), tokstr);
    ast     *body = fnexpr(id, delim);
    return ast(EFN, org, .fn={id, param, body});
}
decl *readdecls(int *nptr) {
    struct decl *decls = 0;
    *nptr = 0;
    want(TAND);
    do {
        pos org = srcpos;
        if (want(TLB)) {
            string  **ids = 0;
            int     nids = 0;
            do {
                if (peek(TRB)) break;
                ids = realloc(ids, ++nids * sizeof *ids);
                ids[nids - 1] = (need(TID), tokstr);
            } while (want(TCOMMA));
            need(TRB), need(TEQ);

            ast     *value = expr();
            string  *it = intern("__", -1);

            decls = realloc(decls, (*nptr + nids + 1) * sizeof *decls);
            decls[(*nptr)++] = (struct decl) {it, value};
            for (int i=0; i<nids; i++) {
                ast *f = ast(EVAR, org, .id=it);
                ast *x = ast(ELIT, org, .lit=_int(i));
                ast *value = ast(EBIN, org, .app={ONTH, f, x});
                decls[(*nptr)++] = (struct decl) {ids[i], value};
            }
        } else {
            string *id = (need(TID), tokstr);
            ast    *value = fnexpr(id, TEQ);
            decls = realloc(decls, ++(*nptr) * sizeof *decls);
            decls[*nptr - 1] = (struct decl) {id, value};
        }
    } while (want(TAND));
    return decls;
}
ast *aexpr(bool required) {
    pos org = srcpos;
    if (!required && peek(TID) && findop()) return 0;
    if (want(TINT)) return ast(ELIT, org, .lit=_int(tokint));
    if (want(TSTR)) return ast(ELIT, org, .lit=str(tokstr));
    if (want(TTRUE)) return ast(ELIT, org, .lit=boole(1));
    if (want(TFALSE)) return ast(ELIT, org, .lit=boole(0));
    if (want(TID))  return ast(EVAR, org, .id=tokstr);
    if (want(TLP))  return want(TRP)? ast(ELIT, org, .lit=nil): suffix(expr(), TRP);
    if (want(TLB)) {
        ast *list = 0;
        ast **p = &list;
        do {
            if (peek(TRB)) break;
            ast *hd = expr();
            *p = ast(EBIN, hd->pos, .app={OCONS, hd, 0});
            p = &(*p)->app.rhs;
        } while (want(TCOMMA));
        *p = ast(ELIT, org, .lit=nil);
        need(TRB);
        return list;
    }
    if (want(TFN)) return fnexpr(0, TARROW);
    return required? syntax("need expression"): NULL;
}
ast *iexpr(int level) {
    if (level > 10) {
        pos org = srcpos;
        if (want(TIF)) {
            ast *a = suffix(expr(), TTHEN);
            ast *b = suffix(expr(), TELSE);
            ast *c = expr();
            return ast(EIF, ._if={a,b,c});
        }
        if (want(TLET)) {
            bool    rec = want(TREC);
            int     n = 0;
            decl    *decls = readdecls(&n);
            ast     *in = (need(TIN), expr());
            return ast(ELET, org, .let={rec, decls, n, in});
        }
        ast *rhs, *lhs = aexpr(true);
        while ((rhs=aexpr(false)))
            lhs = ast(EBIN, lhs->pos, .app={OAPP,lhs,rhs});
        return lhs;
    }
    ast *lhs = iexpr(level + 1);
    struct infix *op;
    while (((op=peek(TSEMI)? ops+OSEQ: findop()) && level==op->lhs) ||
           (level == 9 && peek(TBACK)))
    {
        next();
        if (token == TBACK) {
            pos org = srcpos;
            ast *id = ast(EVAR, org, .id=(need(TID), tokstr));
            ast *rhs = iexpr(8);
            lhs = ast(EBIN, lhs->pos, .app={OAPP, id, lhs});
            lhs = ast(EBIN, lhs->pos, .app={OAPP, lhs, rhs});
        } else {
            ast *rhs = iexpr(op->rhs);
            lhs = ast(EBIN, lhs->pos, .app={op-ops, lhs, rhs});
        }
    }
    return lhs;
}
ast *expr() { return iexpr(0); }

void *semantic(ast *e, char *msg, ...) {
    va_list ap; va_start(ap, msg);
    pr("boot: error %S:%d: %*\n", e->pos.fn, e->pos.ln, msg, &ap);
    exit(1);
}

int emit(int n) { code[vpc] = n; return vpc++; }
int emit2(int n, int m) { emit(n); return emit(m); }
void patch(int at) { code[at] = vpc; }
int constant(value x) {
    for (int i=0; i<nconstants; i++)
        if (equal(constants[i], x)) return i;
    constants[nconstants] = x;
    return nconstants++;
}

int find(string *id, env *env, value **valp) {
    int index = 0;
    *valp = 0;
    for ( ; env; env=env->next)
        if (env->val) if (env->id==id) return (*valp=env->val), -1; else {}
        else if (env->id==id) return index; else index++;
    return -1;
}

void _compile(ast *e, env *env, bool tail) {
    int     br, end, index;
    value   *valp;
    switch (e->form) {
    case ELIT:      emit2(ILOAD, constant(e->lit)); break;
    case EVAR:      index = find(e->id, env, &valp);
                    if (valp) emit2(ILOAD, constant(*valp));
                    else if (index < 0) semantic(e, "undefined: %S", e->id);
                    else emit2(IVAR, index);
                    return;
    case EFN:       br = emit2(IFN, 0);
                    emit(constant(str(e->fn.id? e->fn.id: intern("???",-1))));
                    compile(e->fn.body, new(struct env, e->fn.param, 0, env), true);
                    emit(IRET);
                    patch(br);
                    return;
    case ELET:      if (e->let.rec) {
                        for (int i=0; i<e->let.n; i++)
                            env = new(struct env, e->let.decls[i].id, 0, env);
                        for (int i=0; i<e->let.n; i++) {
                            if (e->let.decls[i].val->form != EFN)
                                semantic(e->let.decls[i].val, "must be fn");
                            compile(e->let.decls[i].val, env, false);
                            emit(ILET);
                        }
                        emit2(IREC, e->let.n);
                        compile(e->let.in, env, tail);
                        emit2(IDROP, e->let.n);
                    } else {
                        for (int i=0; i<e->let.n; i++) {
                            compile(e->let.decls[i].val, env, false);
                            env = new(struct env, e->let.decls[i].id, 0, env);
                            emit(ILET);
                        }
                        compile(e->let.in, env, tail);
                        emit2(IDROP, e->let.n);
                    }
                    return;
    case EIF:       compile(e->_if.a, env, false);
                    br = emit2(IBRF, 0);
                    compile(e->_if.b, env, tail);
                    end = emit2(IBRA, 0);
                    patch(br);
                    compile(e->_if.c, env, tail);
                    patch(end);
                    return;
    case EBIN:      if (e->app.op==OSEQ) {
                        compile(e->app.lhs, env, false);
                        emit(IPOP);
                        compile(e->app.rhs, env, tail);
                    } else if (e->app.op==OAND || e->app.op==OOR) {
                        compile(e->app.lhs, env, false);
                        br = emit2(e->app.op==OAND? IAND: IOR, 0);
                        compile(e->app.rhs, env, tail);
                        patch(br);
                    } else {
                        if (e->app.op==OAPP && e->app.lhs->form==EVAR) {
                            char *id = e->app.lhs->id->text;
                            int opcode = -1;
                            if (!strcmp(id, "hd")) opcode = IHD;
                            else if (!strcmp(id, "tl")) opcode = ITL;
                            else if (!strcmp(id, "print")) opcode = IPR;
                            else if (!strcmp(id, "ord")) opcode = IORD;
                            else if (!strcmp(id, "chr")) opcode = ICHR;
                            else if (!strcmp(id, "size")) opcode = ISIZE;
                            else if (!strcmp(id, "readfile")) opcode = IRDF;
                            else if (!strcmp(id, "exit")) opcode = IEXIT;

                            if (opcode >= 0) {
                                compile(e->app.rhs, env, false);
                                emit(opcode);
                                return;
                            }
                        }
                        compile(e->app.lhs, env, false);
                        compile(e->app.rhs, env, false);
                        if (e->app.op==OAPP && tail)
                            emit(ITAIL);
                        else
                            emit(ops[e->app.op].opcode);
                    }
                    return;
    }
}
void compile(ast *e, env *env, bool tail) {
    int pc = vpc;
    _compile(e, env, tail);
    // Mark all code generated since the marked pc as part of
    // this expression unless it already has a position.
    for ( ; pc<vpc; pc++)
        if (codepos[pc].ln == 0) codepos[pc] = e->pos;
}

void xch(int n, value x) { *(S -= n - 1) = x; }
void *fault(char *msg, ...) {
    va_list ap; va_start(ap, msg);
    pos pos = codepos[pc].ln? codepos[pc]: (struct pos) {intern("unknown",-1), 0};
    pr("boot: error %S:%d: %*\n", pos.fn, pos.ln, msg, &ap);
    for (dump *d = D; d; d=d->D)
        pr("> %S:%d\n", codepos[d->pc].fn, codepos[d->pc].ln);
    exit(1);
}
value ck(int i, int type, char *what) {
    if (S[-i].type!=type) fault("needed %s: %v", what, S[-i]);
    return S[-i];
}
int cbool(int i) { return ck(i, BOOLE, "bool")._int; }
int cint(int i) { return ck(i, INT, "int")._int; }
string *cstr(int i)  { return ck(i, STR, "string").str; }
struct cons *ccons(int i)  { return ck(i, CONS, "list").cons; }

void eval() {
    values *e;
    int     i, n;
    value   x, f;

    while (true) {
    switch (code[pc++]) {
    case IHALT: return;
    case IEXIT: exit(cint(0));
    case ILOAD: xch(0, constants[code[pc++]]); break;
    case IVAR:  for (e=E, n=code[pc++]; n; n--) e=e->next;
                xch(0, e->val);
                break;
    case IFN:   xch(0, fn(pc+2, E, constants[code[pc+1]].str)); pc=code[pc]; break;
    case IRET:  (x=*S), (pc=D->pc), (S=D->S), (E=D->E), (D=D->D);
                xch(0, x); break;
    case ILET:  E = new(values, *S--, E); break;
    case IREC:  for (e=E, n=code[pc++]; n; n--, e=e->next)
                    e->val.fn->env = E;
                break;
    case IDROP: for (n=code[pc++]; n; n--) E=E->next; break;
    case IBRF:  if (!cbool(0)) pc=code[pc]; else pc++; S--; break;
    case IBRA:  pc=code[pc]; break;
    case IAND:  if (!cbool(0)) pc=code[pc]; else pc++, S--; break;
    case IOR:   if (cbool(0)) pc=code[pc]; else pc++, S--; break;
    case IPOP:  S--; break;
    case IAPP:  (x=*S--), (f=*S--);
                if (f.type!=FN) fault("calling non-fn: %v", f);
                D=new(dump, pc, S, E, D);
                (pc=f.fn->pc), (E=new(values, x, f.fn->env));
                break;
    case ITAIL: (x=*S--), (f=*S--);
                if (f.type!=FN) fault("calling non-fn: %v", f);
                (pc=f.fn->pc), (E=new(values, x, f.fn->env));
                break;
    case INTH:  x = S[-1];
                for (n = cint(0); n; n--)
                    if (x.type==CONS) x = x.cons->tl;
                if (x.type!=CONS) fault("nth: non-list: %v", x);
                xch(2, x.cons->hd);
                break;
    case ICONS: if (S->type==NIL || ccons(0)) xch(2, cons(S[-1], *S)); break;
    case IMUL:  xch(2, _int(cint(1)*cint(0))); break;
    case IDIV:  xch(2, _int(cint(1)/cint(0))); break;
    case IREM:  xch(2, _int(cint(1)%cint(0))); break;
    case IADD:  xch(2, _int(cint(1)+cint(0))); break;
    case ISUB:  xch(2, _int(cint(1)-cint(0))); break;
    case ILT:   xch(2, boole(cint(1)<cint(0))); break;
    case ILE:   xch(2, boole(cint(1)<=cint(0))); break;
    case IGT:   xch(2, boole(cint(1)>cint(0))); break;
    case IGE:   xch(2, boole(cint(1)>=cint(0))); break;
    case IEQU:  xch(2, boole(equal(S[-1], *S))); break;
    case INEQ:  xch(2, boole(!equal(S[-1], *S))); break;
    case IAT:   n=cstr(1)->len;
                i=cint(0);
                if (i<-n || i>n) fault("out of bounds: %d", i);
                if (i<0) i += n;
                xch(2, chars[cstr(1)->text[i] & 255]);
                break;
    case ICAT:  {   string *a = cstr(1), *b = cstr(0);
                    string *s = mkstr(0, a->len + b->len);
                    memcpy(s->text, a->text, a->len);
                    memcpy(s->text + a->len, b->text, b->len);
                    xch(2, str(s));
                    break;
                }
    case ISHD:  ccons(1)->hd = *S; xch(2, *S); break;
    case IHD:   xch(1, ccons(0)->hd); break;
    case ITL:   xch(1, ccons(0)->tl); break;
    case IPR:   printvalue(*S); break;
    case IORD:  xch(1, _int(cstr(0)->text[0] & 255)); break;
    case ICHR:  xch(1, chars[cint(0) & 255]); break;
    case ISIZE: xch(1, _int(cstr(0)->len)); break;
    case IRDF:  {   FILE *file = fopen(cstr(0)->text, "rb");
                    if (!file) { xch(1, str(mkstr(0, 0))); continue; }
                    fseek(file, 0, SEEK_END);
                    n = ftell(file);
                    rewind(file);
                    string *s = mkstr(0, n);
                    fread(s->text, 1, n, file);
                    fclose(file);
                    xch(1, str(s));
                    break;
                }
    }
    }
}

env *top(env *env) {
    while (!peek(TEOF)) {
        need(TLET);
        bool    rec = want(TREC);
        string  *id = (need(TID), tokstr);
        ast     *rhs = fnexpr(id, TEQ);
        if (rec) {
            if (rhs->form != EFN) semantic(rhs, "must be fn");
            // This predicts where the code will be compiled.
            // If _compile() or IFN changes, this must as well.
            value *dummy = new(value[1], fn(vpc+3, 0, id));
            env = new(struct env, id, dummy, env);
        }
        // pr("# let%s %S = %e\n", rec?" rec":"", id, rhs);
        compile(rhs, env, false);
        codepos[vpc] = rhs->pos;
        emit(IHALT);
        eval();
        if (!rec) env = new(struct env, id, new(value[1], *S), env);
        S--;
    }
    return env;
}

int main(int argc, char **argv) {
    setvbuf(stdout, 0, _IONBF, 0);

    for (char **i=toks; *i; i++) *i = intern(*i, -1)->text;
    for (int i=0; ops[i].id; i++) ops[i].id = intern(ops[i].id, -1)->text;
    for (int i=0; i<256; i++) chars[i] = str(intern((char[]){i}, 1));

    for (argv++; *argv; argv++) {
        env *env = 0;
        opensrc("prelude.ml");
        env = top(env);
        opensrc(*argv);
        env = top(env);
    }
    puts("done.");
}
