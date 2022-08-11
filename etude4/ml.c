#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define new(t,...) memcpy(malloc(sizeof(t)), &(t){__VA_ARGS__}, sizeof(t))
#define ast(f,...) new(struct ast, f, srcpos, __VA_ARGS__)

typedef struct string { int n; char txt[]; } string;
typedef struct pos { string *fn; int ln; } pos;

typedef enum tok {
    TEND, TINT, TCHR, TSTR, TCID, TLP, TRP, TLB, TRB, TCOMMA,
    TID, TEQ, TARROW, TBAR, TIF, TTHEN, TELSE, TCASE, TFN, TAND,
    TLET, TREC, TIN, TDATA, TINL, TINR,
} tok;
char    *toks[] = {"end","int","char","string","CID","(",")",
        "[","]",",","ID","=","->","|","if","then","else","case",
        "fn","and","let","rec","in","datatype","infixl","infixr", 0};

typedef struct list {
    union {
        struct type *type;
        struct ast  *ast;
        struct ctor *ctor;
        struct op   *op;
        struct symbol *sym;
        void        *ptr;
    };
    struct list *next;
} list;

typedef struct type {
    enum { TYPE, TYPEVAR, TUPTYPE, FNTYPE } form;
    string *id;
    struct type *inst;
    list *args;
} type;

typedef struct ctor { string *id; type *type; } ctor;
typedef struct op { string *id; int lhs, rhs; } op;

typedef struct symbol {
    string  *id;
    type    *type;
    int     opcode;
} symbol;

typedef struct typesub {
    type *from;
    type *to;
    struct typesub *next;
} typesub;

typedef struct ast {
    enum { EINT, ECHR, ESTR, EID, ECID, ETUP, ELIST, EAPP,
           ELET, EREC, EFN, ECASE, EIF, ERULE, EFAIL,
    } form;
    pos     pos;
    int     n;
    string  *str;
    struct ast *lhs, *rhs, *next;
    type    *type;
    symbol  *symbol;
    ctor    *ctor;
} ast;

char    source[65536], *src;
pos     srcpos;
string  **interns;
int     ninterns;
bool    peeked;
tok     token;
int     tint;
string  *tstr;
list    *ops;
ast     *dummy;
string  *tup_id, *list_id, *cons_id, *eq_id;
type    *int_type, *chr_type, *str_type, *bool_type;
list    *all_types;
list    *all_ctors;
list    *nongeneric;
int     ocol = 1;
int     *indent = (int[65536]) {0};

void *pr(char *msg, ...);
ast *_exp(void);
type *ty(void);
type *typecheck(ast *e, symbol *env);

string *mkstr(char *txt, int n) {
    if (n < 0) n = strlen(txt);
    string *str = malloc(sizeof *str + n + 1);
    str->n = n;
    memcpy(str->txt, txt, n);
    str->txt[n] = 0;
    return str;
}

string *intern(char *txt, int n) {
    if (n < 0) n = strlen(txt);
    for (int i = 0; i < ninterns; i++)
        if (interns[i]->n == n && !memcmp(interns[i]->txt, txt, n))
            return interns[i];
    interns = realloc(interns, ++ninterns * sizeof *interns);
    return interns[ninterns - 1] = mkstr(txt, n);
}

list *cons(void *hd, list *tl) { return new(list, .ptr=hd, .next=tl); }
type *typevar(string *id) { return new(type, TYPEVAR, .id=id); }
type *basetype(string *id, list *x) {return new(type, TYPE, .id=id, .args=x); }
type *tuptype(list *x) { return new(type, TUPTYPE, .args=x); }
type *listtype(type *t) { return basetype(list_id, cons(t, 0)); }
type *fntype(type *f, type *x) {
    list *args = cons(f, cons(x, 0));
    return new(type, FNTYPE, .args=args);
}

bool isvar(type *t) { return t->form == TYPEVAR; }
type *prune(type *t) { return t->inst? (t->inst = prune(t->inst)): t; }
type *renamed(type *t, int *uid) {
    t = prune(t);
    if (isvar(t) && !t->id)
        t->id = mkstr((char[]){'$', 'a' + (*uid)++}, 2);
    for (list *i = t->args; i; i = i->next)
        renamed(i->type, uid);
    return t;
}
type *findtype(string *id) {
    for (list *i = all_types; i; i = i->next)
        if (i->type->id == id) return i->type;
    return 0;
}
ctor *findctor(string *id) {
    for (list *i = all_ctors; i; i = i->next)
        if (i->ctor->id == id) return i->ctor;
    return 0;
}

void *prty(type *t, bool paren) {
    t = prune(t);
    switch (t->form) {
    case TYPE:      if (t->args && t->args->next) {
                        pr("(");
                        for (list *i = t->args; i; i = i->next)
                            pr("%T%s", i->type, i->next? ", ": "");
                        pr(") ");
                    } else if (t->args)
                        pr("%T ", t->args->type);
                    return pr("%S", t->id);
    case TYPEVAR:   return pr("%S", t->id);
    case FNTYPE:    return pr(paren? "(%T -> %t)": "%T -> %t",
                              t->args->type,
                              t->args->next->type);
    case TUPTYPE:   if (!t->args) return pr("()");
                    for (list *i = t->args; i; i = i->next)
                        pr("%T%s", i->type, i->next? "*": "");
                    return 0;
    }
    return 0;
}

void *prexp(ast *e) {
    switch (e->form) {
    case EINT:  return pr("%d", e->n);
    case ECHR:  return pr("'%c'", e->n);
    case ESTR:  return pr("\"%S\"", e->str);
    case EID:   return pr("%S", e->str);
    case ECID:  return pr("%S", e->str);
    case ETUP:  pr("(");
                for (ast *i = e->lhs; i; i = i->next)
                    pr("%e%s", i, i->next? ", ": "");
                return pr(")");
    case ELIST: pr("[");
                for (ast *i = e->lhs; i; i = i->next)
                    pr("%e%s", i, i->next? ", ": "");
                return pr("]");
    case EAPP:  return pr("(%e %e)", e->lhs, e->rhs);
    case ELET:
    case EREC:  return pr(e->rhs->form == ELET || e->rhs->form == EREC?
                          "%[%s %e in\n%]%e": "%[%s %e\nin%] %e",
                          e->form == EREC? "let rec": "let",
                          e->lhs,
                          e->rhs);
    case ERULE: pr("%e = %e", e->lhs, e->rhs);
                if (e->rhs->type) pr(" :: %t", e->rhs->type);
                if (e->next) pr ("\nand %e", e->next);
                return 0;
    case ECASE: pr("%[case %e", e->lhs);
                for (ast *i = e->rhs; i; i = i->next)
                    pr("\n| %e -> %e", i->lhs, i->rhs);
                return pr("%]");
    case EIF:   return pr("%[if %e\nthen %[%e%]\nelse %[%e%]%]",
                          e->lhs, e->rhs->lhs, e->rhs->rhs);
    case EFN:   pr("%[fn");
                for (ast *i = e->lhs; i; i = i->next) {
                    for (ast *j = i->lhs; j; j = j->next)
                        pr(" %e", j);
                    pr(" -> %e", i->rhs);
                    if (i->next) pr("\n| ");
                }
                return pr("%]");
    case EFAIL: return pr("(FAIL)");
    }
    return 0;
}

void vpr(char *msg, va_list ap) {
    char *msg2;
    string *str;
    int uid = 0;
    for (char *s = msg; *s; s++)
        if (*s == '\n') {
            putchar('\n');
            for (int i = 0; i < *indent - 1; i++) putchar(' ');
            ocol = *indent;
        }
        else if (*s != '%')
            ocol++, putchar(*s);
        else switch (*++s) {
        case 'c': ocol++; putchar(va_arg(ap, int)); break;
        case 'd': ocol += printf("%d", va_arg(ap, int)); break;
        case 's': ocol += printf("%s", va_arg(ap, char *)); break;
        case 'S': str = va_arg(ap, string *);
                  ocol += fwrite(str->txt, 1, str->n, stdout);
                  break;
        case 'e': prexp(va_arg(ap, ast *)); break;
        case 't': prty(renamed(va_arg(ap, type *), &uid), false); break;
        case 'T': prty(renamed(va_arg(ap, type *), &uid), true); break;
        case '[': *++indent = ocol; break;
        case ']': indent--; break;
        case '*':
            msg2 = va_arg(ap, char *);
            vpr(msg2, *va_arg(ap, va_list *));
            break;
        }
}

void *pr(char *msg, ...) {
    va_list ap; va_start(ap, msg);
    vpr(msg, ap);
    va_end(ap);
    return 0;
}

void *syntax(char *msg, ...) {
    va_list ap; va_start(ap, msg);
    pr("error %s:%d: %*\n", srcpos.fn->txt, srcpos.ln, msg, &ap);
    exit(1);
}

void setsrc(char *filename, char *text) {
    srcpos = (pos) { .fn=intern(filename, -1), .ln=1 };
    peeked = false;
    src = source;
    strcpy(source, text);
}

void opensrc(char *filename) {
    setsrc(filename, "");
    FILE *file = fopen(filename, "rb");
    if (!file) syntax("cannot open");
    source[fread(source, 1, sizeof source, file)] = 0;
    fclose(file);
}

int character(void) {
    if (*src != '\\') return *src++;
    switch ((src += 2)[-1]) {
    case 'n': return '\n';
    case 't': return '\t';
    default: return src[-1];
    }
}

tok next(void) {
    if (peeked)
        return peeked = false, token;
    while (true)
        if (*src == '\n') srcpos.ln++, src++;
        else if (isspace(*src)) src++;
        else if (*src == '#') while (*src && *src != '\n') src++;
        else break;
    if (*src == 0) return token = TEND;
    if (isdigit(src[*src == '-']))
        return tint = strtol(src, &src, 10), token = TINT;
    for (int i = TCID + 1; i < TID; i++)
        if (*src == *toks[i]) return src++, token = i;
    if (*src == '\'') {
        if (*++src == '\'') syntax("char cannot be empty");
        tint = character();
        if (*src++ != '\'') syntax("unclosed char");
        return token = TCHR;
    }
    if (*src == '"') {
        for (char *i = src++, *j = i; ;)
            if (!*src || *src == '\n') syntax("unclosed string");
            else if (*src == '"')
                return tstr = intern(i, j - i), src++, token = TSTR;
            else
                *j++ = character();
    }

    char *org = src;
    while (isalnum(*src) || *src == '_' || *src == '\'') src++;
    if (org == src) while (*src && strchr("$%&*+-./:<=>?@^|~", *src)) src++;
    if (org == src) syntax("not a token: %c", *src);
    tstr = intern(org, src - org);
    for (int i = TID + 1; toks[i]; i++)
        if (toks[i] == tstr->txt) return token = i;
    return token = isupper(*org)? TCID: TID;
}

bool peek(tok t) { peeked = next(); return token == t; }
bool want(tok t) { peeked = next() != t; return !peeked; }
void need(tok t) { if (!want(t)) syntax("need %s", toks[t]); }
bool peek_id(string *id) { return peek(TID) && tstr == id; }
bool want_id(string *id) { return peek_id(id) && want(TID); }
bool need_id(string *id) { if (!want_id(id)) syntax("need %S", id); return 1; }
void *suffix(void *x, tok t) { need(t); return x; }

string *uid(void) {
    static int counter;
    char buf[16];
    return intern(buf, sprintf(buf, "$%d", ++counter));
}
ast *tree(int form, ast *lhs, ast *rhs) {
    return ast(form, .pos=lhs? lhs->pos: srcpos, .lhs=lhs, .rhs=rhs);
}
int count_asts(ast *list) { return list? 1 + count_asts(list->next): 0; }
ast **flat_list(ast *src, ast ***listp) {
    ast **f = *listp = malloc(count_asts(src) * sizeof **listp);
    for (ast *i = src; i; i = i->next) *f++ = i;
    return f;
}
void append(ast **listp, ast *x) {
    ast **ptr = listp;
    while (*ptr) ptr = &(*ptr)->next;
    *ptr = x;
}
void push(list **listp, void *x) {
    list **ptr = listp;
    while (*ptr) ptr = &(*ptr)->next;
    *ptr = cons(x, 0);
}
void append_to_let(ast **ptr, ast *x) {
    if (*ptr && ((*ptr)->form == ELET || (*ptr)->form == EREC))
        return append_to_let(&(*ptr)->rhs, x);
    *ptr = x;
}

op *findop(int lvl) {
    if (peek(TID))
        for (list *i = ops; i; i = i->next)
            if (i->op->id == tstr && (lvl < 0 || i->op->lhs == lvl))
                return i->op;
    return 0;
}

ast *atexp(bool required) {
    if (!required && findop(-1)) return 0; // Avoid operator as arg
    if (want(TINT)) return ast(EINT, .n=tint);
    if (want(TCHR)) return ast(ECHR, .n=tint);
    if (want(TSTR)) return ast(ESTR, .str=tstr);
    if (want(TID))  return ast(EID, .str=tstr);
    if (want(TCID)) return ast(ECID, .str=tstr);
    if (want(TLP) || want(TLB)) {
        bool istup = token == TLP;
        tok  end = istup? TRP: TRB;
        ast  *list = 0;
        do {
            if (peek(end)) break;
            append(&list, _exp());
        } while (want(TCOMMA));
        need(end);
        return tree(istup? ETUP: ELIST, list, 0);
    }
    return required? syntax("need expression"): 0;
}

ast *infexp(int lvl) {
    if (lvl == 11) {
        ast *lhs = atexp(true), *rhs;
        while ((rhs = atexp(false)))
            lhs = tree(EAPP, lhs, rhs);
        return lhs;
    }
    ast *lhs = infexp(lvl + 1);
    for (op *o; (o = findop(lvl)); ) {
        next();
        ast *rhs = infexp(o->rhs);
        lhs = tree(EAPP, ast(EID, .str=o->id), lhs);
        lhs = tree(EAPP, lhs, rhs);
    }
    return lhs;
}

ast *fnexp(string *id, tok delim) {
    if (want(delim))
        return _exp();
    ast *rules = 0;
    do {
        ast *params = 0;
        while (!want(delim))
            append(&params, atexp(true));
        ast *rhs = _exp();
        append(&rules, tree(ERULE, params, rhs));
    } while (want(TBAR) && (!id || need_id(id)));
    return tree(EFN, rules, 0);
}

ast *declist(void) {
    ast *list = 0;
    do {
        ast *lhs = atexp(true);
        ast *rhs = lhs->form == EID
                    ? fnexp(lhs->str, TEQ)
                    : (need(TEQ), _exp());
        append(&list, tree(ERULE, lhs, rhs));
    } while (want(TAND));
    return list;
}

ast *_exp(void) {
    if (want(TIF)) {
        ast *a = suffix(_exp(), TTHEN);
        ast *b = suffix(_exp(), TELSE);
        ast *c = _exp();
        return tree(EIF, a, tree(ERULE, b, c));
    }
    else if (want(TCASE)) {
        ast *sub = _exp();
        ast *rules = 0;
        while (want(TBAR)) {
            ast *lhs = suffix(_exp(), TARROW);
            ast *rhs = _exp();
            append(&rules, tree(ERULE, lhs, rhs));
        }
        return tree(ECASE, sub, rules);
    }
    else if (want(TLET)) {
        bool rec = want(TREC);
        ast *decs = (want(TAND), declist());
        need(TIN);
        ast *body = _exp();
        return tree(rec? EREC: ELET, decs, body);
    }
    else if (want(TFN))
        return fnexp(0, TARROW);
    return infexp(0);
}

type *atty(void) {
    type *t, *u;
    list *args = 0;
    int  n = 1;

    if (want(TID)) {                                // Base type
        t = findtype(tstr);
        if (!t) syntax("undefined type: %S", tstr);
        if (t->args) syntax("type needs args: %t", t);
    }
    else if (want(TLP)) {                      // Type arguments
        n = 0;
        do {
            if (peek(TRP)) break;
            push(&args, ty());
            n++;
        } while (want(TCOMMA));
        need(TRP);

        if (n == 1) { t = args->type; args = 0; }    // Not args
    }
    else
        syntax("need type");

    while (!peek_id(tup_id) && want(TID)) {        // Type ctors
        u = findtype(tstr);
        if (!u) syntax("undefined type: %S", tstr);

        int m = 0;
        for (list *i = u->args; i; i = i->next) m++;
        if (n != m) syntax("wrong number of args: %t", u);

        t = basetype(u->id, args? args: cons(t, 0));
        n = 1;
        args = 0;
    }

    if (args) syntax("type args without constructor"); // () without id

    return t;
}

type *ty(void) {
    type *lhs = atty();
    if (peek_id(tup_id)) {
        list *args = cons(lhs, 0);
        while (want_id(tup_id))
            push(&args, atty());
        lhs = tuptype(args);
    }

    if (want(TARROW))
        return fntype(lhs, ty());
    return lhs;
}

void top_level(ast **letsp) {
    while (!peek(TEND))
        if (want(TINL) || want(TINR)) {     // Infix declaration
            int adjust = token == TINL? 1: 0;
            int lhs = (need(TINT), tint);
            while (want(TID))
                ops = cons(new(op, tstr, lhs, lhs + adjust), ops);
        }
        else if (want(TDATA)) {          // Datatype declaration
            list    *args = 0;
            if (want(TLP)) {
                do {
                    if (peek(TRP)) break;
                    need(TID);
                    push(&args, typevar(tstr));
                } while (want(TCOMMA));
                need(TRP);
            }

            string  *id = suffix((need(TID), tstr), TEQ);
            type    *datatype = basetype(id, args);

            if (findtype(id)) syntax("type already defined: %S", id);

            // Define datatype and its type parameters
            all_types = cons(datatype, all_types);
            for (list *i = args; i; i = i->next)
                all_types = cons(i->type, all_types);

            do {
                string  *id = (need(TCID), tstr);
                type    *arg = peek(TLP) || peek(TID)? ty(): 0;
                type    *t = arg? fntype(arg, datatype): datatype;
                if (findctor(id)) syntax("already defined: %S", id);
                all_ctors = cons(new(ctor, .id=id, .type=t), all_ctors);
            } while (want(TBAR));

            for (list *i = args; i; i = i->next) // Drop type params
                all_types = all_types->next;
        }
        else if (want(TLET)) {              // Variable/Function
            bool rec = want(TREC);
            ast *decs = (want(TAND), declist());
            ast *cur = tree(rec? EREC: ELET, decs, dummy);
            append_to_let(letsp, cur);
        }
        else
            syntax("need top-level declaration");
}

bool occurs_in(type *var, type *t) {
    t = prune(t);
    if (t->form == TYPEVAR) return t == var;
    for (list *i = t->args; i; i = i->next)
        if (occurs_in(var, i->type)) return true;
    return false;
}

type *fresh(type *t, typesub **subs) {
    t = prune(t);
    if (t->form == TYPEVAR) {
        for (list *i = nongeneric; i; i = i->next)
            if (occurs_in(t, i->type)) return t;
        for (typesub *i = *subs; i; i = i->next)
            if (t == i->from) return i->to;
        *subs = new(typesub, .from=t, .to=typevar(0), .next=*subs);
        return (*subs)->to;
    }
    if (!t->args) return t;

    type *clone = new(type);
    *clone = *t;
    clone->args = 0;
    for (list *i = t->args; i; i = i->next)
        push(&clone->args, fresh(i->type, subs));
    return clone;
}

bool unifies(type *x, type *y) {
    x = prune(x);
    y = prune(y);
    if (x->form == TYPEVAR)
        return occurs_in(x, y)? x == y: (x->inst = y, true);
    if (y->form == TYPEVAR)
        return unifies(y, x);
    if (x->form != y->form || x->id != y->id) return false;
    for (list *i = x->args, *j = y->args; i && j; i = i->next, j = j->next)
        if (!unifies(i->type, j->type)) return false;
    return true;
}

void *semantic(ast *e, char *msg, ...) {
    va_list ap; va_start(ap, msg);
    pr("error %s:%d: %*\n", e->pos.fn->txt, e->pos.ln, msg, &ap);
    exit(1);
}

type *unify(ast *e, type *want, type *got) {
    if (!unifies(want, got)) semantic(e, "type error:\n  %t\n  %t", want, got);
    return prune(want);
}

ast *xform_pat(ast *e, ast *val, ast *good, ast *bad, list *env) {
    ast *test;

    switch (e->form) {
    case EINT:
    case ECHR:
    case ESTR:
    case ECID:
        if (bad->form != EFAIL) semantic(bad, "unreachable rule");
        test = tree(EAPP, tree(EAPP, ast(EID, .str=eq_id), e), val);
        return tree(EIF, test, tree(ERULE, good, bad));
    case EID:   return tree(ELET, tree(ERULE, e, val), good);
    case ETUP:  if (!e->lhs->next)
                    return xform_pat(e->lhs, val, good, bad, env);
    case ELIST:
    case EAPP:
    case ELET:
    case EREC:
    case EFN:
    case ECASE:
    case EIF:
    case ERULE:
    case EFAIL: return semantic(e, "not a pattern: %e", e);
    }
    return semantic(e, "INTERNAL ERROR: NOT XFORMED %e", e);
}

ast *xform(ast *e, list *env) {
    bool simple;
    ast *output = 0, **flat = 0, **f, *x, *y;

    switch (e->form) {
    case EINT:
    case ECHR:
    case ESTR:  return e;

    case EID:   for (list *i = env; i; i = i->next)
                    if (i->sym->id == e->str)
                        return ast(EID, .pos=e->pos, .str=e->str,
                                   .symbol=i->sym);
                return semantic(e, "undefined: %S", e->str);
    case ECID:  for (list *i = all_ctors; i; i = i->next)
                    if (i->ctor->id == e->str)
                        return ast(ECID, .pos=e->pos, .str=e->str,
                                   .ctor=i->ctor);
                return semantic(e, "undefined: %S", e->str);
    case ETUP:  if (e->lhs && !e->lhs->next) // Parenthesized exp
                    return e->lhs;
                for (ast *i = e->lhs; i; i = i->next)
                    append(&output, xform(i, env));
                return tree(ETUP, output, 0);
    case ELIST: for (ast *i = e->lhs; i; i = i->next)
                    append(&output, xform(i, env));
                return tree(ELIST, output, 0);
    case EAPP:  x = xform(e->lhs, env);
                y = xform(e->rhs, env);
                return tree(EAPP, x, y);
    case ELET:
        /*
            1. Break lets into single definitions
            2. Convert patterns to case expressions
               - r.h.s. becomes the subject
               - l.h.s. becomes rule l.h.s. and rule r.h.s. is the body
        */
        if (e->lhs->next) {              // Multiple definitions
            ast *all = 0, *rule;
            for (ast *i = e->lhs; i; i = i->next)
                rule = tree(ERULE, i->lhs, i->rhs), // omit 'next' link
                append_to_let(&all, tree(ELET, rule, 0));
            append_to_let(&all, e->rhs);
            return xform(all, env);
        }
        if (e->lhs->lhs->form != EID) {           // Pattern dec
            x = tree(ECASE, e->lhs->rhs, tree(ERULE, e->lhs->lhs, e->rhs));
            return xform(x, env);
        }
        // Simple variable dec
        x = xform(e->lhs->rhs, env);
        env = cons(new(symbol, .id=e->lhs->lhs->str, .type=typevar(0)), env);
        y = xform(e->rhs, env);
        return tree(ELET, tree(ERULE, e->lhs->lhs, x), y);

    case EREC:
        /*
            1. Check that all l.h.s. are vars
            2. Check that all r.h.s. are syntactically functions
               - Just having a function value is not enough
        */
        for (ast *i = e->lhs; i; i = i->next) {
            if (i->lhs->form != EID || i->rhs->form != EFN)
                semantic(i, "let rec only defines functions");
            env = cons(new(symbol, .id=i->lhs->str, .type=typevar(0)), env);
        }
        for (ast *i = e->lhs; i; i = i->next)
            xform(i->rhs, env);
        xform(e->rhs, env);
        return e;

    case EFN:
        /*
            1. Break pattern and/or multiple rules into single rule
               a. Create new canonical param names
               b. Convert body into a case exp
                  - Subject is the tuple of canonical params
                  - The rules are the fn rules w/ l.h.s. converted to tuples
            2. Convert multiple args into single args
         */
        simple = !e->lhs->next;                      // One rule
        for (ast *rule = e->lhs; simple && rule; rule = rule->next)
            for (ast *par = rule->lhs; simple && par; par = par->next)
                simple &= par->form == EID;
        if (!simple) {                      // Simplify patterns
            int n = count_asts(e->lhs->lhs);
            ast *canon = 0;
            for (ast *i = e->lhs->next; i; i = i->next)
                if (n != count_asts(i->lhs))
                    semantic(i, "wrong number of params on rule");
            for (int i = 0; i < n; i++)
                append(&canon, ast(EID, .str=uid()));
            ast *sub = tree(ETUP, canon, 0);
            ast *rules = 0;
            for (ast *i = e->lhs; i; i = i->next)
                append(&rules, tree(ERULE, tree(ETUP, i->lhs, 0), i->rhs));
            ast *body = tree(ECASE, sub, rules);
            return xform(tree(EFN, tree(ERULE, canon, body), 0), env);
        }
        if (e->lhs->lhs->next) {        // Simplify to one param
            f = flat_list(e->lhs->lhs, &flat);
            ast *body = e->lhs->rhs;
            while (f-- > flat)
                x = ast(EID, .pos=(*f)->pos, .str=(*f)->str),
                body = tree(EFN, tree(ERULE, x, body), 0);
            return xform(body, env);
        }
        // Single simple param, single rule
        env = cons(new(symbol, .id=e->lhs->lhs->str, .type=typevar(0)), env);
        x = tree(ERULE, e->lhs->lhs, xform(e->lhs->rhs, env));
        return tree(EFN, x, e->rhs);

    case ECASE:
        /*
            1. Wrap subject in `let` if not a variable
            2. Connect later rules to `bad` branch of priors
         */
        if (e->lhs->form != EID) {
            ast *var = ast(EID, .pos=e->lhs->pos, .str=uid());
            x = tree(ELET,
                     tree(ERULE, var, e->lhs),
                     tree(ECASE, var, e->rhs));
            return xform(x, env);
        }
        f = flat_list(e->rhs, &flat);
        ast *body = ast(EFAIL, .pos=e->pos);
        while (f-- > flat)
            body = xform_pat((*f)->lhs, e->lhs, (*f)->rhs, body, env);
        return body;

    case EIF:
    case ERULE:
    case EFAIL:
    }
    return semantic(e, "INTERNAL ERROR: NOT XFORMED %e", e);
}

list *bake(char *id, char *typespec, int opcode, list *global) {
    return cons(new(symbol,
                    .id=intern(id, -1),
                    .type=(setsrc("", typespec), suffix(ty(), TEND)),
                    .opcode=opcode),
               global);
}

int main(int argc, char **argv) {
    setvbuf(stdout, 0, _IONBF, 0);

    for (int i = 0; toks[i]; i++)
        toks[i] = intern(toks[i], -1)->txt;
    tup_id = intern("*", -1);
    list_id = intern("list", -1);
    cons_id = intern(":", -1);
    eq_id = intern("==", -1);

    int_type = basetype(intern("int", -1), 0);
    chr_type = basetype(intern("char", -1), 0);
    str_type = basetype(intern("string", -1), 0);
    bool_type = basetype(intern("bool", -1), 0);
    push(&all_types, int_type);
    push(&all_types, chr_type);
    push(&all_types, str_type);
    push(&all_types, bool_type);

    list *global = 0;
    all_types = cons(typevar(intern("a", -1)), all_types);
    global = bake("+", "int->int->int", 0, global);
    global = bake("-", "int->int->int", 0, global);
    global = bake("*", "int->int->int", 0, global);
    global = bake("/", "int->int->int", 0, global);
    global = bake("rem", "int->int->int", 0, global);
    global = bake("<", "int->int->bool", 0, global);
    global = bake(">", "int->int->bool", 0, global);
    global = bake("<=", "int->int->bool", 0, global);
    global = bake(">=", "int->int->bool", 0, global);
    global = bake("==", "a->a->bool", 0, global);
    global = bake("<>", "a->a->bool", 0, global);
    all_types = all_types->next; // Remove generic type

    opensrc(argv[1]);
    dummy = tree(ETUP, 0, 0);
    ast *lets = dummy;
    top_level(&lets);

    lets = xform(lets, global);
    pr("# %e\n", lets);
    puts("done.");
}
