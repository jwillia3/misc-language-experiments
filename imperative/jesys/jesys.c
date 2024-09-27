enum { MAX_PARAMS=64, MAX_VARS=MAX_PARAMS, STACK_SIZE=256*1024, };
#define COPY(X) memmove(malloc(sizeof (X)), &(X), sizeof (X))
#define COPYN(XS,N) memmove(malloc((N) * sizeof *(XS)), (XS), (N) * sizeof *(XS))
#include <ctype.h>
#include <iso646.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum { TEND, TNAME, TSTRING, TINT, TLP, TRP, TLB, TRB, TLC, TRC, TCOMMA, TSEMI, TCOLON, TDOT, TSET,
               TMUL, TDIV, TREM, TADD, TSUB, TCAT, TLT, TGT, TEQ, TNE, TLE, TGE, TFUN, TIF, TTHEN, TELSE, TIS,
               TNOT, TAND, TOR, TPRINT, TDATATYPE, TVAR, TNIL, TTRUE, TFALSE, TARRAY, TINDEX, TNEG, TCALL, TBLOCK} Token;
typedef struct Expr Expr;
typedef enum { NIL, BOOL, INT, STRING, CONS, ARRAY, FUN } Type;
char *type_names[] = {"NIL", "BOOL", "INT", "STRING", "CONS", "ARRAY", "FUN"};
typedef struct Value Value;
struct Value {  Type type;
                union {
                    bool boole;
                    int  integer;
                    char *string;
                    struct Cons { char *name, **members; int n; Value *xs; } *cons;
                    struct Array { int n; Value *xs; } *array;
                    struct Fun { int n;
                                char *name, **params;
                                struct Env *env;
                                Expr *body;
                                Value (*host)(Value*);
                                bool is_cons; } *fun;
                }; };
typedef struct { int n; Expr **xs; } Exprs;
struct Expr {   Token head;
                char *filename;
                int   line;
                union {
                    char    *string, *name;
                    int     integer;
                    Expr    *x;
                    struct { Expr *lhs, *rhs; };
                    Exprs   array, block;
                    struct { Expr *f; Exprs args; } call;
                    struct { Expr *lhs; char *name; } member, is;
                    struct { char *name, **params; int n; Expr *body; } fun;
                    struct { char *name, **params; int n; Value cached; } adt;
                    struct { Expr *c, *t, *f; } cond;
                    struct { char *name; Expr *value; } var;
                }; };
typedef struct { char *name; Value value; } Symbol;
typedef struct Env {
                Symbol xs[MAX_VARS];
                int n;
                struct Env *next; } Env;
typedef struct Frame { Value f; Expr *expr; Env *old_env, *cur_env; } Frame;
Value   Nil, False={BOOL}, True={BOOL, true};
Frame   stack[STACK_SIZE], *sp = stack - 1;
Expr    *current_expr;
Env     *env;
char    *chars[256], *a_side, *b_side;
char    srcbuf[128 * 1024], tbuf[sizeof srcbuf], *src = srcbuf, *filename, *tokentext, **interns;
int     line = 1, token, peeked, tokenint, ninterns;
char    *pun = "()[]{},;:.=*/%+-^<>", *pun2 = "==<><=>=", *spec = "_!?";
char    *kwd[] = {"fun", "if", "then", "else", "is", "not", "and", "or", "print", "datatype", "var", "NIL", "TRUE", "FALSE", NULL};

void syntax(char *msg, ...) {
    printf("error %s:%d: ", filename, line);
    va_list ap; va_start(ap, msg);
    vprintf(msg, ap);
    exit(1);
}
char *intern(char *txt) {
    for (int i = ninterns - 1; i >= 0; i--)
        if (not strcmp(txt, interns[i])) return interns[i];
    interns = realloc(interns, ++ninterns * sizeof *interns);
    return interns[ninterns - 1] = strdup(txt);
}
void open_source(char *path) {
    FILE *file = fopen(filename = path, "rb");
    if (not file) syntax("cannot open");
    srcbuf[fread(srcbuf, 1, sizeof srcbuf, file)] = 0;
    fclose(file);
}
Token next() {
    char *t = tbuf, *p;
    if (peeked) { peeked = false; return token; }
    for ( ; isspace(*src) or *src == '#'; src++)
        if (*src == '\n') line++;
        else if (*src == '#') while (src[1] and src[1] != '\n') src++;
    if (not *src) return token = TEND;
    if (isdigit(src[*src == '-'])) { tokenint = strtoul(src, &src, 10); return token = TINT; }
    for (p = pun2; *p; p += 2) if (not memcmp(p, src, 2)) { src += 2; return token = (p - pun2) / 2 + TEQ; }
    if (p = strchr(pun, *src)) { src++; return token = p - pun + TLP; }
    if (*src == '"' or *src == '\'')
        for (char quote = *src++; ; line += src[-1] == '\n')
            if (not *src) syntax("unclosed string");
            else if (*src == quote) { src++; *t = 0; tokentext = intern(tbuf); return token = TSTRING; }
            else if (*src == '^') { *t++ = *++src ^ 0x40; src++; }
            else if (*src == '\\') { *t++ = *++src; src++; }
            else *t++ = *src++;
    while (isalnum(*src) or (*src and strchr(spec, *src)) or *(unsigned char*)src >= 0x80) *t++ = *src++;
    if (t == tbuf) syntax("bad character %c (U-%04X)", *src, *src & 0xff);
    *t = 0; tokentext = intern(tbuf);
    for (char **k = kwd; *k; k++) if (not strcmp(*k, tbuf)) return token = k - kwd + TFUN;
    return token = TNAME;
}
bool peek(Token t) { next(); peeked = true; return t == token; }
bool want(Token t) { next(); peeked = t != token; return not peeked; }
bool range(Token low, Token high) { next(); peeked = token < low or token > high; return not peeked; }
char *need(Token t, char *msg) { if (not want(t)) syntax("need %s", msg); return tokentext; }
Expr *expr();
Expr *tree(Expr proto) { proto.filename=filename; proto.line=line; return COPY(proto); }
Exprs list_of(Expr *item(), Token sep, Token end, char *msg) {
    Expr **xs = NULL;
    int n = 0;
    do {    if (peek(end)) break;
            xs = realloc(xs, ++n * sizeof *xs);
            xs[n - 1] = item();
    } while (want(sep));
    need(end, msg);
    return (Exprs){n, xs};
}
Expr *suffixed(Expr *item(), Token t, char *msg) { Expr *e = item(); need(t, msg); return e; }
int param_list(char **params) {
    int n = 0;
    need(TLP, "(");
    do {    if (peek(TRP)) break;
            if (n > MAX_PARAMS) syntax("Max number of params is %d", MAX_PARAMS);
            params[n++] = need(TNAME, "param name");
    } while (want(TCOMMA));
    need(TRP, ")");
    return n;
}
Expr *parse(int level) {
    if (level == 0)
        switch (next()) {
        case TNIL:      return tree((Expr){TNIL});
        case TTRUE:     return tree((Expr){TTRUE});
        case TFALSE:    return tree((Expr){TFALSE});
        case TINT:      return tree((Expr){TINT, .integer=tokenint});
        case TSTRING:   return tree((Expr){TSTRING, .string=tokentext});
        case TNAME:     return tree((Expr){TNAME, .name=tokentext});
        case TLP:       return suffixed(expr, TRP, ")");
        case TLB:       return tree((Expr){TARRAY, .array=list_of(expr, TCOMMA, TRB, "]")});
        default:        syntax("need an expression"); return NULL;
        }
    else if (level == 1) {
        Expr *e = parse(level - 1);
        while (true)
            if (want(TLB))  e = tree((Expr){TINDEX, .lhs=e, .rhs=suffixed(expr, TRB, "]")});
            else if (want(TLP)) e = tree((Expr){TCALL, .call={e, list_of(expr, TCOMMA, TRP, ")")}});
            else if (want(TDOT)) if (want(TINT))
                                     e = tree((Expr){TINDEX, .lhs=e, .rhs=tree((Expr){TINT, .integer=tokenint})});
                                 else e = tree((Expr){TDOT, .member={e, need(TNAME, "member name")}});
            else return e;
    } else if (level == 2 and want(TNOT)) return tree((Expr){TNOT, .x=expr(level - 1)});
    else if (level == 2 and want(TSUB)) return tree((Expr){TNEG, .x=expr(level - 1)});
    else if (level >= 3 and level <= 8) {
        Expr *e = parse(level - 1);
        while ( (level == 3 and range(TMUL, TREM)) or
                (level == 4 and range(TADD, TCAT)) or
                (level == 5 and want(TIS)) or
                (level == 6 and range(TLT, TGE)) or
                (level == 7 and range(TAND, TOR)) or
                (level == 8 and want(TSET))) {
                    if (token == TIS)
                        e = tree((Expr){TIS, .is={e, want(TNIL) ? intern("NIL") : need(TNAME, "type")}});
                    else {
                        Token head = token;
                        e = tree((Expr){head, .lhs=e, .rhs=parse(level >= 7 ? level : level - 1)});
                    }
                }
        return e;
    } else if (level == 9) {
        if (want(TFUN)) {
            char *name = want(TNAME) ? tokentext : NULL;
            char **params = malloc(MAX_PARAMS * sizeof *params);
            int n = param_list(params);
            Expr *body = expr();
            return tree((Expr){TFUN, .fun={name, params, n, body}});
        } else if (want(TDATATYPE)) {
            char *name = need(TNAME, "cons name");
            bool has_params = peek(TLP);
            char **params = has_params ? malloc(MAX_PARAMS * sizeof *params) : NULL;
            int n = has_params ? param_list(params) : 0;
            return tree((Expr){TDATATYPE, .adt={name, params, n}});
        } else if (want(TVAR)) {
            char *name = need(TNAME, "var name");
            need(TSET, "=");
            return tree((Expr){TVAR, .var={name, expr()}});
        } else if (want(TIF)) {
            Expr *c = suffixed(expr, TTHEN, "then");
            Expr *t = suffixed(expr, TELSE, "else");
            return tree((Expr){TIF, .cond={c,t,expr()}});
        } else if (want(TLC)) return tree((Expr){TBLOCK, .block=list_of(expr, TSEMI, TRC, "}")});
        else if (want(TPRINT)) return tree((Expr){TPRINT, .x=expr()});
        else return parse(level - 1);
    } else return parse(level - 1);
}
Expr *expr() { return parse(10); }
void fault(char *msg, ...);
Value call(Value f, int n, Value *args);
Value new_int(int i) { return (Value){INT, .integer=i}; }
Value new_string(char *x) { return (Value){STRING, .string=x}; }
Value new_cons(char *name, int n, char **members, Value *xs) {
    return (Value){CONS, .cons=COPY(((struct Cons){.name=name, .members=members, .n=n, .xs=xs}))};
}
Value new_array(int n, Value *xs) {
    return (Value){ARRAY, .array=COPY(((struct Array){.n=n, .xs=xs}))};
}
Value new_fun(struct Fun proto) { return (Value){FUN, .fun=COPY(proto)}; }
int compare(Value a, Value b) {
    if (a.type != b.type) fault("cannot compare values of different types %v vs %v", a, b);
    switch (a.type) {
    case NIL:       return 0;
    case BOOL:      return a.boole - b.boole;
    case INT:       return a.integer - b.integer;
    case STRING:    return strcmp(a.string, b.string);
    default:        fault("cannot compare %s", type_names[a.type]); return 0;
    }
}
bool equal(Value a, Value b) {
    if (a.type == NIL and b.type != NIL) return false;
    if (a.type != NIL and b.type == NIL) return false;
    if (a.type != b.type) fault("cannot compare values of different types %v vs %v", a, b);
    switch (a.type) {
    case NIL:       return true;
    case BOOL:      return a.boole == b.boole;
    case INT:       return a.integer == b.integer;
    case STRING:    return a.string == b.string ? true : not strcmp(a.string, b.string);
    case CONS:      if (a.cons == b.cons) return true;
                    if (a.cons->name != b.cons->name) return false;
                    if (a.cons->n != b.cons->n) return false;
                    if (a.cons->xs == b.cons->xs) return true;
                    for (int i = 0; i < a.cons->n; i++)
                        if (not equal(a.cons->xs[i], b.cons->xs[i])) return false;
                    return true;
    case ARRAY:     if (a.array == b.array) return true;
                    if (a.array->n != b.array->n) return false;
                    if (a.array->xs == b.array->xs) return true;
                    for (int i = 0; i < a.array->n; i++)
                        if (not equal(a.array->xs[i], b.array->xs[i])) return false;
                    return true;
    case FUN:       return a.fun == b.fun;
    default:        return false;
    }
}
char *cats(char *out, char *in) { while (*out++ = *in++); return out - 1; }
char *_to_s(char *out, Value value, bool formal) {
    switch (value.type) {
    case NIL:       return cats(out, "NIL");
    case BOOL:      return cats(out, value.boole ? "TRUE" : "FALSE");
    case INT:       {char buf[32]; sprintf(buf, "%d", value.integer); return cats(out, buf); }
    case STRING:    return formal ? cats(cats(cats(out, "'"), value.string), "'") : cats(out, value.string);
    case CONS:      if (value.cons->n == 0) return cats(out, value.cons->name);
                    out = cats(cats(out, value.cons->name), "(");
                    for (int i = 0; i < value.cons->n; i++) {
                        if (i != 0) out = cats(out, ", ");
                        out = _to_s(out, value.cons->xs[i], true);
                    } return cats(out, ")");
    case ARRAY:     out = cats(out, "[");
                    for (int i = 0; i < value.array->n; i++) {
                        if (i != 0) out = cats(out, ", ");
                        out = _to_s(out, value.array->xs[i], formal);
                    } return cats(out, "]");
    case FUN:       out = cats(out, "fun");
                    if (value.fun->name) out = cats(cats(out, " "), value.fun->name);
                    out = cats(out, "(");
                    for (int i = 0; i < value.array->n; i++) {
                        if (i != 0) out = cats(out, ", ");
                        out = cats(out, value.fun->params[i]);
                    } return cats(out, ")");
    default:        return cats(out, "!BAD VALUE!");
    }
}
char *to_s(char *out, Value value, bool formal) {
    out = out ? out : malloc(1024 * 1024);
    _to_s(out, value, formal);
    return realloc(out, strlen(out) + 1);
}
Value print(Value value) { char *s = to_s(NULL, value, false); puts(s); free(s); return value; }
Value exec(Expr *e);
char *clip(Value x, int max) {
    char *string = to_s(NULL, x, true);
    int len = strlen(string);
    if (len > max) {
        int m = max / 2,  n = max - m;
        memmove(string + m + 2, string + len - n + 2, n - 1);
        memmove(string + m - 1, "...", 3);
        string[max] = 0;
        return realloc(string, max + 1);
    } else return string;
}
void print_trace(Value f, Env *env, Expr *at) {
    printf(">");
    if (at) printf(" %s:%d", at->filename, at->line);
    if (f.type != NIL) {
        printf(" %s(", f.fun->name ? f.fun->name : "fun");
        for (int i = 0; env and i < f.fun->n; i++) {
            if (i != 0) printf(", ");
            printf("%s:%s", env->xs[i].name, clip(env->xs[i].value, 15));
        }
        printf(")");
    }
    puts("");
}
void fault(char *msg, ...) {
    printf("run-time error: ");
    va_list ap; va_start(ap, msg);
    for (char *p = msg; *p; p++)
        if (*p == '%')
            switch (*++p) {
            case 'd':   printf("%d", va_arg(ap, int)); break;
            case 's':   printf("%s", va_arg(ap, char*)); break;
            case 'v':   printf("%s", clip(va_arg(ap, Value), 60)); break;
            }
        else putchar(*p);
    puts(".\n\n ~ TRACE ~");
    print_trace(Nil, NULL, current_expr);
    for (Frame *i = sp; i >= stack; i--)
        print_trace(i->f, i->cur_env, i->expr);
    exit(1);
}
bool as_b(Value x) { return x.type != NIL and (x.type != BOOL or x.boole); }
int as_i(Value x, char *who) { if (x.type != INT) fault("%s expected int not %v", who, x); return x.integer; }
int length(Value xs) {
    return xs.type == STRING ? strlen(xs.string)
        :  xs.type == ARRAY ? xs.array->n
        : (fault("length() needs an array or string not %v", xs), 0);
}
Value index_val(Value xs, int index, Value *new) {
    int len = length(xs), i = index;
    if (i < 0) i += len;
    if (i < 0 or i > len) fault("out of bounds %d/%d on %v", index, len, xs);
    if (xs.type == STRING) {
        if (new) fault("cannot modify strings");
        return new_string(chars[xs.string[i]]);
    }
    return new ? (xs.array->xs[i] = *new) : xs.array->xs[i];
}
Value push(Value xs, Value x) {
    if (xs.type != ARRAY) fault("push() needs an array not %v", xs);
    xs.array->xs = realloc(xs.array->xs, ++xs.array->n * sizeof *xs.array->xs);
    xs.array->xs[xs.array->n - 1] = x;
    return xs;
}
Value pop(Value xs) {
    if (xs.type != ARRAY) fault("pop() needs an array not %v", xs);
    if (xs.array->n == 0) fault("pop() on []");
    return xs.array->xs[--xs.array->n];
}
Value slice(Value xs, int i, int j) {
    int len = length(xs);
    if (i < 0) i += len;
    if (i < 0 or i > len) fault("i out of bounds %d/%d on %v", i, len, xs);
    if (j < 0) j += len + 1;
    if (j < 0 or j > len) fault("j out of bounds %d/%d on %v", j, len, xs);
    if (j < i) fault("indexes cross %d < %d", j, i);
    if (xs.type == STRING and i + 1 == j) return new_string(chars[xs.string[i]]);
    if (xs.type == STRING and j == len) return new_string(xs.string + i);
    if (xs.type == STRING) {
        char *s = malloc(j - i + 1); s[j - i] = 0;
        return new_string(memmove(s, xs.string + i, j - i));
    }
    return new_array(j - i, xs.array->xs + i);
}
Value member_val(Value cons, char *name, Value *new) {
    if (cons.type != CONS) fault("non-cons value %v", cons);
    for (int i = 0; i < cons.cons->n; i++)
        if (cons.cons->members[i] == name)
            return new ? (cons.cons->xs[i] = *new) : cons.cons->xs[i];
    return fault("%s is not a member of %s", name, cons.cons->name), Nil;
}
bool is(Value x, char *name) {
    return x.type == CONS ? x.cons->name == name : type_names[x.type] == name;
}
Env *new_scope(Env *old) { return COPY(((Env){.next = old})); }
Value define(char *name, Value value) {
    if (env->n > MAX_VARS) fault("too many vars");
    env->xs[env->n++] = (Symbol){name, value};
    return value;
}
Value lookup(Env *e, char *name, Value *set) {
    if (not e) fault("%s is not defined", name);
    for (int i = e->n - 1; i >= 0; i--)
        if (e->xs[i].name == name)
            return set ? (e->xs[i].value = *set) : e->xs[i].value;
    return lookup(e->next, name, set);
}
void begin(Value f, Expr *expr, Env *new_env) {
    if (++sp >= stack + STACK_SIZE) fault("stack overflow");
    *sp = (Frame){f, expr, env, new_env};
    env = new_env;
}
Value ret(Value x) { env = sp--->old_env; return x; }
Value call(Value f, int n, Value *args) {
    if (f.type != FUN) fault("calling non-function %v", f);
    if (f.fun->n != n) fault("arg mismatch %d/%d to %v", n, f.fun->n, f);
    if (f.fun->is_cons) return new_cons(f.fun->name, n, f.fun->params, COPYN(args, n));
    if (f.fun->host) return begin(f, current_expr, f.fun->env), ret(f.fun->host(args));
    else {
        begin(f, current_expr, new_scope(f.fun->env));
        for (int i = 0; i < n; i++) define(f.fun->params[i], args[i]);
        return ret(exec(f.fun->body));
    }
}
Value _exec(Expr *e) {
    Value x, xs;
    switch (e->head) {
    case TNIL:      return Nil;
    case TTRUE:     return True;
    case TFALSE:    return False;
    case TINT:      return new_int(e->integer);
    case TSTRING:   return new_string(e->string);
    case TNAME:     return lookup(env, e->name, NULL);
    case TARRAY:    {   Value *xs = malloc(e->array.n * sizeof *xs);
                        for (int i = 0; i < e->array.n; i++) xs[i] = exec(e->array.xs[i]);
                        return new_array(e->array.n, xs); }
    case TINDEX:    {   Value xs    = exec(e->lhs);
                        int   index = as_i(exec(e->rhs), "[]");
                        return index_val(xs, index, NULL); }
    case TDOT:      return member_val(exec(e->member.lhs), e->member.name, NULL);
    case TDATATYPE: if (e->adt.cached.type == NIL and e->adt.n > 0)
                        e->adt.cached = new_fun((struct Fun){e->adt.n, e->adt.name, e->adt.params, .is_cons=true});
                    else if (e->adt.cached.type == NIL)
                        e->adt.cached = new_cons(e->adt.name, 0, NULL, NULL);
                    return define(e->adt.name, e->adt.cached);
    case TFUN:      {   Value f = new_fun((struct Fun){e->fun.n, e->fun.name, e->fun.params, env, e->fun.body});
                        if (e->fun.name) define(e->fun.name, f);
                        return f; }
    case TCALL:     {   Value f = exec(e->call.f);
                        Value xs[MAX_PARAMS];
                        for (int i = 0; i < e->call.args.n; i++) xs[i] = exec(e->call.args.xs[i]);
                        return call(f, e->call.args.n, xs); }
    case TVAR:      return define(e->var.name, exec(e->var.value));
    case TBLOCK:    x = Nil; for (int i = 0; i < e->block.n; i++) x = exec(e->block.xs[i]); return x;
    case TIF:       return as_b(exec(e->cond.c)) ? exec(e->cond.t) : exec(e->cond.f);
    case TNOT:      return as_b(exec(e->x)) ? False : True;
    case TNEG:      { int x = as_i(exec(e->x), "-"); return new_int(x); }
    case TADD:      { int x = as_i(exec(e->lhs), "+"); int y = as_i(exec(e->rhs), "+"); return new_int(x + y); }
    case TSUB:      { int x = as_i(exec(e->lhs), "-"); int y = as_i(exec(e->rhs), "-"); return new_int(x - y); }
    case TMUL:      { int x = as_i(exec(e->lhs), "*"); int y = as_i(exec(e->rhs), "*"); return new_int(x * y); }
    case TDIV:      { int x = as_i(exec(e->lhs), "/"); int y = as_i(exec(e->rhs), "/");
                        if (y == 0) fault("division by zero"); return new_int(x / y); }
    case TREM:      { int x = as_i(exec(e->lhs), "%"); int y = as_i(exec(e->rhs), "%");
                        if (y == 0) fault("division by zero"); return new_int(x / y); }
    case TCAT:      {   char *x = malloc(1024 * 1024);
                        char *end = _to_s(x, exec(e->lhs), false);
                        _to_s(end, exec(e->rhs), false);
                        return new_string(realloc(x, strlen(x))); }
    case TLT:       x = exec(e->lhs); return compare(x, exec(e->rhs)) < 0 ? True : False;
    case TGT:       x = exec(e->lhs); return compare(x, exec(e->rhs)) > 0 ? True : False;
    case TLE:       x = exec(e->lhs); return compare(x, exec(e->rhs)) <= 0 ? True : False;
    case TGE:       x = exec(e->lhs); return compare(x, exec(e->rhs)) >= 0 ? True : False;
    case TEQ:       x = exec(e->lhs); return equal(x, exec(e->rhs)) ? True : False;
    case TNE:       x = exec(e->lhs); return equal(x, exec(e->rhs)) ? False : True;
    case TIS:       return is(exec(e->is.lhs), e->is.name) ? True : False;
    case TAND:      return as_b(x = exec(e->lhs)) ? exec(e->rhs) : x;
    case TOR:       return as_b(x = exec(e->lhs)) ? x : exec(e->rhs);
    case TSET:      if (e->lhs->head == TNAME)
                        return lookup(env, e->lhs->name, (Value[]){exec(e->rhs)});
                    else if (e->lhs->head == TDOT) {
                        x = exec(e->lhs->member.lhs);
                        return member_val(x, e->lhs->member.name, (Value[]){exec(e->rhs)});
                    } else if (e->lhs->head == TINDEX) {
                        x = exec(e->lhs->lhs);
                        int i = as_i(exec(e->lhs->rhs), "=");
                        return index_val(x, i, (Value[]){exec(e->rhs)}); }
                    else { fault("bad assignment form"); }
    case TPRINT:    return print(exec(e->x));
    default:        return fault("NOT IMPLEMENTED %d", e->head), Nil;
    }
}
Value exec(Expr *e) {
    Expr *old = current_expr;
    current_expr = e;
    Value x = _exec(e);
    current_expr = old;
    return x;
}
Value loop(int n, int step, Value x, Value f) {
    if (step < 0)
        for (int i = n - 1; i >= 0; i--) x = call(f, 2, (Value[]){new_int(i), x});
    else for (int i = 0; i < n; i++) x = call(f, 2, (Value[]){new_int(i), x});
    return x; }
Value hslice(Value *a) { return slice(a[0], as_i(a[1], "slice"), as_i(a[2], "slice")); }
Value hlength(Value *a) { return new_int(length(a[0])); }
Value hpush(Value *a) { return push(a[0], a[1]); }
Value hpop(Value *a) { return pop(a[0]); }
Value hupto(Value *a) { return loop(as_i(a[0], "upto"), 1, a[1], a[2]); }
Value hdownto(Value *a) { return loop(as_i(a[0], "downto"), -1, a[1], a[2]); }
Value hread_file(Value *a) {
    if (a[0].type != STRING) fault("read_file() needs a string not %v", a);
    FILE *file = fopen(a[0].string, "r");
    if (not file) return new_cons(b_side, 1, chars + 'x', COPYN((Value[]){new_string(intern("cannot open file"))}, 1));
    fseek(file, 0, SEEK_END);
    int len = ftell(file);
    char *text = malloc(len + 1); text[len] = 0;
    rewind(file);
    fread(text, 1, len, file);
    fclose(file);
    return new_cons(a_side, 1, chars + 'x', COPYN((Value[]){new_string(text)}, 1));
}
void make_fun(struct Fun fun) {
    fun.name = intern(fun.name);
    fun.params = COPYN(fun.params, fun.n);
    for (int i = 0; i < fun.n; i++) fun.params[i] = intern(fun.params[i]);
    define(fun.name, new_fun(fun));
}
int main(int argc, char **argv) {
    #if _WIN32
        int __stdcall SetConsoleCP(unsigned), __stdcall SetConsoleOutputCP(unsigned);
        SetConsoleCP(65001); SetConsoleOutputCP(65001);
    #endif
    env = new_scope(NULL);
    for (int i = 0; i < 256; i++) chars[i] = intern((char[2]){i,0});
    for (int i = 0; i < FUN; i++) type_names[i] = intern(type_names[i]);
    a_side = intern("A"); b_side = intern("B");
    make_fun((struct Fun){1, "length", (char*[]){"xs"}, .host=hlength});
    make_fun((struct Fun){3, "slice", (char*[]){"xs", "i", "j"}, .host=hslice});
    make_fun((struct Fun){2, "push", (char*[]){"xs", "x"}, .host=hpush});
    make_fun((struct Fun){1, "pop", (char*[]){"xs"}, .host=hpop});
    make_fun((struct Fun){3, "upto", (char*[]){"n", "x", "f"}, .host=hupto});
    make_fun((struct Fun){3, "downto", (char*[]){"n", "x", "f"}, .host=hdownto});
    make_fun((struct Fun){1, "read_file", (char*[]){"path"}, .host=hread_file});
    open_source(argv[1]);
    while (not peek(0)) {
        if (want(TSEMI)) continue;
        Expr *e = expr();
        begin(Nil, e, env);
        Value x = ret(exec(e));
        //printf("> %s\n", to_s(NULL, x, true));
    }
}