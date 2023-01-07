#include <assert.h>
#include <ctype.h>
#include <iso646.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ARITY   8
#define New(T,...) memcpy(malloc(sizeof(T)), &(T){__VA_ARGS__}, sizeof(T))
#define Obj(T,...) New(obj, .type=T, __VA_ARGS__)
#define CK(TYPE, N)\
    for (int i = 0; i < N; i++)\
        if (not TYPE (a[i]))\
            print("`%s` needed number not %o%!", comb->id, a[i]);
#define CK_NUM(N) CK(isnum, N)
#define CK_BOOL(N) CK(isbool, N)
#define X a[0]
#define Y a[1]
#define Z a[2]
#define RET(X) C = X; break
#define RET_BOOL(X) C = (X)? _true: _false; break

typedef enum {
    NoToken, LeftParen, RightParen, LeftBrace, RightBrace, Comma, Dot,
    NumToken, StrToken, IdToken, EqualToken, DefToken, ElseToken, FalseToken,
    IfToken, InToken, InfixlToken, InfixrToken, ThenToken, TrueToken,
    WhereToken,
} Token;

typedef struct obj obj;
typedef enum {
    NilType, BoolType, NumType, StrType,
    IdType, CombType, ConsType, AppType, ClosureType,
    // All types that can be printed w/o parens should be before CombType
} Type;
typedef enum {
    AddComb, SubComb, MulComb, DivComb, RemComb,
    LtComb, LeComb, GtComb, GeComb, EqComb, NeComb, NotComb,
    ConsComb, HdComb, TlComb, IfComb, DbgComb,
    UserComb,
} Builtin;
struct obj {
    Type type;
    union {
        bool boole;
        double num;
        char *str;
        struct { char *id; int index; };
        Builtin comb;
        struct { obj *hd, *tl; };
    };
};
typedef struct {
    char    *id;
    int     arity;
    int     strictness;
    obj     *value;
    obj     *body;
    obj     *params[MAX_ARITY];
} Combinator;
typedef struct {
    char    *id;
    int     lhs;
    int     rhs;
} Op;

int print_obj(obj *o);
obj *eval(obj *o, obj *E);
obj *force(obj *e);

char    source[65536];
char    _buf[sizeof source];
char    *src = source;
int     lineno = 1;
bool    peeked;
Token   token;
double  _num;
char    *_txt;
char    *interns[65536];
int     ninterns;
char    *special = "!$%&*+-/:<=>?^|~";
char    *pun = "()[],.";
char    *resv[] = {
            "=", "def", "else", "false",
            "if", "in", "infixl", "infixr",
            "then", "true", "where", 0};
Combinator combs[1024] = {
            {"+",   2, 2},
            {"-",   2, 2},
            {"*",   2, 2},
            {"/",   2, 2},
            {"rem", 2, 2},
            {"<",   2, 2},
            {"<=",  2, 2},
            {">",   2, 2},
            {">=",  2, 2},
            {"==",  2, 2},
            {"<>",  2, 2},
            {"not", 1, 1},
            {":",   2, 0},
            {"hd",  1, 1},
            {"tl",  1, 1},
            {"if",  3, 1},
            {"dbg", 1, 1},
        };
int     ncombs;
Op      operators[256];
int     noperators;
obj     *nil = &(obj){NilType};
obj     *_true = &(obj){BoolType, .boole=true};
obj     *_false = &(obj){BoolType, .boole=false};
obj     *_cons, *_if;


bool needs_paren(obj *o) {
    return o->type > CombType;
}
int print(char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    for ( ; *msg; msg++)
        if (*msg != '%')
            putchar(*msg);
        else switch (*++msg) {
        case 'd':   printf("%d", va_arg(ap, int)); break;
        case 'g':   printf("%g", va_arg(ap, double)); break;
        case 's':   printf("%s", va_arg(ap, char*)); break;
        case 'e':   print_obj(va_arg(ap, obj*)); break;
        case 'o':   print_obj(force(va_arg(ap, obj*))); break;
        case 'O':   {
                        obj *o = force(va_arg(ap, obj*));
                        if (needs_paren(o))
                            print("(%o)", o);
                        else
                            print_obj(o);
                    } break;
        case '!':   printf(".\nexiting.\n"); exit(1);
        }
    va_end(ap);
    return 0;
}
bool isnil(obj *o) { return o->type == NilType; }
bool isbool(obj *o) { return o->type == BoolType; }
bool isnum(obj *o) { return o->type == NumType; }
bool isstr(obj *o) { return o->type == StrType; }
bool isid(obj *o) { return o->type == IdType; }
bool iscomb(obj *o) { return o->type == CombType; }
bool iscons(obj *o) { return o->type == ConsType; }
bool isapp(obj *o) { return o->type == AppType; }
bool isclosure(obj *o) { return o->type == ClosureType; }
obj *hd(obj *o) { return o->hd; }
obj *tl(obj *o) { return o->tl; }
obj *thenum(double x) { return Obj(NumType, .num=x); }
obj *thestr(char *x) { return Obj(StrType, .str=x); }
obj *theid(char *x, int i) { return Obj(IdType, .id=x, .index=i); }
obj *thecomb(Builtin x) { return Obj(CombType, .comb=x); }
obj *cons(obj *hd, obj *tl) { return Obj(ConsType, .hd=hd, .tl=tl); }
obj *app(obj *hd, obj *tl) { return Obj(AppType, .hd=hd, .tl=tl); }
obj *closure(obj *hd, obj *tl) { return Obj(ClosureType, .hd=hd, .tl=tl); }
obj *force(obj *e) { // Note that this overwrites closure objects!
    while (isclosure(e))
        *e = *eval(hd(e), tl(e));
    return e;
}
Combinator *try_find_comb(char *id) {
    for (Combinator *c = combs; c < ncombs + combs; c++)
        if (c->id == id)
            return c;
    return 0;
}
Combinator *find_comb(char *id) {
    Combinator *c = try_find_comb(id);
    if (not c)
        print("%s is not defined%!", id);
    return c;
}

bool equal(obj *a, obj *b) {
    if (a->type != b->type)
        return false;
    
    switch (a->type) {
    case NilType:   return true;
    case BoolType:  return a->boole == b->boole;
    case NumType:   return a->num == b->num;
    case StrType:   return not strcmp(a->str, b->str);
    case IdType:    return a->str == b->str;
    case CombType:  return a->comb == b->comb;
    case ConsType:  return equal(hd(a), hd(b)) and equal(tl(a), tl(b));
    case AppType:   assert(not "applications can't be compared"); return false;
    case ClosureType: assert(not "closures can't be compared"); return false;
    default:        assert(not "handled in equal()"); return false;
    }
}
int print_obj(obj *o) {
    switch (o->type) {
    case NilType:   return print("[]");
    case BoolType:  return print(o->boole? "true": "false");
    case NumType:   return print("%g", o->num);
    case StrType:   return print("\"%s\"", o->str);
    case IdType:    return print("%s", o->id);
    case CombType:  return print("%s", combs[o->comb].id);
    case ConsType:  return print("%O:%o", hd(o), tl(o));
    case AppType:   return print("%O %O", hd(o), tl(o));
    case ClosureType: return print("#(%o, %o)", hd(o), tl(o));
    default:        assert(not "handled in print_obj()"); return 0;
    }
}

char *intern(char *txt) {
    for (char **i = interns; i < ninterns + interns; i++)
        if (not strcmp(*i, txt))
            return *i;
    return interns[ninterns++] = strdup(txt);
}
void syntax(char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    printf("error %d: ", lineno);
    vprintf(msg, ap);
    puts(".");
    exit(1);
}
bool isidchr(unsigned c) {
    return isalnum(c) or c > 128 or c == '_' or c == '\'';
}
Token next() {
    if (peeked) {
        peeked = false;
        return token;
    }
    for ( ; isspace(*src) or *src == '#'; src++)
        if (*src == '\n')
            lineno++;
        else if (*src == '#')
            while (src[1] and src[1] != '\n')
                src++;
    char *p, *t = _buf;
    if (not *src)
        return token = NoToken;
    if (isdigit(src[*src == '-'? 1: 0])) {
        _num = strtod(src, &src);
        return token = NumToken;
    }
    if ((p = strchr(pun, *src))) {
        src++;
        return token = p - pun + LeftParen;
    }
    if (*src == '"')
        for (char quote = *src++; ; src++)
            if (not *src)
                syntax("unclosed string");
            else if (*src == quote) {
                src++;
                *t = 0;
                _txt = intern(_buf);
                return token = StrToken;
            } else
                *t++ = *src;
    while (isidchr(*src))
        *t++ = *src++;
    if (t == _buf)
        while (*src and strchr(special, *src))
            *t++ = *src++;
    if (t == _buf)
        syntax("bad token %c", *src);
    *t = 0;
    _txt = intern(_buf);
    for (char **r = resv; *r; r++)
        if (not strcmp(*r, _buf))
            return token = r - resv + IdToken + 1;
    return token = IdToken;
}
bool peek(Token t) {
    peeked = next();
    return token == t;
}
bool want(Token t) {
    peeked = next() != t;
    return token == t;
}
void need(Token t, char *spelt) {
    if (not want(t))
        syntax("need %s", spelt);
}

obj *_exp();
Op *operator_next(int level) {
    if (peek(IdToken))
        for (Op *op = operators; op->id; op++)
            if (op->id == _txt and (not level or level == op->lhs))
                return op;
    return 0;
}
obj *_suffix(obj *x, Token t, char *spelt) {
    need(t, spelt);
    return x;
}
obj *_list() {
    if (want(RightBrace))
        return nil;
    obj *hd = _exp();
    if (not want(Comma)) {
        need(RightBrace, "]");
        return app(app(_cons, hd), nil);
    }   
    obj *tl = _list();
    return app(app(_cons, hd), tl);
}
obj *_atexp(bool required) {
    if (want(FalseToken))   return _false;
    if (want(TrueToken))    return _true;
    if (want(NumToken))     return thenum(_num);
    if (want(StrToken))     return thestr(_txt);
    if (not required and peek(IdToken) and operator_next(0)) return 0;
    if (want(IdToken))      return theid(_txt, -1);
    if (want(LeftParen))    return _suffix(_exp(), RightParen, ")");
    if (want(LeftBrace))    return _list();
    if (required) syntax("need expression");
    return 0;
}
obj *_appexp() {
    obj *e = _atexp(true), *x;
    while ((x = _atexp(false)))
        e = app(e, x);
    return e;
}
obj *_infexp(int level) {
    if (level == 11)
        return _appexp();
    obj *e = _infexp(level + 1);
    Op *op;
    while ((op = operator_next(level))) {
        next();
        e = app(theid(op->id, -1), e);
        e = app(e, _infexp(op->rhs));
    }
    return e;
}
obj *_exp() {
    if (want(IfToken)) {
        obj *c = _suffix(_exp(), ThenToken, "then");
        obj *t = _suffix(_exp(), ElseToken, "else");
        obj *f = _exp();
        return app(app(app(_if, c), t), f);
    }
    return _infexp(0);
}
bool _program() {
    if (want(InfixlToken) or want(InfixrToken)) {
        bool left = token == InfixlToken;
        need(NumToken, "level");
        int lhs = _num;
        int rhs = lhs + left;
        while (want(IdToken))
            operators[noperators++] = (Op){_txt, lhs, rhs};
        need(Dot, ".");
        return true;
    }
    if (want(DefToken)) {
        need(IdToken, "id");
        char *id = _txt;
        int arity = 0;
        obj *params[MAX_ARITY] = {0};
        
        if (try_find_comb(id))
            syntax("%s already defined", id);
        
        while (want(IdToken)) {
            if (arity >= MAX_ARITY)
                syntax("Too many parameters; max is %d", MAX_ARITY);
            params[arity++] = theid(_txt, -1);
        }
        need(EqualToken, "=");
        
        obj *body = _suffix(_exp(), Dot, ".");
        
        combs[ncombs] = (Combinator) {
            .id = id,
            .arity = arity,
            .strictness = 0,
            .value = thecomb(ncombs),
            .body = body
        };
        memcpy(combs[ncombs].params, params, arity * sizeof *params);
        ncombs++;
        return true;
    }
    return false;
}
int debruijn(char *id, obj *env) {
    int index = 0;
    for (obj *e = env; not isnil(e); e = tl(e), index++)
        if (hd(e)->id == id)
            return index;
    return -1;
}
obj *compile(obj *o, obj *env) {
    int index;
    switch (o->type) {
    case NilType:   return o;
    case BoolType:  return o;
    case NumType:   return o;
    case StrType:   return o;
    case IdType:    return (index = debruijn(o->id, env)) > -1?
                                theid(o->id, index):
                                thecomb(find_comb(o->id) - combs);
    case CombType:  return o;
    case ConsType:  assert(not "conses should not exist in source");
    case AppType:   return app(compile(hd(o), env), compile(tl(o), env));
    case ClosureType: assert(not "closures should not exist in source");
    default:        assert(not "handled in compile()"); return 0;
    }
}
void print_stack(obj **s, obj **bs) {
    for ( ; s > bs; s--)
        print("%e ", *s);
}
bool needs_evaluation(obj *o) {
    switch (o->type) {
    case NilType:   return false;
    case BoolType:  return false;
    case NumType:   return false;
    case StrType:   return false;
    case IdType:    return true;
    case CombType:  return false;
    case ConsType:  return false;
    case AppType:   return true;
    case ClosureType: return true;;
    default:        assert(not "handled in needs_evaluation()"); return 0;
    }
}
obj *eval(obj *initial, obj *E) {
    // REDUCTION STRATEGY
    // [\x.e]   POP S TO E, C=e
    // [mn]     PUSH(n,E) TO S, C=m
    // [x]      C,E=E[x]
    // [k]      RETURN k
    // [+]      POP 2x S TO E, RETURN ADDED RESULT
    obj *C = initial;
    obj *stk[1024];
    obj **BS = stk - 1;
    obj **S = BS;
    obj *a[MAX_ARITY];
    obj *tmp;
    
    while (true) {
        print("\nTRACE: C=%e; E=%e; S=(%d)", C, E, S - BS);
        print_stack(S, BS);
        puts("");
        
        if (isapp(C)) {
            *++S = closure(tl(C), E);
            C = hd(C);
        } else if (isid(C)) {
            for (int i = 0; i < C->index; i++)
                E = tl(E);
            obj *x = hd(E);
            C = hd(x);
            E = tl(x);
        } else if (iscomb(C)) {
            Combinator *comb = combs + C->comb;
            assert(comb->arity < MAX_ARITY);
            
            if (S - BS + 1 < comb->arity)
                print("error: %s given %d/%d args%!",
                    comb->id,
                    S - BS + 1,
                    comb->arity);
                    
            for (int i = 0; i < comb->arity; i++)
                a[i] = *S--;
            for (int i = 0; i < comb->strictness; i++)
                a[i] = eval(hd(a[i]), tl(a[i]));
            
            if (comb->body) {
                for (int i = comb->arity; i-- > 0; )
                    E = cons(a[i], E);
                C = comb->body;
            } else
                switch (C->comb) {
                case AddComb:   CK_NUM(2); RET(thenum(X->num + Y->num));
                case SubComb:   CK_NUM(2); RET(thenum(X->num - Y->num));
                case MulComb:   CK_NUM(2); RET(thenum(X->num * Y->num));
                case DivComb:   CK_NUM(2); RET(thenum(X->num / Y->num));
                case RemComb:   CK_NUM(2); RET(thenum(fmod(X->num, Y->num)));
                case LtComb:    CK_NUM(2); RET_BOOL(X->num < Y->num);
                case LeComb:    CK_NUM(2); RET_BOOL(X->num <= Y->num);
                case GtComb:    CK_NUM(2); RET_BOOL(X->num > Y->num);
                case GeComb:    CK_NUM(2); RET_BOOL(X->num >= Y->num);
                case EqComb:    RET_BOOL(equal(X, Y));
                case NeComb:    RET_BOOL(not equal(X, Y));
                case NotComb:   CK_BOOL(1); RET_BOOL(not X->boole);
                case ConsComb:  RET(cons(X, Y));
                case HdComb:    RET(hd(X));
                case TlComb:    RET(tl(X));
                case IfComb:    CK_BOOL(1);
                                tmp = X->boole? Y: Z;
                                C = hd(tmp);
                                E = tl(tmp);
                                break;
                case DbgComb:   print("DEBUG %o\n", X); RET(X);
                default:        assert(not "handled comb");
                }
        } else if (isclosure(C)) {
            E = tl(C);
            C = hd(C);
        } else if (S > BS)
            print("error: %o left on stack%!", *S);
        else 
            return C;
    }
}

int main() {
    for (int i = 0; i < UserComb; i++) {
        combs[i].id = intern(combs[i].id);
        combs[i].value = thecomb(i);
    }
    ncombs = UserComb;
    _cons = combs[ConsComb].value;
    _if = combs[IfComb].value;
    fread(source, 1, sizeof source, stdin);
    while (_program());
    need(NoToken, "end of file");
    
    for (Combinator *c = combs + UserComb; c < ncombs + combs; c++) {
        obj *env = nil;
        for (int i = c->arity; i-- > 0; )
            env = cons(c->params[i], env);
        c->body = compile(c->body, env);
    }
    
    for (int i = UserComb; i < ncombs; i++)
        print("# %s = %o .\n", combs[i].id, combs[i].body);
    puts("");
    
    
    Combinator *c = find_comb(intern("main"));
    if (c->arity)
        syntax("main cannot take arguments");
    
    print("> %o\n", eval(c->body, nil));

}