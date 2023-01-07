#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <iso646.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define Obj(T,...)\
    memcpy(malloc(sizeof(obj)), &(obj){T,__VA_ARGS__}, sizeof(obj))
#define S(f,g)  app(app(S, f), g)
#define K(x)    app(K, x)
#define C(f,g)  app(app(C, f), g)
#define B(f,g)  app(app(B, f), g)
#define Y(f)    app(Y, f)
typedef enum {
    NoToken, LeftParen, RightParen, LeftBrace,
    RightBrace, Comma, Dot, NumToken, StrToken,
    IdToken, EqualToken, DefToken, ElseToken,
    FalseToken, IfToken, InToken, InfixlToken,
    InfixrToken, LetToken, RecToken, ThenToken,
    TrueToken,
} Token;
typedef enum {
    NoComb, ConsComb, AddComb, SubComb, MulComb,
    DivComb, RemComb, SComb, KComb, IComb, BComb,
    CComb, YComb, IfComb, LtComb, GtComb, EqComb,
    HdComb, TlComb, NullComb, ForceComb, PrintComb,
} Comb;
typedef enum {
    NilType, BoolType, CombType, NumType,
    StrType, IdType, ConsType, AppType,
} Type;
typedef struct obj obj;
struct obj {
    Type type;
    union {
        bool    boole;
        Comb    comb;
        double  num;
        char    *str;
        char    *id;
        struct { obj *hd, *tl; };
    };
};
typedef struct {
    char    *id;
    int     lhs;
    int     rhs;
} Op;
typedef struct Symbol {
    char    *id;
    obj     *params;
    obj     *body;
    struct Symbol *next;
} Symbol;
typedef struct {
    char    *id;
    int     arity;
    int     strict; // number of strict args
} Combinator;

char    source[65536];
char    _buf[sizeof source];
char    *src = source;
int     lineno = 1;
bool    peeked;
Token   token;
double  _num;
char    *_str;
char    *pun = "()[],.";
char    *special = "!%&$+-/:<=>?@~1^|*";
char    *resv[] = {"=", "def", "else", "false", "if",
            "in", "infixl", "infixr",
            "let", "rec", "then", "true", 0};
Combinator combinators[] = {
            {"n/a", 0},
            {":", 2, 0},
            {"+", 2, 2},
            {"-", 2, 2},
            {"*", 2, 2},
            {"/", 2, 2},
            {"rem", 2, 2},
            {"S", 3},
            {"K", 2},
            {"I", 1},
            {"B", 3},
            {"C", 3},
            {"Y", 1},
            {"if", 3, 1},
            {"<", 2, 2},
            {">", 2, 2},
            {"==", 2, 2},
            {"hd", 1, 1},
            {"tl", 1, 1},
            {"null", 1, 1},
            {"force", 1, 1},
            {"print", 1, 1},
        };
char    *interns[65536];
int     ninterns;
Op      operators[128];
int     noperators;
obj     *_nil = &(obj){NilType};
obj     *_false = &(obj){BoolType, .boole=false};
obj     *_true = &(obj){BoolType, .boole=true};
obj     *S, *K, *I, *B, *C, *Y;
Symbol  *symbols;
int     steps;

void print(char *msg, ...);
obj *eval(obj *o);
obj *_exp();
obj *abstract(char *id, obj *e);

char *intern(char *txt) {
    for (char **i = interns; i < interns + ninterns; i++)
        if (not strcmp(*i, txt))
            return *i;
    return interns[ninterns++] = strdup(txt);
}
obj *hd(obj *o) { return o->hd; }
obj *tl(obj *o) { return o->tl; }
obj *hdhd(obj *o) { return hd(hd(o)); }
obj *hdtl(obj *o) { return hd(tl(o)); }
obj *tlhd(obj *o) { return tl(hd(o)); }
obj *tltl(obj *o) { return tl(tl(o)); }
bool isnil(obj *o) { return o->type == NilType; }
bool isbool(obj *o) { return o->type == BoolType; }
bool isnum(obj *o) { return o->type == NumType; }
bool isstr(obj *o) { return o->type == StrType; }
bool isid(obj *o) { return o->type == IdType; }
bool iscons(obj *o) { return o->type == ConsType; }
bool iscomb(obj *o) { return o->type == CombType; }
bool isapp(obj *o) { return o->type == AppType; }
obj *comb(Comb x) { return Obj(CombType, .comb=x); }
obj *num(double x) { return Obj(NumType, .num=x); }
obj *str(char *x) { return Obj(StrType, .str=x); }
obj *id(char *x) { return Obj(IdType, .id=x); }
obj *cons(obj *hd, obj *tl) { return Obj(ConsType, .hd=hd, .tl=tl); }
obj *app(obj *hd, obj *tl) { return Obj(AppType, .hd=hd, .tl=tl); }
Comb comb_of(obj *o) { return iscomb(o)? o->comb: NoComb; }
Comb app_comb(obj *o) { return isapp(o)? comb_of(hd(o)): NoComb; }
bool as_bool(obj *o) {
    if (not isbool(o))
        {print("error: %o not bool.\n", o); exit(1);}
    return o->boole;
}
double as_num(obj *o) {
    if (not isnum(o))
        {print("error: %o not number.\n", o); exit(1);}
    return o->num;
}
obj *as_cons(obj *o) {
    if (not iscons(o))
        {print("error: %o not cons.\n", o); exit(1);}
    return o;
}
bool equal(obj *a, obj *b) {
    if (a->type != b->type)
        return false;
    switch (a->type) {
    case NilType:       return true;
    case BoolType:      return a->boole == b->boole;
    case CombType:      return a->comb == b->comb;
    case NumType:       return a->num == b->num;
    case StrType:       return not strcmp(a->str, b->str);
    case IdType:        return a->id == b->id;
    case ConsType:      return equal(hd(a), hd(b)) and equal(tl(a), tl(b));
    case AppType:       assert(not "handled equal(App,App)");
    default:            assert(not "handled in equal()");
    }
    return false;
}
void print_obj(obj *o) {
    switch (o->type) {
    case NilType:       print("[]"); break;
    case BoolType:      print(o->boole? "true": "false"); break;
    case CombType:      print("%s", combinators[o->comb].id); break;
    case NumType:       print("%g", o->num); break;
    case StrType:       print("\"%s\"", o->str); break;
    case IdType:        print("%s", o->id); break;
    case ConsType:      print("%O:%o", eval(hd(o)), eval(tl(o))); break;
    case AppType:       print("%O %O", hd(o), tl(o)); break;
    default:            assert(not "handled in print_obj");
    }
}
void print(char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    for (char *s = msg; *s; s++)
        if (*s == '%')
            switch (*++s) {
            case 'c':       printf("%c", va_arg(ap, int)); break;
            case 'd':       printf("%d", va_arg(ap, int)); break;
            case 'g':       printf("%g", va_arg(ap, double)); break;
            case 's':       printf("%s", va_arg(ap, char*)); break;
            case 'o':       print_obj(va_arg(ap, obj*)); break;
            case 'O':       {   obj *o = va_arg(ap, obj*);
                                if (iscons(o) or isapp(o))
                                    print("(%o)", o);
                                else print_obj(o);
                            }   break;
            default:        assert(not "handled type in print()");
            }
        else putchar(*s);
    va_end(ap);
}
obj *indir(obj *x) {
    return app(I, x);
}
obj *elide(obj *x) {
    return app_comb(x) == IComb? elide(tl(x)): x;
}
obj *eval_comb(Comb comb, obj *a, obj *b, obj *c) {
    switch (comb) {
    case ConsComb:  return indir(cons(a, b));
    case AddComb:   return indir(num(as_num(a) + as_num(b)));
    case SubComb:   return indir(num(as_num(a) - as_num(b)));
    case MulComb:   return indir(num(as_num(a) * as_num(b)));
    case DivComb:   return indir(num(as_num(a) / as_num(b)));
    case RemComb:   return indir(num(fmod(as_num(a), as_num(b))));
    case SComb:     return app(app(a, c), app(b, c));
    case KComb:     return indir(a);
    case IComb:     return a;
    case BComb:     return app(a, app(b, c));
    case CComb:     return app(app(a, c), b);
    case YComb:     return app(a, b);
    case IfComb:    return indir(as_bool(a)? b: c);
    case LtComb:    return indir(as_num(a) < as_num(b)? _true: _false);
    case GtComb:    return indir(as_num(a) > as_num(b)? _true: _false);
    case EqComb:    return indir(equal(a, b)? _true: _false);
    case HdComb:    return indir(hd(as_cons(a)));
    case TlComb:    return indir(tl(as_cons(a)));
    case NullComb:  return indir(isnil(a)? _true: _false);
    case ForceComb: return indir(a);
    case PrintComb: print("ECHO: %o\n", a); return indir(a);
    default:
        printf("comb: #%d\n", comb);
        printf("comb: %s\n", combinators[comb].id);
        assert(not "handled comb()");
    }
    return _nil;
}
obj *eval(obj *o) {
    obj *stack[1024];
    obj **st = stack;
    obj **sp = st - 1;
    *++sp = o;
    while (true) {
        steps++;
//        print(isapp(*sp)? "TRACE: @\n": "TRACE: %o\n", *sp);
        if (isapp(*sp)) {
            obj *tmp = elide(hd(*sp));
            *++sp = tmp;
            continue;
        }
        if (not iscomb(*sp))
            return *sp;
        
        Comb c = (*sp)->comb;
        Combinator *comb = combinators + c;
        obj *a[3];
    
        assert(comb->arity <= sp - stack);
    
        sp--;
        for (int i = 0; i < comb->arity; i++)
            a[i] = tl(*sp--);
        for (int i = 0; i < comb->strict; i++)
            a[i] = eval(a[i]);
        obj *original = sp[1];
        *original = *eval_comb(c, a[0], a[1], a[2]);
        *++sp = original;
    }
}
Symbol *define(char *id, obj *params, obj *body) {
    Symbol *sym = malloc(sizeof *sym);
    *sym = (Symbol){id, params, body, symbols};
    return symbols = sym;
}
Symbol *lookup(char *id) {
    for (Symbol *i = symbols; i; i = i->next)
        if (i->id == id)
            return i;
    {print("error: %s not defined.", id); exit(1);}
}
void syntax(char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    printf("error %d: ", lineno);
    vprintf(msg, ap);
    puts(".");
    exit(1);
}
bool is_id(unsigned c) {
    return isalnum(c) or c > 127 or c == '_' or c == '\'';
}
int next() {
    if (peeked) {
        peeked = false;
        return token;
    }
    while (*src)
        if (*src == '\n') {
            lineno++;
            src++;
        } else if (isspace(*src))
            src++;
        else if (*src == '#')
            while (*src and *src != '\n')
                src++;
        else break;
    
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
        for (char quote = *src++; ; )
            if (*src == quote) {
                src++;
                *t = 0;
                _str = intern(_buf);
                return token = StrToken;
            } else if (not *src or *src == '\n')
                syntax("unclosed string");
            else *t++ = *src++;
    
    while (is_id(*src))
        *t++ = *src++;
    if (t == _buf)
        while (*src and strchr(special, *src))
            *t++ = *src++;
    *t = 0;
    _str = intern(_buf);
    if (t == _buf)
        syntax("bad token");
    for (char **i = resv; *i; i++)
        if (not strcmp(*i, _buf))
            return token = i - resv + IdToken + 1;
    return token = IdToken;
}
bool peek(Token t) {
    peeked = next();
    return t == token;
}
bool want(Token t) {
    peeked = t != next();
    return t == token;
}
void need(Token t, char *msg) {
    if (not want(t))
        syntax("need %s", msg);
}
Op *next_operator(int level) {
    if (peek(IdToken))
        for (Op *i = operators; i < operators + noperators; i++)
            if (not strcmp(i->id, _str) and (not level or level == i->lhs))
                return i;
    return 0;
}
obj *_suffix(obj *o, Token t, char *spelt) {
    need(t, spelt);
    return o;
}
obj *_list() {
    if (want(RightBrace))
        return _nil;
    obj *hd = _exp();
    if (not want(Comma)) {
        need(RightBrace, "]");
        return app(app(comb(ConsComb), hd), _nil);
    }
    return app(app(comb(ConsComb), hd), _list());
}
obj *_atexp(bool required) {
    if (want(FalseToken))
        return _false;
    if (want(TrueToken))
        return _true;
    if (want(NumToken))
        return num(_num);
    if (want(StrToken))
        return str(_str);
    if (peek(IdToken) and not required and next_operator(0))
        return 0;
    if (want(IdToken))
        return id(_str);
    if (want(LeftParen))
        return _suffix(_exp(), RightParen, ")");
    if (want(LeftBrace))
        return _list();
    if (required)
        syntax("need expression");
    return 0;
}
obj *_appexp() {
    obj *x, *e = _atexp(true);
    while ((x = _atexp(false)))
        e = app(e, x);
    return e;
}
obj *_infexp(int level) {
    if (level == 11)
        return _appexp();
    obj *e = _infexp(level + 1);
    Op *op;
    while ((op = next_operator(level))) {
        next();
        e = app(id(_str), e);
        e = app(e, _infexp(op->rhs));
    }
    return e;
}
obj *_exp() {
    if (want(IfToken)) {
        obj *c = _suffix(_exp(), ThenToken, "then");
        obj *t = _suffix(_exp(), ElseToken, "else");
        obj *f = _exp();
        return app(app(app(comb(IfComb), c), t), f);
    }
    return _infexp(0);
}
void _infix() {
    bool left = token == InfixlToken;
    need(NumToken, "level");
    int lhs = _num;
    int rhs = _num + left;
    while (want(IdToken))
        operators[noperators++] = (Op){_str, lhs, rhs};
    need(Dot, ".");
}
void _def() {
    need(IdToken, "id");
    char *id = _str;
    
    obj *p, *params = _nil;
    while ((p = _atexp(false)))
        params = cons(p, params);
    
    need(EqualToken, "=");
    obj *body = _suffix(_exp(), Dot, ".");
    define(id, params, body);
}
void _program() {
    while (not peek(NoToken))
        if (want(InfixlToken)) _infix();
        else if (want(InfixrToken)) _infix();
        else if (want(DefToken)) _def();
        else need(DefToken, "def");
}
obj *optimise(obj *e) {
    assert(isapp(e) and app_comb(hd(e)) == SComb);
    obj *left = tlhd(e);
    obj *right = tl(e);
    if (app_comb(left) == KComb and app_comb(right) == KComb)
        return K(app(tl(left), tl(right)));
    if (app_comb(left) == KComb and comb_of(right) == IComb)
        return tl(left);
    if (app_comb(left) == KComb)
        return B(tl(left), right);
    if (app_comb(right))
        return C(left, tl(right));
    return e;
}
obj *abstract(char *id, obj *e) {
    switch (e->type) {
    case NilType:       return K(e);
    case BoolType:      return K(e);
    case CombType:      return K(e);
    case NumType:       return K(e);
    case StrType:       return K(e);
    case IdType:        return e->id == id? comb(IComb): K(e);
    case ConsType:      assert(not "a cons in abstract()");
    case AppType:       return optimise(S(abstract(id, hd(e)), abstract(id, tl(e))));
    default:            assert(not "handled in abstract()");
    }
    return e;
}
obj *compile(obj *e) {
    switch (e->type) {
    case NilType:       return e;
    case BoolType:      return e;
    case CombType:      return e;
    case NumType:       return e;
    case StrType:       return e;
    case IdType:        return lookup(e->id)->body;
    case ConsType:      assert(not "a cons in compile()");
    case AppType:       return app(compile(hd(e)), compile(tl(e)));
    default:            assert(not "handled in compile()");
    }
    return e;
}
void define_comb(Comb c) {
    define(intern(combinators[c].id), _nil, comb(c));
}
void oper(Comb c, int lhs, int rhs) {
    define_comb(c);
    operators[noperators++] = (Op){intern(combinators[c].id), lhs, rhs};
}
void init() {
    S = comb(SComb);
    K = comb(KComb);
    I = comb(IComb);
    B = comb(BComb);
    C = comb(CComb);
    Y = comb(YComb);
    oper(ConsComb, 1, 1);
    oper(LtComb, 4, 5);
    oper(GtComb, 4, 5);
    oper(EqComb, 4, 5);
    oper(AddComb, 5, 6);
    oper(SubComb, 5, 6);
    oper(MulComb, 6, 7);
    oper(DivComb, 6, 7);
    oper(RemComb, 6, 7);
    define_comb(HdComb);
    define_comb(TlComb);
    define_comb(NullComb);
    define_comb(ForceComb);
    define_comb(PrintComb);
}
int main() {
    init();
    fread(source, 1, sizeof source, stdin);
    _program();
    for (Symbol *sym = symbols; sym; sym = sym->next) {
        obj *body = sym->body;
        for (obj *p = sym->params; iscons(p); p = tl(p))
            body = abstract(hd(p)->id, body);
        *sym->body = *body;
    }
    for (Symbol *sym = symbols; sym; sym = sym->next)
        *sym->body = *compile(sym->body);
//    for (Symbol *sym = symbols; sym; sym = sym->next)
//        print("# %s: %o\n", sym->id, sym->body);
    print("\n\n> %o\n", eval(lookup(intern("main"))->body));
    printf("done in %d steps.\n", steps);
}