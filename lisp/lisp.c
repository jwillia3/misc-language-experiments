#include <assert.h>
#include <ctype.h>
#include <iso646.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define new(T,...) ((T*)memcpy(malloc(sizeof(T)), &(T){__VA_ARGS__}, sizeof(T)))
#define fatal(o,msg,...) (print("ERROR: " msg "\n\nin: %o\n", __VA_ARGS__, o), exit(1), nil)
typedef struct string { int n; char *txt; } string;
typedef enum { Nil, Bool, Int, Symbol, String, Cons, Fn, Spec } Type;
typedef struct obj {
    Type type;
    union {
        bool boole;
        int num;
        string *sym, *str;
        struct cons *cons;
        struct fn *fn;
        struct spec *spec;
    };
} obj;
struct cons { obj car, cdr; };
struct fn { obj params, body, env; };
typedef struct { obj o, e; } Tail;
typedef Tail SpecRaw(obj o, obj e);
typedef obj SpecVec(obj o[64]);
struct spec { SpecRaw *raw; SpecVec *vec; int n; obj name; };

char    *_typenames[] = {"nil", "bool", "int", "symbol", "string", "cons", "fn",
            "spec", 0};
obj     typenames[sizeof _typenames / sizeof *_typenames];
char    *pun = "()[].,'\" \n\r\t;";
char    source[131072];
char    *src = source;
char    *filename;
int     line = 1;
int     token;
int     tokenint;
char    tokenbuf[sizeof source];
string  *tokentxt;
string  *interns[65536];
string  **lastintern = interns;
string  *singles[256];
obj     nil = {Nil};
obj     trueobj = {Bool, .boole=true};
obj     falseobj = {Bool, .boole=false};
obj     listof, quote, empty;

string *copystr(char *txt, int n) {
    return new(string, .txt=memcpy(malloc(n), txt, n), .n=n);
}
string *intern(char *txt, int n) {
    for (string **i = interns; i < lastintern; i++)
        if ((*i)->n == n and not memcmp((*i)->txt, txt, n))
            return *i;
    return *lastintern++ = copystr(txt, n);
}

obj newsym(string *x) { return (obj){Symbol, .sym=x}; }
obj csym(char *txt) { return newsym(intern(txt, strlen(txt))); }
obj newstr(string *x) { return (obj){String, .str=x}; }
obj newint(int x) { return (obj){Int, .num=x}; }
obj cons(obj car, obj cdr) {
    return (obj){Cons, .cons=new(struct cons, car, cdr)};
}
obj newfn(obj params, obj body, obj env) {
    return (obj){Fn, .fn=new(struct fn, params, body, env)};
}
obj newspec(obj name, SpecRaw *raw, SpecVec *vec, int n) {
    return (obj){Spec, .spec=new(struct spec, .raw=raw, .vec=vec, .n=n)};
}
bool isnil(obj o) { return o.type == Nil; }
bool isbool(obj o) { return o.type == Bool; }
bool isint(obj o) { return o.type == Int; }
bool issym(obj o) { return o.type == Symbol; }
bool isstr(obj o) { return o.type == String; }
bool iscons(obj o) { return o.type == Cons; }
bool isfn(obj o) { return o.type == Fn; }
bool isspec(obj o) { return o.type == Spec; }
bool isnotnil(obj o) { return not isnil(o); }
obj car(obj o) { return o.type == Cons? o.cons->car: o; }
obj cdr(obj o) { return o.type == Cons? o.cons->cdr: nil; }
obj caar(obj o) { return car(car(o)); }
obj cadr(obj o) { return car(cdr(o)); }
obj cdar(obj o) { return cdr(car(o)); }
obj cddr(obj o) { return cdr(cdr(o)); }
obj first(obj o) { return car(o); }
obj second(obj o) { return cadr(o); }
obj third(obj o) { return car(cddr(o)); }
obj fourth(obj o) { return cadr(cddr(o)); }
bool istrue(obj x) { return not isnil(x) and (not isbool(x) or x.boole); }

void printobj(obj o, bool repr);
void print(char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    for ( ; *msg; msg++)
        if (*msg != '%')
            putchar(*msg);
        else switch (*++msg) {
        case 'd':   printf("%d", va_arg(ap, int)); break;
        case 's':   printf("%s", va_arg(ap, char*)); break;
        case 'o':   printobj(va_arg(ap, obj), true); break;
        }
    va_end(ap);
}
void printobj(obj o, bool repr) {
    switch (o.type) {
    case Nil:       print("nil"); break;
    case Bool:      print(o.boole? "true": "false"); break;
    case Int:       print("%d", o.num); break;
    case String:    if (repr) {
                        putchar('"');
                        for (char *i=o.str->txt, *j=i + o.str->n; i < j; i++)
                            if (*i == '\n') print("\\n");
                            else if (*i == '"') print("\\\"");
                            else putchar(*i);
                        putchar('"');
                    } else fwrite(o.str->txt, 1, o.str->n, stdout);
                    break;
    case Symbol:    fwrite(o.str->txt, 1, o.str->n, stdout); break;
    case Cons:      putchar('(');
                    for ( ; iscons(o); o = cdr(o))
                        print("%o%s", car(o), isnil(cdr(o))? "" : " ");
                    if (isnotnil(o))
                        print(". %o", o);
                    putchar(')');
                    break;
    case Fn:        print("(lambda %o %o)", o.fn->params, o.fn->body); break;
    case Spec:      print("#SPEC"); break;
    }
}
void syntax(char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    printf("error %s:%d: ", filename, line);
    vprintf(msg, ap);
    puts(".");
    exit(1);
}
int next() {
    for ( ; isspace(*src) or *src == ';'; src++)
        if (*src == '\n')
            line++;
        else if (*src == ';')
            for ( ; src[1] and src[1] != '\n'; src++);
    if (not *src)
        return token = 0;
    if (isdigit(src[*src == '-']))
        return tokenint = strtoul(src, &src, 10),
                token = '0';
    char *t = tokenbuf;
    if (*src == '"')
        for (char quote = *src++; ; src++)
            if (not *src)
                syntax("unclosed string");
            else if (*src == quote)
                return  src++,
                        tokentxt = intern(tokenbuf, t - tokenbuf),
                        token = 's';
            else if (*src == '\\')
                for (char *i="0\0\"\"\\\\a\ab\bn\nr\rt\t", c=*++src; *i; i+=2) {
                    if (i[0] == c)
                        *t++ = i[1];
                }
            else *t++ = *src;
    if (strchr(pun, *src))
        return token = *src++;
    while (not strchr(pun, *src))
        *t++ = *src++;
    tokentxt = intern(tokenbuf, t - tokenbuf);
    if (not tokentxt->n)
        syntax("bad token");
    return token = 'a';
}
obj read();
obj readlist(int end) {
    if (token == end)
        return nil;
    obj car = read();
    if (next() == '.') {
        next();
        obj cdr = read();
        if (next() != end)
            syntax("dot must be last element");
        return cons(car, cdr);
    }
    obj cdr = readlist(end);
    return cons(car, cdr);
}
obj read() {
    if (token == '(')
        return next(), readlist(')');
    if (token == '[')
        return next(), cons(listof, readlist(']'));
    if (token == 'a')
        return newsym(tokentxt);
    if (token == 's')
        return newstr(tokentxt);
    if (token == '0')
        return newint(tokenint);
    if (token == '\'')
        return next(), cons(quote, cons(read(), nil));
    syntax("bad syntax");
    return nil;
}
bool equal(obj a, obj b) {
    if (a.type != b.type)
        return false;
    switch (a.type) {
    case Nil:       return true;
    case Bool:      return a.boole == b.boole;
    case Int:       return a.num == b.num;
    case Symbol:    return a.sym == b.sym;
    case String:    return a.str == b.str or
                        (a.str->n == b.str->n and
                        not memcmp(a.str->txt, b.str->txt, a.str->n));
    case Cons:      return equal(car(a), car(b)) and equal(cdr(a), cdr(b));
    case Fn:        return a.fn == b.fn;
    case Spec:      return a.spec == b.spec;
    }
}
int min(int a, int b) { return a < b? a: a == b? 0: 1; }
int compare(obj a, obj b) {
    if (a.type != b.type)
        return false;
    int r, len;
    switch (a.type) {
    case Nil:       return 0;
    case Bool:      return a.boole - b.boole;
    case Int:       return a.num - b.num;
    case Symbol:
    case String:    len = min(a.str->n, b.str->n);
                    return (r = memcmp(a.str->txt, b.str->txt, len))? r:
                            a.str->n < len? -1:
                            b.str->n < len? 1: 0;
    case Cons:      return (r = compare(car(a), car(b)))? r:
                            compare(cdr(a), cdr(b));
    case Fn:        return a.fn - b.fn;
    case Spec:      return a.spec - b.spec;
    }
}
obj assoc(obj name, obj e) {
    return isnil(e) ? nil :
        equal(caar(e), name) ? car(e) :
        assoc(name, cdr(e));
}
obj eval(obj o, obj e) {
    top:
    if (issym(o))
        return cdr(assoc(o, e));
    if (not iscons(o))
        return o;
    obj f = eval(car(o), e);
    if (isspec(f))
        if (f.spec->vec) {
            obj x[64], *p = x;
            for (obj a = cdr(o); isnotnil(a); a = cdr(a))
                *p++ = eval(car(a), e);
            while (p < x + f.spec->n)
                *p++ = nil;
            return f.spec->vec(x);
        } else if (f.spec->raw) {
            Tail tl = f.spec->raw(cdr(o), e);
            if (isnil(tl.e))
                return tl.o;
            o = tl.o;
            e = tl.e;
            goto top;
        }
    if (not isfn(f))
        return fatal(o, "cannot call %o.", f);
    obj ne = f.fn->env;
    obj a = cdr(o);
    obj p = f.fn->params;
    for ( ; iscons(a); a = cdr(a), p = cdr(p))
        ne = cons(cons(car(p), eval(car(a), e)), ne);
    o = f.fn->body;
    e = ne;
    goto top;
}
int toint(obj x) { return isint(x)? x.num: 0; }
string *tostr(obj x) { return isstr(x)? x.str: empty.str; }
obj _typeof(obj *x) { return typenames[x->type]; }
obj _equalp(obj *x) { return equal(x[0], x[1])? trueobj: falseobj; }
obj _compare(obj *x) { return newint(compare(x[0], x[1])); }
obj _lessthan(obj *x) { return compare(x[0], x[1]) < 0? trueobj: falseobj; }
obj _lessequal(obj *x) { return compare(x[0], x[1]) <= 0? trueobj: falseobj; }
obj _greaterthan(obj *x) { return compare(x[0], x[1]) > 0? trueobj: falseobj;}
obj _greaterequal(obj *x) { return compare(x[0], x[1]) >= 0? trueobj: falseobj;}
obj _add(obj *x) { return newint(toint(x[0]) + toint(x[1])); }
obj _sub(obj *x) { return newint(toint(x[0]) - toint(x[1])); }
obj _sym2str(obj *x) { return issym(*x)? newstr(x->sym): empty; }
obj _int2str(obj *x) {
    char buf[32];
    int n = sprintf(buf, "%d", toint(*x));
    return newstr(copystr(buf, n));
}
obj _car(obj *x) { return car(*x); }
obj _cdr(obj *x) { return cdr(*x); }
obj _ord(obj *x) { return isstr(*x) and x->str->n? newint(*x->str->txt): nil; }
obj _chr(obj *x) { return isint(*x)? newstr(singles[x->num % 256]): empty; }
obj _cons(obj *x) { return cons(x[0], x[1]); }
obj _echo(obj *x) { print("%o\n", x[0]); return x[0]; }
obj _print(obj *x) { printobj(x[0], false); return x[0]; }
obj _readfile(obj *x) {
    char fn[256];
    if (not isstr(*x) or x->str->n > sizeof fn)
        return nil;
    memmove(fn, x->str->txt, x->str->n);
    fn[x->str->n] = 0;
    FILE *file = fopen(fn, "rb");
    fseek(file, 0, SEEK_END);
    int len = ftell(file);
    char *buf = malloc(len);
    rewind(file);
    fread(buf, 1, len, file);
    fclose(file);
    obj tmp = newstr(copystr(buf, len));
    free(buf);
    return tmp;
}
obj _explode(obj *x) {
    string *s = tostr(*x);
    obj o = nil;
    for (char *i = s->txt + s->n; i-- > s->txt; )
        o = cons(newstr(singles[*i]), o);
    return o;
}
obj _implode(obj *x) {
    string *tmp, *sep = tostr(x[1]);
    int len = 0, n = 0, oi = 0, on = 0;
    for (obj i = x[0]; isnotnil(i); i = cdr(i))
        len += tostr(car(i))->n,
        n++;
    len += n <= 1? 0: (n - 1) * sep->n;
    char *buf = malloc(len);
    for (obj i = x[0]; isnotnil(i); i = cdr(i))
        tmp = tostr(car(i)),
        memmove(buf + oi, tmp->txt, tmp->n),
        oi += tmp->n,
        oi += ++on != n ? (memmove(buf + oi, sep->txt, sep->n), sep->n): 0;
    tmp = intern(buf, len);
    free(buf);
    return newstr(tmp);
}
Tail _if(obj o, obj e) {
    return (Tail){istrue(eval(first(o), e))? second(o): third(o), e};
}
Tail _begin(obj xs, obj e) {
    for ( ; isnotnil(cdr(xs)); xs = cdr(xs))
        eval(car(xs), e);
    return (Tail){car(xs), e};
}
Tail _lambda(obj o, obj e) {
    return (Tail){newfn(first(o), second(o), e)};
}
obj defname(obj i) {
    return iscons(car(i))? caar(i): car(i);
}
obj defvalue(obj i, obj e) {
    return iscons(first(i))? newfn(cdar(i), second(i), e): eval(second(i), e);
}
Tail _let(obj o, obj e) {
    for ( ; isnotnil(cdr(o)); o = cddr(o))
        e = cons(cons(defname(o), defvalue(o, e)), e);
    return (Tail){car(o), e};
}
Tail _letrec(obj o, obj e) {
    for (obj i = o; isnotnil(cdr(i)); i = cddr(i))
        e = cons(cons(defname(i), nil), e);
    for ( ; isnotnil(cdr(o)); o = cddr(o))
        assoc(defname(o), e).cons->cdr = defvalue(o, e);
    return (Tail){car(o), e};
}
Tail _quote(obj o, obj e) { return (Tail){first(o)}; }
obj matches(obj pat, obj x, obj e) {
    return issym(pat)?  cons(cons(pat, x), e):
        (iscons(pat) and equal(car(pat), quote))? (equal(second(pat), x)? e: falseobj):
        (iscons(pat) and not iscons(x))? falseobj:
        iscons(pat)? matches(cdr(pat), cdr(x), matches(car(pat), car(x), e)):
        (equal(pat, x)? e: falseobj);
}
Tail _caseof(obj o, obj e) {
    obj x = eval(first(o), e);
    for (obj ne, i = cdr(o); isnotnil(i); i = cddr(i))
        if (not isbool(ne = matches(first(i), x, e)))
            return (Tail){second(i), ne};
    return (Tail){nil};
}
Tail _env(obj o, obj e) { return (Tail){e}; }
Tail _and(obj o, obj e) {
    for (obj i = o; isnotnil(i); i = cdr(i))
        if (not istrue(eval(car(i), e)))
            return (Tail){falseobj};
    return (Tail){trueobj};
}
Tail _or(obj o, obj e) {
    for (obj i = o; isnotnil(i); i = cdr(i))
        if (istrue(eval(car(i), e)))
            return (Tail){trueobj};
    return (Tail){falseobj};
}
obj _fnparams(obj *x) { return isfn(*x)? x->fn->params: nil; }
obj _fnbody(obj *x) { return isfn(*x)? x->fn->body: nil; }
Tail _listof(obj o, obj e) {
    obj out = nil, final;
    for (obj i = o; isnotnil(i); i = cdr(i))
        out = cons(eval(car(i), e), out);
    final = nil;
    for (obj i = out, n = cdr(out); isnotnil(i); i = n, n = cdr(i))
        i.cons->cdr = final,
        final = i;
    return (Tail){final};
}
int main(int argc, char **argv) {
    obj env = nil;
    obj name;
    struct base { char *name; int n; SpecVec *vec; SpecRaw *raw; } base[] = {
        {"env",     .raw=_env},
        {"typeof",  1, _typeof},
        {"equal?",  2, _equalp},
        {"compare", 2, _compare},
        {"<",       2, _lessthan},
        {"<=",      2, _lessequal},
        {">",       2, _greaterthan},
        {">=",      2, _greaterequal},
        {"ord",     1, _ord},
        {"chr",     1, _chr},
        {"add",     2, _add},
        {"sub",     2, _sub},
        {"explode", 1, _explode},
        {"implode", 2, _implode},
        {"int->str",1, _int2str},
        {"sym->str",1, _sym2str},
        {"fn-params",1, _fnparams},
        {"fn-body", 1, _fnbody},
        {"car",     1, _car},
        {"cdr",     1, _cdr},
        {"cons",    2, _cons},
        {"if",      .raw=_if},
        {"begin",   .raw=_begin},
        {"lambda",  .raw=_lambda},
        {"let",     .raw=_let},
        {"letrec",  .raw=_letrec},
        {"echo",    1, _echo},
        {"print",   1, _print},
        {"quote",   .raw=_quote},
        {"caseof",  .raw=_caseof},
        {"listof",  .raw=_listof},
        {"read-file",1, _readfile},
        {"and",     .raw=_and},
        {"or",      .raw=_or},
        {0}
    };
    
    empty = csym("");
    quote = csym("quote");
    listof = csym("listof");
    for (int i = 0; i < 256; i++)
        singles[i] = intern((char[1]){i}, 1);
    for (int i = 0; _typenames[i]; i++)
        typenames[i] = csym(_typenames[i]);
    env = cons(cons(csym("true"), trueobj), env);
    env = cons(cons(csym("false"), falseobj), env);
    for (struct base *i = base; i->name; i++)
        name = csym(i->name),
        env = cons(cons(name, newspec(name, i->raw, i->vec, i->n)), env);
    FILE *file = fopen(filename = argv[1], "rb");
    if (not file)
        syntax("could not open");
    fread(source, 1, sizeof source, file);
    fclose(file);
    next();
    obj o = read();
//    print("> %o\n", o);
    o = eval(o, env);
//    print("> %o\n", o);
}