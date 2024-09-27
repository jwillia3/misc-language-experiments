#define STACK_SIZE  8*1024*1024

#include <ctype.h>
#include <math.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define new(T,...) (T*)memcpy(malloc(sizeof(T)), &(T){__VA_ARGS__}, sizeof(T))
#define ast(F,P,...) new(struct ast, .form=F, .pos=P, __VA_ARGS__)

typedef enum {
    /*
        This enum must stay in order with tokname.
        NULL marks the end of tokens that can be read from source.
        Other tokens are semantic for use in ASTs.
        All single-character punctuation must be consecutive.
        All two-character punctuation must be consecutive.
        All keywords come after Tid and before NULL.
     */
    Teof, Tnum, Tstring, Tlparen, Trparen, Tlbrace, Trbrace, Tlcurly, Trcurly,
    Tcomma, Tdot, Tcolon, Tsemi, Tquest, Tnot,
    Tmul, Tdiv, Trem, Tadd, Tsub, Tcat, Tlt, Tgt, Tset,
    Teq, Tne, Tle, Tge, Tmuls, Tdivs, Trems, Tadds, Tsubs, Tand,
    Tor, Tqq, Tarrow,
    Tid, Timport, Tclass, Tdef, Tvar, Tif, Telse, Twhile, Treturn,
    Ttrue, Tfalse, Tnull, Tprint,
    Tend_of_tokens,
    Ttuple, Tlist, Thash, Tindex, Tcall, Tneg, Tblock,
} toktype;

static const char *tokname[] = {
    "END", "number", "string", "(", ")", "[", "]", "{", "}",
    ",", ".", ":", ";", "?", "!",
    "*", "/", "%", "+", "-", "^", "<", ">", "=",
    "==", "!=", "<=", ">=", "*=", "/=", "%=", "+=", "-=", "&&",
    "||", "??", "=>",
    "id", "import", "class", "def", "var", "if", "else", "while", "return",
    "true", "false", "null", "print",
    NULL,
    "tuple", "list", "hash", "index", "call", "neg", "block",
};
static const int precedence[256] = {
    [Tqq]=7,
    [Tmul]=6, [Tdiv]=6, [Trem]=6, [Tadd]=5, [Tsub]=5, [Tcat]=5,
    [Teq]=4, [Tne]=4, [Tlt]=4, [Tgt]=4, [Tle]=4, [Tge]=4, [Tand]=3, [Tor]=2,
    [Tset]=1, [Tmuls]=1, [Tdivs]=1, [Trems]=1, [Tadds]=1, [Tsubs]=1,
};
static const bool right_assoc[256] = {
    [Tqq]=1, [Tand]=1, [Tor]=1,
    [Tset]=1, [Tmuls]=1, [Tdivs]=1, [Trems]=1, [Tadds]=1, [Tsubs]=1,
};
static const toktype set_operator[256] = {
    [Tmuls]=Tmul, [Tdivs]=Tdiv, [Trems]=Trem, [Tadds]=Tadd, [Tsubs]=Tsub,
};

static const char esc[256] = {
    ['\0']='0', ['\a']='a', ['\b']='b', ['\033']='e', ['\f']='f', ['\v']='v',
    ['\n']='n', ['\r']='r', ['\t']='t', ['\\']='\\', ['"']='"',
};
static const char unesc[256] = {
    ['a']='\a', ['b']='\b', ['e']='\033', ['f']='\f', ['v']='\v',
    ['n']='\n', ['r']='\r', ['t']='\t', ['\\']='\\',
};

typedef struct { int len; unsigned hash; char text[]; } string;
typedef struct pos { string *name; int line, col; } pos;

typedef struct ast ast;
struct ast {
    toktype form;
    union {
        double  num;
        string  *str;
        string  *id;
        ast     *uni;
        ast     *ret;
        struct { int n; ast **vals; } list, tuple;
        struct { int n; struct ent_ast { string *key; ast *val; } *ents; } hash;
        struct { ast *lhs, *index; } index;
        struct proto { int n; string **params, *name; ast *body; } *proto;
        struct { int n; ast *f, **args; } call;
        struct { ast *lhs, *rhs; } bin;
        struct { ast *a, *b, *c; } cond;
        struct { int n; ast **stmts; } block;
        struct { ast *cond, *body; } loop;
        struct { string *name; ast *val; } var;
        struct { ast *lhs; string *name; } dot;
    };
    pos pos;
};

typedef struct value value;
static const char *classnames[] = {
    "Null", "Bool", "Num", "String", "Tuple", "List", "Hash", "Object", "Fn"
};
struct value {
    enum { Null, Bool, Num, String, Tuple, List, Hash, Object, Fn } type;
    union {
        bool    boole;
        double  num;
        string  *str;
        struct tuple { int n; value *vals; } *tuple;
        struct list { int n, cap; value *vals; } *list;
        struct hash { int n, cap, *order; struct entry *ents; } *obj, *hash;
        struct fn { struct proto *proto; struct hash *env; } *fn;
    };
};
struct entry { string *key; value val; };

struct ast_arg { ast **args; struct hash *env; };

static char     *src;
static char     *sol;
static bool     peeked;
static toktype  token;
static pos      tokpos;
static double   toknum;
static string   *tokstr;
static string   **interns;
static int      ninterns;
static value    _null={Null}, _false={Bool}, _true={Bool, .boole=true}, _unit;
static string   *characters[256];
static string   *nonlocal_id, *class_id, *name_id;
static value    ret_val;
static sigjmp_buf *ret;
static struct hash *classes[sizeof classnames / sizeof *classnames];


static void print(const char *msg, ...);

static unsigned hash_string(const char *text, int len) {
    unsigned h = 5381;
    for (int i = 0; i < len; i++)
        h = h * 33 + text[i];
    return h;
}
#define thenum(X) (value){ Num, .num=(X) }
#define thestr(X) (value){ String, .str=(X) }
#define thetuple(X) (value){ Tuple, .tuple=(X) }
#define thelist(X) (value){ List, .list=(X) }
#define thehash(X) (value){ Hash, .hash=(X) }
#define theobj(X) (value){ Object, .obj=(X) }
#define thefn(X) (value){ Fn, .fn=(X) }


static string *newstr(const char *text, int len) {
    string *str = malloc(sizeof *str + len + 1);
    str->len = len;
    str->text[len] = 0;
    if (text) {
        unsigned h = 5381;
        for (int i = 0; i < len; i++)
            h = h * 33 + text[i],
            str->text[i] = text[i];
        str->hash = h;
    }
    return str;
}

static string *intern(const char *text, int len) {
    unsigned h = hash_string(text, len);
    for (int i = 0; i < ninterns; i++)
        if (interns[i]->hash == h &&
            interns[i]->len == len &&
            !memcmp(interns[i]->text, text, len))
        {
            return interns[i];
        }
    interns = realloc(interns, (ninterns + 1) * sizeof *interns);
    return interns[ninterns++] = newstr(text, len);
}

static struct tuple *newtuple(value *old, int n) {
    value *vals = malloc(n * sizeof *vals);
    if (old) memcpy(vals, old, n * sizeof *vals);
    return new(struct tuple, .n=n, .vals=vals);
}

static struct list *newlist(value *old, int n, int cap) {
    value *vals = malloc(cap * sizeof *vals);
    if (old) memcpy(vals, old, n * sizeof *vals);
    return new(struct list, .n=n, .cap=cap, .vals=vals);
}

static struct hash *newhash(int cap) {
    cap = (cap >= 8? cap: 8);
    cap = cap + cap * 1/4;
    int             *order = malloc(cap * sizeof *order);
    struct entry    *ents = calloc(1, cap * sizeof *ents);
    return new(struct hash, .cap=cap, .order=order, .ents=ents);
}

static struct hash *newobj(void) {
    return newhash(0);
}

static struct fn *newfn(struct proto *proto, struct hash *env) {
    return new(struct fn, .proto=proto, .env=env);
}

static string *try_unescape(const char *text, int len) {
    int j = 0;
    while (j < len && text[j] != '\\') j++;
    if (j == len)
        return intern(text, len);

    string *out = newstr(NULL, len);
    char *p = out->text;
    for (int i = 0; i < len; i++) {
        j = i;
        while (j < len && text[j] != '\\') *p++ = text[j++];
        if (j < len)
            *p++ = unesc[text[j + 1]]? unesc[text[j + 1]]: text[j + 1];
        i = j + 1;
    }
    *p = 0;
    out->len = p - out->text;
    out->hash = hash_string(out->text, out->len);
    return out;
}

static string *read_file(const char *path) {
    FILE *file = fopen(path, "rb");
    if (!file) return NULL;

    fseek(file, 0, SEEK_END);
    int len = ftell(file);
    rewind(file);
    string *str = newstr(NULL, len);
    fread(str->text, 1, len, file);
    str->hash = hash_string(str->text, len);
    fclose(file);
    return str;
}

static void print_value(value x, bool repr) {
    int     len;
    value   *xs;
    char    *delim;

    switch (x.type) {
    case Null:      fputs("null", stdout); break;
    case Bool:      fputs(x.boole? "true": "false", stdout); break;
    case Num:       printf("%g", x.num); break;
    case String:    if (repr) {
                        putchar('"');
                        int     n = x.str->len;
                        char    *text = x.str->text;
                        for (int i = 0; i < n; i++) {
                            int j = i;
                            while (j < n && !esc[text[j]]) j++;
                            fwrite(text + i, 1, j - i, stdout);
                            if (j < n) putchar('\\'), putchar(esc[text[j]]);
                            i = j;
                        }
                        putchar('"');
                    } else
                        fwrite(x.str->text, 1, x.str->len, stdout);
                    break;
    case Tuple:     len = x.tuple->n, xs = x.tuple->vals, delim="()"; goto mult;
    case List:      len = x.list->n, xs = x.list->vals, delim="[]"; goto mult;
    case Object:    puts("TODO");
    case Hash:      putchar('{');
                    for (int i = 0; i < x.hash->n; i++) {
                        print(i? ", %S: %v": "%S: %v",
                              x.hash->ents[x.hash->order[i]].key,
                              x.hash->ents[x.hash->order[i]].val);
                    }
                    putchar('}'); break;
    case Fn:        if (x.fn->proto->name)
                        print("%S", x.fn->proto->name);
                    else fputs("#fn", stdout);
                    break;
    }
    return;
    mult:
    putchar(delim[0]);
    for (int i = 0; i < len; i++) print(i? ", %v": "%v", xs[i]);
    putchar(delim[1]);
}

static void vprint(const char *msg, va_list ap) {
    string *str;
    const char *msg2;
    for (const char *s = msg; *s; ) {
        const char *base = s;
        while (*s && *s != '%') s++;
        if (base != s)
            fwrite(base, 1, s - base, stdout);
        if (*s == '%')
            switch ((s += 2)[-1]) {
            case 'd':   printf("%d", va_arg(ap, int)); break;
            case 'g':   printf("%g", va_arg(ap, double)); break;
            case 's':   fputs(va_arg(ap, char*), stdout); break;
            case 'S':   str = va_arg(ap, string*);
                        fwrite(str->text, 1, str->len, stdout); break;
            case 'v':   print_value(va_arg(ap, value), true); break;
            case 'V':   print_value(va_arg(ap, value), false); break;
            case '*':   msg2 = va_arg(ap, const char*);
                        vprint(msg2, *va_arg(ap, va_list*)); break;
            }
    }
}

static void print(const char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    vprint(msg, ap);
}

static void *error(pos p, const char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    print("error %S:%d:%d: %*\n", p.name, p.line, p.col, msg, &ap);
    exit(1);
    return NULL;
}

static bool open_source(const char *path) {
    if (!path)
        return false;
    tokpos = (pos) {intern(path, strlen(path)), 1, 1};
    src = sol = NULL;
    peeked = false;
    
    string *str = read_file(path);
    if (!str) return false;
    src = sol = str->text;
    return true;
}

static toktype next(void) {
    if (peeked)
        return peeked = false, token;
    for ( ; isspace(*src) || *src == '#'; src++)
        if (*src == '\n') tokpos.line++, sol = src + 1;
        else if (*src == '#') while (src[1] && src[1] != '\n') src++;
    tokpos.col = src - sol + 1;

    if (!*src) return token = Teof;

    char *base = src;
    toknum = strtod(src, &src);
    if (base != src) return token = Tnum;

    // Two-character punctuation.
    for (int i = Teq; i <= Tarrow; i++)
        if (tokname[i][0] == src[0] && tokname[i][1] == src[1])
            return src += 2, token = i;

    // Single-character punctuation.
    for (int i = Tlparen; i <= Tset; i++)
        if (*tokname[i] == *src) return src++, token = i;

    // String.
    if (*src == '\'' || *src == '"') {
        for (char q = *src++; ; )
            if (*src == q)
                return  tokstr = try_unescape(base + 1, src - base - 1),
                        tokstr = tokstr? tokstr: intern(base + 1, src - base - 1),
                        src++,
                        token = Tstring;
            else if (*src == '\n' || !*src) error(tokpos, "unclosed string");
            else if (*src == '\\')
                if (*src == '\n' || !*src) error(tokpos, "unclosed esc");
                else src += 2;
            else src++;
    }

    // Identifier or keyword.
    while (isalnum(*src) || *src == '_') src++;
    if (src == base) error(tokpos, "bad token");
    tokstr = intern(base, src - base);
    for (int i = Tid + 1; tokname[i]; i++)
        if (tokstr->text == tokname[i]) return token = i;
    return token = Tid;
}

static bool peek(toktype t) { return next(), peeked = true, token == t; }
static bool want(toktype t) { return next(), !(peeked = token != t); }
static void need(toktype t) {
    if (!want(t)) error(tokpos, "need %s", tokname[t]);
}

static ast *expr(void);
static ast *stmt(void);
static ast *expr_or_stmt(void);
static ast *csv(toktype form, toktype delim) {
    pos     pos = tokpos;
    ast     **vals = NULL;
    int     n = 0;
    do {
        if (peek(delim)) break;
        vals = realloc(vals, (n + 1) * sizeof *vals);
        vals[n++] = expr();
    } while (want(Tcomma));
    need(delim);
    return ast(form, pos, .list={n, vals});
}
static ast *hash_literal(void) {
    pos             pos = tokpos;
    int             n = 0;
    struct ent_ast  *ents = NULL;
    do {
        if (peek(Trcurly)) break;
        ents = realloc(ents, (n + 1) * sizeof *ents);
        ents[n].key = (need(Tid), tokstr);
        ents[n].val = (need(Tcolon), expr());
        n++;
    } while (want(Tcomma));
    need(Trcurly);
    return ast(Thash, pos, .hash={n, ents});
}
static ast *primary(void) {
    pos     pos = tokpos;
    ast     *c;
    switch (next()) {
    case Tnum:      return ast(Tnum, pos, .num=toknum);
    case Tstring:   return ast(Tstring, pos, .str=tokstr);
    case Ttrue:     return ast(Ttrue, pos);
    case Tfalse:    return ast(Tfalse, pos);
    case Tnull:     return ast(Tnull, pos);
    case Tid:       return ast(Tid, pos, .id=tokstr);
    case Tlparen:   c = csv(Ttuple, Trparen);
                    return c->tuple.n == 1? *c->tuple.vals: c;
    case Tlbrace:   return csv(Tlist, Trbrace);
    case Tlcurly:   return hash_literal();
    default:        return error(pos, "need expression");
    }
}
static ast *suffix(void) {
    ast *lhs = primary();
    ast *x;

    while (true) {
        pos pos = tokpos;
        if (want(Tlbrace)) {
            ast *x = expr();
            need(Trbrace);
            lhs = ast(Tindex, pos, .index={lhs, x});
        }
        else if (want(Tdot)) {
            string *name = (need(Tid), tokstr);
            lhs = ast(Tdot, pos, .dot={lhs, name});
        }
        else if (want(Tlparen)) {
            int     n = 0;
            ast     **args = NULL;
            do {
                if (peek(Trparen)) break;
                args = realloc(args, (n + 1) * sizeof *args);
                args[n++] = expr();
            } while (want(Tcomma));
            need(Trparen);
            lhs = ast(Tcall, pos, .call={n, lhs, args});
        }
        else if (want(Tarrow)) {
            if (lhs->form != Tid && lhs->form != Ttuple)
                bad_arrow: error(pos, "misplaced =>");

            bool    single = lhs->form == Tid;
            int     n = single? 1: lhs->tuple.n;
            ast     **vals = single? &lhs: lhs->tuple.vals;
            string  **params = malloc(n * sizeof *params);

            for (int i = 0; i < n; i++) {
                if (vals[i]->form != Tid) goto bad_arrow;
                params[i] = vals[i]->id;
            }
            ast *body = expr_or_stmt();
            struct proto *proto=new(struct proto, n, params, NULL, body);
            return ast(Tarrow, pos, .proto=proto);
        }
        else return lhs;
    }
}
static ast *prefix(void) {
    pos pos = tokpos;
    if (want(Tsub)) return ast(Tneg, pos, .uni=prefix());
    else if (want(Tnot)) return ast(Tnot, pos, .uni=prefix());
    else if (want(Tprint)) return ast(Tprint, pos, .uni=prefix());
    else return suffix();
}
static ast *infix(int prec1, bool leftassoc1, ast *e1) {
    int     prec2 = (peek(0), precedence[token]);
    bool    leftassoc2 = !right_assoc[token];
    if (!prec2 || prec1 > prec2 || (prec1 == prec2 && leftassoc1))
        return e1;
    else {
        pos pos = tokpos;
        toktype form = next();
        ast     *e2 = infix(prec2, leftassoc2, prefix());
        return infix(prec1, leftassoc1, ast(form, pos, .bin={e1, e2}));
    }
}
static ast *expr(void) {
    ast *a = infix(-1, false, prefix());
    if (want(Tquest)) {
        pos pos = tokpos;
        ast *b = expr();
        ast *c = (need(Tcolon), expr());
        return ast(Tquest, pos, .cond={a, b, c});
    } else return a;
}
static ast *expr_or_stmt(void) {
    return peek(Tlcurly)? stmt(): expr();
}
static ast *stmt(void) {
    pos pos = tokpos;
    if (want(Tlcurly)) {
        int     n = 0;
        ast     **stmts = NULL;
        while (!want(Trcurly))
            stmts = realloc(stmts, (n + 1) * sizeof *stmts),
            stmts[n++] = stmt();
        if (!n) return ast(Tnull, pos);
        return ast(Tblock, pos, .block={n, stmts});
    }
    else if (want(Tif)) {
        ast     *a = (need(Tlparen), expr());
        ast     *b = (need(Trparen), stmt());
        ast     *c = want(Telse)? stmt(): ast(Tnull, tokpos);
        return ast(Tif, pos, .cond={a, b, c});
    } else if (want(Twhile)) {
        ast     *cond = (need(Tlparen), expr());
        ast     *body = (need(Trparen), stmt());
        return ast(Twhile, pos, .loop={cond, body});
    } else if (want(Treturn)) {
        ast     *ret = peek(Tsemi)? ast(Tnull, pos): expr();
        need(Tsemi);
        return ast(Treturn, pos, .ret=ret);
    } else if (want(Tvar)) {
        string  *name = (need(Tid), tokstr);
        ast     *val = (need(Tset), expr());
        need(Tsemi);
        return ast(Tvar, pos, .var={name, val});
    } else if (want(Tdef)) {
        string  *name = (need(Tid), tokstr);
        string  **params = NULL;
        int     n = 0;
        need(Tlparen);
        do {
            if (peek(Trparen)) break;
            params = realloc(params, (n + 1) * sizeof *params);
            params[n++] = (need(Tid), tokstr);
        } while (want(Tcomma));
        need(Trparen);
        ast             *body = expr_or_stmt();
        struct proto    *proto = new(struct proto,
                                     .n=n,
                                     .name=name,
                                     .params=params,
                                     .body=body);
        ast             *fn = ast(Tarrow, pos, .proto=proto);
        return ast(Tvar, pos, .var={name, fn});
    } else if (want(Tsemi)) {
        return ast(Tblock, pos, .block={.n=0, .stmts=NULL});
    } else {
        ast     *c = expr();
        need(Tsemi);
        return c;
    }
}

static bool truth(value x) {
    return x.type != Null && (x.type != Bool || x.boole);
}
static bool string_equal(string *x, string *y) {
    if (x == y) return true;
    if (x->hash != y->hash) return false;
    if (x->len != y->len) return false;
    return !memcmp(x->text, y->text, x->len);
}
static bool equal(value x, value y) {
    if (x.type != y.type) return false;
    switch (x.type) {
    case Null:      return true;
    case Bool:      return x.boole == y.boole;
    case Num:       return x.num == y.num;
    case String:    return string_equal(x.str, y.str);
    case Tuple:     if (x.tuple == y.tuple) return true;
                    if (x.tuple->n != y.tuple->n) return false;
                    for (int i = 0; i < x.tuple->n; i++)
                        if (!equal(x.tuple->vals[i], y.tuple->vals[i]))
                            return false;
                    return true;
    case List:      return x.list == y.list;
    case Object:    return x.obj == y.obj;
    case Hash:      return x.hash == y.hash;
    case Fn:        return x.fn == y.fn;
    }
    return false;
}
static value join_strings(pos pos, value *vals, int n) {
    int total = 0;
    for (int i = 0; i < n; i++)
        if (vals[i].type != String) error(pos, "non-string: %v", vals[i]);
        else total += vals[i].str->len;

    string *out = newstr(NULL, total);
    char *p = out->text;
    for (int i = 0; i < n; i++)
        memcpy(p, vals[i].str->text, vals[i].str->len),
        p += vals[i].str->len;

    out->hash = hash_string(out->text, out->len);
    return thestr(out);
}
static struct entry *hash_find(struct hash *hash, string *key, bool or_create) {
    unsigned i, h = key->hash;
    struct entry *e = hash->ents;

    i = h;
    while ((e = hash->ents + i % hash->cap)->key) {
        if (string_equal(e->key, key))
            return e;
        i = (5 * i) + 1 + h;
        h /= 32;
    }

    if (or_create) {
        if (hash->n > hash->cap * 3 / 4) {
            /*
                Resize over-full hash table by creating another.
                Then retry adding this current key.
             */
            struct hash old = *hash;
            hash->cap *= 2;
            hash->n = 0;
            hash->order = malloc(hash->cap * sizeof *hash->order);
            hash->ents = calloc(1, hash->cap * sizeof *hash->ents);
            for (int i = 0; i < old.n; i++) {
                struct entry e = old.ents[old.order[i]];
                hash_find(hash, e.key, true)->val = e.val;
            }
            return hash_find(hash, key, true);
        }
        e->key = key;
        hash->order[hash->n++] = e - hash->ents;
        return e;
    }
    return NULL;
}
static value hash_set(struct hash *hash, string *key, value val) {
    return hash_find(hash, key, true)->val = val;
}
static value hash_get(struct hash *hash, string *key) {
    struct entry *ent = hash_find(hash, key, false);
    return ent? ent->val: _null;
}
static struct hash *get_class(value x) {
    if (x.type == Object) {
        value c = hash_get(x.hash, class_id);
        if (c.type == Object) return c.hash;
    }
    return classes[x.type];
}
static value get_index(pos pos, value lhs, value index) {
    if (lhs.type == Hash) {
        if (index.type == Num) {
            int i = (int) index.num;
            if (i < 0) i += lhs.hash->n;
            if (i < 0 || i >= lhs.hash->n) error(pos, "out of bounds: %v", index);
            return lhs.hash->ents[lhs.hash->order[i]].val;
        }
        if (index.type != String) error(pos, "index not string: %v", index);
        return hash_get(lhs.hash, index.str);
    }
    else if (lhs.type == List) {
        if (index.type != Num) error(pos, "index not number: %v", index);
        int i = (int) index.num;
        if (i < 0) i += lhs.list->n;
        if (i < 0 || i >= lhs.list->n) error(pos, "out of bounds: %v", index);
        return lhs.list->vals[i];
    }
    else if (lhs.type == Tuple) {
        if (index.type != Num) error(pos, "index not number: %v", index);
        int i = (int) index.num;
        if (i < 0) i += lhs.tuple->n;
        if (i < 0 || i >= lhs.tuple->n) error(pos, "out of bounds: %v", index);
        return lhs.tuple->vals[i];
    }
    else if (lhs.type == String) {
        if (index.type != Num) error(pos, "index not number: %v", index);
        int i = (int) index.num;
        if (i < 0) i += lhs.str->len;
        if (i < 0 || i >= lhs.str->len) error(pos, "out of bounds: %v", index);
        return thestr(characters[lhs.str->text[i]]);
    }
    else return error(pos, "not indexable: %v", lhs), _null;
}
static value set_index(pos pos, value lhs, value index, value val) {
    if (lhs.type == Hash) {
        if (index.type == Num) {
            int i = (int) index.num;
            if (i < 0) i += lhs.hash->n;
            if (i < 0 || i >= lhs.hash->n) error(pos, "out of bounds: %v", index);
            return lhs.hash->ents[lhs.hash->order[i]].val;
        }
        if (index.type != String) error(pos, "index not string: %v", index);
        return hash_set(lhs.hash, index.str, val), val;
    }
    else if (lhs.type == List) {
        if (index.type != Num) error(pos, "index not number: %v", index);
        int i = (int) index.num;
        if (i < 0) i += lhs.list->n;
        if (i < 0 || i >= lhs.list->n) error(pos, "out of bounds: %v", index);
        return lhs.list->vals[i] = val;
    }
    else return error(pos, "cannot assign to: %v[%v]", lhs, index), _null;
}


static int      *program;
static int      pcode_cap;
static int      npcode;
static value    *constants;
static int      nconstants;
static int      constants_cap;
enum pcode {
    Phalt,
    Pnull,
    Plit,
    Ptrue,
    Pfalse,
    Ptuple,
    Plist,
    Phash,
    Phitem,
    Pget,
    Pset,
    Pvar,
    Pdrop,
    Pprint,
};

static int emit(int x) {
    if (npcode >= pcode_cap)
        pcode_cap = (pcode_cap? pcode_cap: 128) * 2,
        program = realloc(program, pcode_cap * sizeof *program);
    program[npcode] = x;
    return npcode++;
}
static int emit2(int x, int y) { return emit(x), emit(y); }

static int constant(value x) {
    for (int i = 0; i < nconstants; i++)
        if (equal(constants[i], x)) return i;
    if (nconstants + 1 >= constants_cap)
        constants_cap = (constants_cap? constants_cap: 128) * 2,
        constants = realloc(constants, constants_cap * sizeof *constants);
    constants[nconstants] = x;
    return nconstants++;
}


static void compile(ast *c) {
    switch (c->form) {
    case Tnum:      emit2(Plit, constant(thenum(c->num))); break;
    case Tstring:   emit2(Plit, constant(thestr(c->str))); break;
    case Ttrue:     emit(Ptrue); break;
    case Tfalse:    emit(Pfalse); break;
    case Tnull:     emit(Pnull); break;
    case Ttuple:    if (c->tuple.n == 0)
                        emit2(Plit, constant(_unit));
                    else {
                        for (int i = 0; i < c->tuple.n; i++)
                            compile(c->tuple.vals[i]);
                        emit2(Ptuple, c->tuple.n);
                    }
                    break;
    case Tlist:     for (int i = 0; i < c->list.n; i++)
                        compile(c->list.vals[i]);
                    emit2(Plist, c->list.n); break;
    case Thash:     emit2(Phash, c->hash.n);
                    for (int i = 0; i < c->hash.n; i++)
                        compile(c->hash.ents[i].val),
                        emit2(Phitem, constant(thestr(c->hash.ents[i].key)));
                    break;
    case Tid:       emit2(Pget, constant(thestr(c->id))); break;
    case Tvar:      compile(c->var.val);
                    emit2(Pvar, constant(thestr(c->var.name)));
                    break;
    case Tblock:    for (int i = 0; i < c->block.n; i++) {
                        compile(c->block.stmts[i]);
                        emit(Pdrop);
                    }
                    break;
    case Tprint:    compile(c->uni); emit(Pprint); break;
    default:        error(c->pos, "cannot compile: %s", tokname[c->form]);
    }
}

static struct entry *rec_hash_find(struct hash *hash, string *id) {
    struct entry *entry = hash_find(hash, id, false);
    if (!entry) {
        struct entry *parent_entry = hash_find(hash, nonlocal_id, false);
        if (parent_entry && parent_entry->val.type == Hash)
            entry = rec_hash_find(parent_entry->val.hash, id);
    }
    return entry;
}

static value execute(struct hash *env) {
    value       *stack = malloc(STACK_SIZE * sizeof *stack);
    value       *sp = stack - 1;
    int         *pc = program;
    int         n;
    value       r;
    struct entry *var;

    #define J(X) case P##X: goto P##X;
    #define ALL J(halt) J(lit) J(null) J(true) J(false) J(tuple)\
                J(list) J(hash) J(hitem) J(get) J(set) J(var) J(drop)\
                J(print)
    #define next switch((enum pcode) *pc++) { ALL }
    #define push(X) *++sp = X
    #define pop() *sp--
    #define xchg(N, X) *(sp -= N - 1) = X
    #define T sp[0]
    #define T1 sp[-1]
    #define T2 sp[-2]

    next;
    Phalt:      return *sp;
    Plit:       push(constants[*pc++]); next;
    Pnull:      push(_null); next;
    Ptrue:      push(_true); next;
    Pfalse:     push(_false); next;
    Ptuple:     n = *pc++; r = thetuple(newtuple(sp - n + 1, n)); xchg(n, r); next;
    Plist:      n = *pc++; r = thelist(newlist(sp - n + 1, n, n)); xchg(n, r); next;
    Phash:      push(thehash(newhash(*pc++))); next;
    Phitem:     hash_set(T1.hash, constants[*pc++].str, T); pop(); next;
    Pget:       var = rec_hash_find(env, constants[*pc++].str);
                push(var? var->val: _null);
                next;
    Pset:       var = rec_hash_find(env, constants[*pc++].str);
                if (var) var->val = T;
                pop();
                next;
    Pvar:       hash_set(env, constants[*pc++].str, pop()); next;
    Pindex:     r = get_index(T1, T); xchg(2, r); next;
    Pdrop:      pop(); next;
    Pprint:     print_value(T, false); putchar('\n'); next;
}

static void init_strings(void) {
    for (int i = 0; i < 256; i++)
        characters[i] = intern((char[]){i}, 1);
    nonlocal_id = intern("_nonlocal_", 10);
    class_id = intern("_class_", 7);
    name_id = intern("name", 4);
    for (int i = 0; tokname[i]; i++)
        tokname[i] = intern(tokname[i], strlen(tokname[i]))->text;
}
static void init_classes(void) {
    for (int i = 0; i < sizeof classnames / sizeof *classnames; i++) {
        string *name = intern(classnames[i], strlen(classnames[i]));
        classnames[i] = name->text;
        classes[i] = newobj();
        hash_set(classes[i], name_id, thestr(name));
    }
    for (int i = 0; i < sizeof classnames / sizeof *classnames; i++)
        hash_set(classes[i], class_id, theobj(classes[i]));
}
int main(int argc, char **argv) {
    init_strings();
    init_classes();
    _unit = thetuple(newtuple(NULL, 0));

    if (!open_source(argv[1]))
        error(tokpos, "cannot open source");

    struct hash *env = newhash(0);
    ast *e = stmt();
    need(Teof);
    compile(e);
    emit(Phalt);
    print("> %v\n", execute(env));
    puts("done.");
}
