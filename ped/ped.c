#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define new(t,...) memcpy(malloc(sizeof(t)), &(t){__VA_ARGS__}, sizeof(t))
#define ast(t,...) new(ast, t, srcpos, __VA_ARGS__)

typedef struct string {
    int         len;
    unsigned    hash;
    char        text[];
} string;

typedef struct pos { string *fn; int ln; } pos;

typedef enum type { NIL, INT, STR, ARR, OBJ, FUN, SUBR } type;

char *types[] = {
    "nil", "int", "string", "array",
    "object", "fun", "subr", 0
};

typedef struct value {
    type type;
    union {
        int         n;
        string      *str;
        struct arr  *arr;
        struct map  *map, *obj;
        struct fun  *fun;
        struct value (*subr)(int n, struct value *args);
        intptr_t    ptr;
    };
} value;

typedef struct frame {
    struct frame    *fp;
    int             pc;
    value           vals[];
} frame;

struct fun {
    int     nlocals;
    int     nparams;
    int     pc;
    frame   *fp;
    string  *id;
};

struct arr {
    int             len;
    int             cap;
    struct value    *vals;
};

struct entry {
    value   key;
    value   val;
};

struct map {
    int             len;
    int             used;
    int             cap;
    struct entry    *entries;
    int             *order;
};

typedef struct ast {
    int         op;
    pos         pos;
    value       val;
    struct ast  *lhs;
    struct ast  *rhs;
    struct ast  *next;
} ast;

// Keep tokens and names in sync.
// The name arr must end in NULL.
// Longer tokens with the same prefix must be first (e.g. '=' vs '==')
typedef enum tok {
    TEOL, TINT, TSTR, TLP, TRP, TLB, TRB, TLC, TRC, TCOMMA,
    TDOT, TMUL, TDIV, TREM, TADD, TSUB, TEQ, TNE, TBANG, TLT,
    TGT, TLE, TGE, TAND, TOR, TQ, TCOL, TASN, TID, TEND, TIF,
    TELSE, TWHILE, TFUN, TRET, TVAR, TDO, TNIL, TTRUE, TFALSE,
    TCLASS,
} tok;
char *tokens[] = {
    "e.o.l.", "int", "string", "(", ")", "[", "]", "{", "}",
    ",", ".", "*", "/", "%", "+", "-", "==", "!=", "!", "<=",
    ">=", "<", ">", "&&", "||", "?", ":", "=", "id", "end",
    "if", "else", "while", "fun", "return", "var", "do", "nil",
    "true", "false", "class", 0
};

typedef struct symbol {
    string *id;
} symbol;

typedef struct scope {
    int     n;
    symbol  *symbols;
    struct scope *next;
} scope;

typedef enum opcode {
    MHLT,
    MNIL,
    MLOAD,
    MARR,
    MFUN,
    MRET,
    MCALL,
    MPOP,
    MBRA,
    MGET,
    MFGET,
    MSET,
    MFSET,
    // MMAP,
    MSAGG,
    MGAGG,
    MSDOT,
    MGDOT,
    MMDOT,
    MCLSS,
} opcode;

FILE        *srcfile;
pos         srcpos;
char        linebuf[65536];
char        tbuf[sizeof linebuf];
char        *src;
tok         token;
bool        peeked;
string      *tstr;
int         tint;
string      **interns;
int         ninterns;
value       *constants;
int         nconstants;
uint8_t     *program;
int         szprogram;
int         vpc;
uint8_t     *pc;
value       stack[65536];
value       *sp = stack - 1;
frame       *fp;
value       ra;      // register (use this as a temporary in vm)
value       chars[256];
value       nil;
value       anon;
value       namestr;
value       thisstr;
value       object_class;
value       map_class;
value       classes[SUBR + 1];

ast *expr(void);
ast *compound(tok alt_end);
bool equal(value x, value y);
void compile(ast *e, scope *env);

static inline unsigned hashstr(char *text, int len, char *output) {
    unsigned hash = 5381;
    if (output)
        for (int i = 0; i < len; i++) {
            hash = hash * 33 + (unsigned char) text[i];
            output[i] = text[i];
        }
    else
        for (int i = 0; i < len; i++)
            hash = hash * 33 + (unsigned char) text[i];
    return hash;
}

string *newstr(char *text, int len) {
    if (len < 0)
        len = strlen(text);
    string *str = malloc(sizeof *str + len + 1);
    str->len = len;

    if (text)
        str->hash = hashstr(text, len, str->text);

    str->text[len] = 0;
    return str;
}

string *intern(char *text, int len) {
    if (len < 0) len = strlen(text);
    for (int i = 0; i < ninterns; i++)
        if (interns[i]->len == len && !memcmp(interns[i]->text, text, len))
            return interns[i];
    interns = realloc(interns, ++ninterns * sizeof *interns);
    return interns[ninterns - 1] = newstr(text, len);
}


bool isnil(value x) { return x.type == NIL; }
bool isint(value x) { return x.type == INT; }
bool isstr(value x) { return x.type == STR; }
bool isarr(value x) { return x.type == ARR; }
bool isobj(value x) { return x.type == OBJ; }
bool isfun(value x) { return x.type == FUN; }
bool issubr(value x) { return x.type == SUBR; }

int asint(value x) { return isint(x)? x.n: 0; }
string *asstr(value x) { return isstr(x)? x.str: 0; }
struct map *asobj(value x) { return isobj(x)? x.obj: 0; }
struct arr *asarr(value x) { return isarr(x)? x.arr: 0; }
struct fun *asfun(value x) { return isfun(x)? x.fun: 0; }
value (*assubr(value x))(int, value*) { return issubr(x)? x.subr: 0; }

value integer(int n) {
    return (value) { INT, .n=n };
}

value str(string *str) {
    return (value) { STR, .str=str };
}

value newarray(int cap, value *vals) {
    struct arr *a = new(struct arr,
                        .len=(vals? cap: 0),
                        .cap=cap,
                        .vals=malloc(cap * sizeof *vals));
    if (vals)
        memcpy(a->vals, vals, cap * sizeof *vals);
    return (value) { ARR, .arr=a };
}

struct map *newmap(int cap) {
    if (cap < 8)
        cap = 8;
    cap = cap + cap * 3 / 4; // Fill threshhold
    struct map *map = new(struct map,
                          .cap=cap,
                          .entries=malloc(cap * sizeof (struct entry)),
                          .order=malloc(cap * sizeof (int)));
    memset(map->entries, 0, cap * sizeof (struct entry));
    return map;
}

value newobj(int cap) {
    return (value) { OBJ, .obj=newmap(cap) };
}

value fun(string *id, int nlocals, int nparams, int pc, frame *fp) {
    struct fun *f = new(struct fun, .nlocals=nlocals, .nparams=nparams,
                        .pc=pc, .id=id, .fp=fp);
    return (value) { FUN, .fun=f };
}

value subr(value f(int n, value *as)) {
    return (value) { SUBR, .subr=f };
}

int length(value x) {
    return isobj(x)? asobj(x)->len:
           isarr(x)? asarr(x)->len:
           isstr(x)? asstr(x)->len:
           0;
}

void arr_insert(struct arr *arr, int i, value x) {
    if (i < 0) i += arr->len + 1;
    if (i < 0 || i > arr->len) return;

    if (arr->len + 1 >= arr->cap) {
        arr->cap *= 2;
        if (arr->cap <= 0)
            arr->cap = 8;
        arr->vals = realloc(arr->vals, arr->cap * sizeof *arr->vals);
    }
    memmove(arr->vals + i + 1,
            arr->vals + i,
            (arr->len - i) * sizeof *arr->vals);
    arr->vals[i] = x;
    arr->len++;
}

struct entry *map_place(struct map *map, value key, bool define) {
    if (isnil(key)) {
        static struct entry ignore;
        return &ignore;
    }

    // Find position in hash table.
    unsigned hash = isstr(key)? asstr(key)->hash:
                    isint(key)? (unsigned) key.n:
                    (unsigned) key.ptr;
    struct entry *entries = map->entries;
    unsigned i = hash % map->cap;
    while (!isnil(entries[i].key)) {
        if (equal(entries[i].key, key))
            return entries + i;
        i = (5 * i + 1 + hash) % map->cap;
        hash /= 32;
    }

    if (define) {
        // Rebuild crowded table
        if (map->used > 3 * map->cap / 4) {
            struct map *new = newmap(map->cap * 2);
            for (int i = 0; i < map->len; i++) {
                struct entry *e = map->entries + map->order[i];
                map_place(new, e->key, true)->val = e->val;
            }
            *map = *new;
            return map_place(map, key, define);
        }

        // Defining new entry
        map->len++;
        map->used++;
        map->order[map->len - 1] = i;
        map->entries[i].key = key;
    }

    return entries + i;
}

value int_index(value agg, int i) {
    if (isobj(agg)) {
        struct map *map = asobj(agg);
        if (i < 0) i += map->len;
        if (i < 0 || i >= map->len) return nil;
        return map->entries[map->order[i]].key;
    }
    else if (isarr(agg)) {
        struct arr *arr = asarr(agg);
        if (i < 0) i += arr->len;
        if (i < 0 || i >= arr->len) return nil;
        return arr->vals[i];
    }
    else if (isstr(agg)) {
        string *str = asstr(agg);
        if (i < 0) i += str->len;
        if (i < 0 || i >= str->len) return nil;
        return chars[(unsigned) str->text[i]];
    }
    return nil;
}

value get_index(value agg, value index) {
    if (isobj(agg))
        return map_place(asobj(agg), index, false)->val;
    if (isarr(agg) && isint(index))
        return int_index(agg, index.n);
    if (isstr(agg) && isint(index))
        return int_index(agg, index.n);
    return nil;
}

value set_index(value agg, value index, value val) {
    if (isobj(agg)) {
        struct entry *ent = map_place(asobj(agg), index, true);
        return ent->val = val;
    }
    if (isarr(agg) && isint(index)) {
        struct arr *arr = asarr(agg);
        int i = index.n;
        if (i < 0) i += arr->len;
        if (i < 0 || i >= arr->len) return nil;
        return arr->vals[i] = val;
    }
    return val;
}

bool equal(value x, value y) {
    if (x.type != y.type)
        return false;

    switch (x.type) {
    case NIL:       return true;
    case INT:       return asint(x) == asint(y);

    case STR:       return asstr(x) == asstr(y) ||
                        (length(x) == length(y) &&
                         asstr(x)->hash == asstr(y)->hash &&
                         !memcmp(asstr(x)->text, asstr(y)->text, length(x)));

    case ARR:       if (asarr(x) == asarr(y))
                        return true;
                    if (asarr(x)->len != asarr(y)->len)
                        return false;
                    for (int i = 0; i < asarr(x)->len; i++)
                        if (!equal(asarr(x)->vals[i], asarr(y)->vals[i]))
                            return false;
                    return true;

    case OBJ:       if (asobj(x) == asobj(y)) return true;
                    if (length(x) != length(y)) return false;
                    for (int i = 0; i < length(x); i++) {
                        value a = int_index(x, i);
                        value b = int_index(y, i);
                        if (!equal(a, b))
                            return false;
                        if (!equal(get_index(x, a), get_index(y, b)))
                            return false;
                    }
                    return true;

    case FUN:       return asfun(x) == asfun(y);
    case SUBR:      return assubr(x) == assubr(y);
    }
    return false;
}

value get_class(value x) {
    return isobj(x)? x: classes[x.type];
}

void print_value(value x) {
    switch (x.type) {
    case NIL:       fputs("nil", stdout); break;
    case INT:       printf("%d", x.n); break;
    case STR:       fwrite(x.str->text, 1, x.str->len, stdout); break;
    case ARR:       putchar('[');
                    for (int i = 0; i < length(x); i++) {
                        if (i) fputs(", ", stdout);
                        print_value(int_index(x, i));
                    }
                    putchar(']');
                    break;
    case OBJ:       print_value(get_index(x, namestr)); break;
    case FUN:       fputs(asfun(x)->id->text, stdout); break;
    case SUBR:      fputs("(SUBR)", stdout); break;
    }
}

void *syntax(char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    printf("ped: error %s:%d: ", srcpos.fn->text, srcpos.ln);
    vprintf(msg, ap);
    putchar('\n');
    exit(1);
}

void opensrc(char *filename) {
    srcpos = (pos) { intern(filename, -1), 0 };
    srcfile = fopen(filename, "rb");
    if (!srcfile) syntax("cannot open");
}

bool nextline(void) {
    int len = 0;
    peeked = false;
    while (fgets(linebuf + len, sizeof linebuf - len, srcfile)) {
        srcpos.ln++;
        len += strlen(linebuf + len);

        // Trim left and right
        while (len > 0 && isspace(linebuf[len - 1])) len--;
        linebuf[len] = 0;
        for (src = linebuf; isspace(*src); src++);

        if (len > 0 && linebuf[len - 1] == '\\') // Continuation
            linebuf[len - 1] = ' ';
        else if (!*src || *src == '#')             // Empty line
            len = 0;
        else
            return true;
    }
    return false;
}

tok next(void) {
    if (peeked)
        return peeked = false, token;

    while (isspace(*src)) src++;

    if (!*src || *src == '#')
        return token = 0;
    if (isdigit(*src)) {
        tint = strtol(src, &src, 10);
        return token = TINT;
    }
    for (int i = TLP; i < TID; i++)
        if (tokens[i][1] && *src == *tokens[i] && src[1] == tokens[i][1])
            return src += 2, token = i;
        else if (!tokens[i][1] && *src == *tokens[i])
            return src++, token = i;
    if (*src == '"' || *src == '\'') {
        for (char q = *src++, *out = tbuf; ; src++)
            if (*src == q) {
                src++;
                tstr = intern(tbuf, out - tbuf);
                return token = TSTR;
            } else if (!*src)
                syntax("unclosed string");
            else if (*src != '\\')
                *out++ = *src;
            else switch (*++src) {
            case 'a': *out++ = '\a'; break;
            case 'b': *out++ = '\b'; break;
            case 'n': *out++ = '\n'; break;
            case 'r': *out++ = '\r'; break;
            case 't': *out++ = '\t'; break;
            default: *out++ = *src;
            }
    }

    char *org = src;
    while (isalnum(*src) || *src == '_') src++;
    if (org == src)
        syntax("not a token: %c", *src);
    tstr = intern(org, src - org);
    for (int i = TID + 1; tokens[i]; i++)
        if (tokens[i] == tstr->text)
            return token = i;
    return token = TID;
}

bool peek(tok tok) {
    peeked = next();
    return token == tok;
}

bool want(tok tok) {
    peeked = next() != tok;
    return !peeked;
}

void need(tok tok) {
    if (!want(tok))
        syntax("expected %s", tokens[tok]);
}

ast *need_id(void) {
    need(TID);
    return ast(TID, .val=str(tstr));
}

void *after(void *x, tok suffix) {
    need(suffix);
    return x;
}

ast *csv(ast *item(void), tok end) {
    ast *list = 0;
    ast **ptr = &list;
    do {
        if (peek(end))
            break;
        *ptr = item();
        ptr = &(*ptr)->next;
    } while (want(TCOMMA));
    need(end);
    return list;
}

ast *fun_block(void) {
    if (peek(TEOL))
        return compound(TEND);
    ast *result = expr();
    return ast(TRET, .lhs=result);
}

ast *entry(void) {
    ast *lhs = want(TID)? ast(TSTR, .val=str(tstr)):
               want(TSTR)? ast(TSTR, .val=str(tstr)):
               want(TINT)? ast(TINT, .val=integer(tint)):
               syntax("literal key must be ID, string, or int");
    need(TCOL);
    ast *rhs = expr();
    return ast(0, .lhs=lhs, .rhs=rhs);
}

ast *class_body(void) {
    ast *body = 0;
    ast **ptr = &body;

    while (true) {
        if (!nextline())
            syntax("class not closed");
        if (want(TEND))
            break;

        if (want(TVAR)) {
            ast *lhs = after(need_id(), TASN);
            ast *rhs = expr();
            *ptr = ast(TVAR, .lhs=lhs, .rhs=rhs);
        }
        else if (want(TFUN)) {
            value id = want(TID)? str(tstr): anon;
            ast *params = (need(TLP), csv(need_id, TRP));
            ast *body = fun_block();
            params = ast(TID, .val=thisstr, .next=params);
            *ptr = ast(TFUN, .val=id, .lhs=params, .rhs=body);
        }
        else
            syntax("expression not allowed in class definition");

        ptr = &(*ptr)->next;
    }
    return body;
}

ast *primary(void) {
    if (want(TNIL))     return ast(TNIL);
    if (want(TTRUE))    return ast(TTRUE);
    if (want(TFALSE))   return ast(TFALSE);
    if (want(TINT))     return ast(TINT, .val=integer(tint));
    if (want(TSTR))     return ast(TSTR, .val=str(tstr));
    if (want(TID))      return ast(TID, .val=str(tstr));
    if (want(TLP))      return after(expr(), TRP);
    if (want(TLB))      return ast(TLB, .lhs=csv(expr, TRB));
    if (want(TLC))      return ast(TLC, .lhs=csv(entry, TRC));
    if (want(TFUN)) {
        value id = want(TID)? str(tstr): anon;
        ast *params = (need(TLP), csv(need_id, TRP));
        ast *body = fun_block();
        return ast(TFUN, .val=id, .lhs=params, .rhs=body);
    }
    if (want(TRET))     return ast(TRET, .lhs=expr());
    if (want(TVAR)) {
        ast *lhs = after(need_id(), TASN);
        ast *rhs = expr();
        return ast(TVAR, .lhs=lhs, .rhs=rhs);
    }
    if (want(TCLASS)) {
        value id = (need(TID), str(tstr));
        ast *body = class_body();
        return ast(TCLASS, .val=id, .lhs=body);
    }
    return syntax("need expression");
}

ast *suffix(void) {
    ast *lhs = primary();
    while (true)
        if (want(TDOT)) {                              // Member
            need(TID);
            lhs = ast(TDOT, .val=str(tstr), .lhs=lhs);
        }
        else if (want(TLB)) {                           // Index
            ast *rhs = after(expr(), TRB);
            lhs = ast(TRB, .lhs=lhs, .rhs=rhs);
        }
        else if (want(TLP)) {                            // Call
            ast *args = csv(expr, TRP);
            if (want(TDO)) {                     // Append block
                ast *params = (need(TLP), csv(need_id, TRP));
                ast *body = fun_block();
                ast *fun = ast(TFUN, .val=anon, .lhs=params, .rhs=body);
                ast **ptr = &args;
                while (*ptr) ptr = &(*ptr)->next;
                *ptr = fun;
            }
            lhs = ast(TRP, .lhs=lhs, .rhs=args);
        }
        else
            return lhs;
}

ast *prefix(void) {
    if (want(TBANG)) {
        ast *lhs = prefix();
        return ast(TBANG, .lhs=lhs);
    }
    else if (want(TSUB)) {
        ast *lhs = ast(TINT, .val=integer(0));
        ast *rhs = prefix();
        return ast(TSUB, .lhs=lhs, .rhs=rhs);
    }
    else
        return suffix();
}

ast *binop(tok low, tok high, ast *left(void), ast *right(void)) {
    ast *lhs = left();
    while (peek(0), low <= token && token <= high) {
        tok op = next();
        ast *rhs = right();
        lhs = ast(op, .lhs=lhs, .rhs=rhs);
    }
    return lhs;
}

ast *mulop(void) {
    return binop(TMUL, TREM, prefix, prefix);
}

ast *addop(void) {
    return binop(TADD, TSUB, mulop, mulop);
}

ast *relop(void) {
    return binop(TEQ, TGT, addop, addop);
}

ast *andop(void) {
    return binop(TAND, TAND, relop, relop);
}

ast *orop(void) {
    return binop(TOR, TOR, andop, andop);
}

ast *condop(void) {
    ast *lhs = orop();
    if (want(TQ)) {
        ast *branches = after(orop(), TCOL);
        ast *alt = condop();
        branches->next = alt;
        return ast(TQ, .lhs=lhs, .rhs=branches);
    }
    return lhs;
}

ast *expr(void) {
    ast *lhs = condop();
    if (want(TASN)) {
        ast *rhs = expr();
        return ast(TASN, .lhs=lhs, .rhs=rhs);
    }
    return lhs;
}

ast *compound(tok alt_end) { // alt_end ends the block but is not consumed
    if (!want(TEOL))
        return expr();
    ast *exprs = 0;
    ast **ptr = &exprs;
    while (true) {
        if (!nextline())
            syntax("unclosed compound");
        if (want(TEND) || peek(alt_end))
            break;
        *ptr = expr();
        ptr = &(*ptr)->next;
    }
    if (!exprs)
        exprs = ast(TNIL);
    return ast(TEND, .lhs=exprs);
}

int read2(uint8_t *bytes) {
    return ((bytes[0] << 8) + bytes[1]);
}

void write2(uint8_t *bytes, int x) {
    bytes[0] = x >> 8;
    bytes[1] = x;
}

int emit_byte(uint8_t byte) {
    if (vpc == szprogram || (szprogram & 0x1000) == 0) {
        szprogram += 0x1000;
        program = realloc(program, szprogram * sizeof *program);
    }
    program[vpc] = byte;
    return vpc++;
}

int emit2(int x) {
    emit_byte(0);
    emit_byte(0);
    write2(program + vpc - 2, x);
    return vpc - 2;
}

int emit(uint8_t byte, int x) {
    emit_byte(byte);
    emit2(x);
    return vpc - 2;
}

// Patch chain of jumps to target_pc.
// The chain is stored in the values at program[pc].
void patch(int pc, int target_pc) {
    for (int next = pc; next; pc = next) {
        next = read2(program + pc);
        write2(program + pc, target_pc);
    }
}

int store_const(value x) {
    for (int i = 0; i < nconstants; i++)
        if (equal(x, constants[i]))
            return i;
    constants = realloc(constants, ++nconstants * sizeof *constants);
    constants[nconstants - 1] = x;
    return nconstants - 1;
}

void semantic(ast *e, char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    printf("ped: error %s:%d: ", e->pos.fn->text, e->pos.ln);
    vprintf(msg, ap);
    putchar('\n');
    exit(1);
}

int count_ast(ast *e) {
    int len;
    for (len = 0; e; e = e->next) len++;
    return len;
}

void define(scope *env, string *id) {
    env->symbols = realloc(env->symbols, ++env->n * sizeof *env->symbols);
    env->symbols[env->n - 1] = (symbol) { id };
}

symbol *find(scope *env, string *id, int *levelp, int *indexp) {
    *levelp = 0;
    *indexp = 0;
    for (scope *sco = env; sco; sco = sco->next) {
        *indexp = 0;
        for (int i = 0; i < sco->n; i++, ++*indexp)
            if (sco->symbols[i].id == id)
                return sco->symbols + i;

        ++*levelp;
    }
    return 0;
}

void compile_call(ast *lhs, string *method, ast *args, scope *env) {
    int n = 0;
    int is_method = method? 1: 0;

    compile(lhs, env);

    if (is_method)
        emit(MMDOT, store_const(str(method)));

    for (ast *i = args; i; i = i->next) {
        compile(i, env);
        n++;
    }
    emit(MCALL, n + is_method);
}

void compile_fun(string *id, ast *params, ast *body, scope *env) {
    scope   *inner = new(scope, 0, 0, env);
    int     nparams = 0;
    for (ast *i = params; i; i = i->next) {
        define(inner, i->val.str);
        nparams++;
    }

    value f = fun(id, 0, nparams, 0, 0);
    emit(MFUN, store_const(f));                // Make prototype
    if (id != anon.str)                   // Set var if not anon
        emit(MSET, env->n - 1);

    int br = emit(MBRA, 0);                         // Skip body
    f.fun->pc = vpc;
    compile(body, inner);                           // Body code
    emit_byte(MPOP);
    emit_byte(MNIL);                                 // Auto nil
    emit_byte(MRET);
    patch(br, vpc);                            // After the body
    f.fun->nlocals += inner->n;                // Reserve locals
}

void compile_class(string *id, ast *body, scope *env) {
    emit(MLOAD, store_const(namestr));
    emit(MLOAD, store_const(str(id)));

    for (ast *e = body; e; e = e->next)
        if (e->op == TVAR) {
            emit(MLOAD, store_const(e->lhs->val));
            compile(e->rhs, env);
        }
        else if (e->op == TFUN) {
            emit(MLOAD, store_const(e->val));
            compile_fun(asstr(e->val), e->lhs, e->rhs, env);
        }
    emit(MCLSS, count_ast(body) + 1);
    emit(MSET, env->n);
    define(env, id);
}

void compile(ast *e, scope *env) {
    int     level, index;

    switch (e->op) {
    case TNIL:      emit_byte(MNIL); break;
    case TINT:      emit(MLOAD, store_const(e->val)); break;
    case TSTR:      emit(MLOAD, store_const(e->val)); break;

    case TLB:
        for (ast *i = e->lhs; i; i = i->next)
            compile(i, env);
        emit(MARR, count_ast(e->lhs));
        break;

    // case TLC:
    //     n = 0;
    //     for (ast *i = e->lhs; i; i = i->next) {
    //         compile(i->lhs, env);
    //         compile(i->rhs, env);
    //         n++;
    //     }
    //     emit(MMAP, n);
    //     break;

    case TFUN:
        if (e->val.str != anon.str)             // Define symbol
            define(env, e->val.str);
        compile_fun(e->val.str, e->lhs, e->rhs, env);
        break;

    case TRET:
        compile(e->lhs, env);
        emit_byte(MRET);
        break;

    case TEND:                                       // Compound
        for (ast *i = e->lhs; i; i = i->next) {
            compile(i, env);
            if (i->next)
                emit_byte(MPOP);
        }
        break;

    case TRP:                                            // Call
        if (e->lhs->op == TDOT)                   // Method call
            compile_call(e->lhs->lhs, e->lhs->val.str, e->rhs, env);
        else
            compile_call(e->lhs, 0, e->rhs, env);
        break;

    case TRB:                                           // Index
        compile(e->lhs, env);
        compile(e->rhs, env);
        emit_byte(MGAGG);
        break;

    case TDOT:
        compile(e->lhs, env);
        emit(MGDOT, store_const(e->val));
        break;

    case TVAR:
        compile(e->rhs, env);
        emit(MSET, env->n);
        define(env, e->lhs->val.str);
        break;

    case TID:
        if (!find(env, e->val.str, &level, &index))
            semantic(e, "undefined: %s", e->val.str->text);
        if (level == 0)
            emit(MGET, index);
        else
            emit(MFGET, level), emit2(index);
        break;

    case TASN:
        if (e->lhs->op == TID) {
            compile(e->rhs, env);
            if (!find(env, e->lhs->val.str, &level, &index))
                semantic(e, "undefined: %s", e->lhs->val.str->text);
            if (level == 0)
                emit(MSET, index);
            else
                emit(MFSET, level), emit2(index);
        } else if (e->lhs->op == TRB) {
            compile(e->lhs->lhs, env);
            compile(e->lhs->rhs, env);
            compile(e->rhs, env);
            emit_byte(MSAGG);
        } else if (e->lhs->op == TDOT) {
            compile(e->lhs->lhs, env);
            compile(e->rhs, env);
            emit(MSDOT, store_const(e->lhs->val));
        } else
            semantic(e, "INTERNAL_ERROR NOT VAR");
        break;

    case TCLASS:
        compile_class(asstr(e->val), e->lhs, env);
        break;

    default:
        semantic(e, "INTERNAL_ERROR NOT COMPILED %s", tokens[e->op]);
    }

}

void xch(int n, value x) { // Remove n from the stack and push x
    *(sp -= n - 1) = x;
}

int imm(void) {
    pc += 2;
    return read2(pc - 2);
}

void jump(void) {
    pc = program + read2(pc);
}

frame *setup_frame(int resv, int fill, value *values) {
    frame *f = calloc(1, sizeof *f + (resv + fill) * sizeof *sp);
    f->pc = pc - program;
    f->fp = fp;
    memcpy(f->vals, values, fill * sizeof *sp);
    return f;
}

frame *get_frame(int n) {
    frame *f = fp;
    for (int i = 0; i < n; i++) f = f->fp;
    return f;
}

void do_call(int n) {
    ra = sp[-n];
    if (isfun(ra)) {
        fp = setup_frame(asfun(ra)->nlocals,
                         n < asfun(ra)->nparams? n: asfun(ra)->nparams,
                         sp - n + 1);
        sp -= n + 1;
        pc = program + asfun(ra)->pc;
    }
    else if (issubr(ra))
        xch(n + 1, assubr(ra)(n, sp - n + 1));
    else
        xch(n + 1, nil);
    ra = nil;
}

value eval(void) {
    int n;

    while (true)
    switch ((opcode) *pc++) {
    case MHLT:  return *sp--;
    case MNIL:  xch(0, nil); break;
    case MLOAD: xch(0, constants[imm()]); break;
    case MARR:  n = imm(); xch(n, newarray(n, sp - n + 1)); break;
    // case MMAP:  n = imm();
    //             ra = newmap(n);
    //             for (int i = -n; i++ < 0; )
    //                 set_index(ra, sp[i * 2 - 1], sp[i * 2]);
    //             xch(n * 2, ra);
    //             ra = nil;
    //             break;
    case MGAGG: xch(2, get_index(sp[-1], sp[0])); break;
    case MSAGG: xch(3, set_index(sp[-2], sp[-1], sp[0])); break;
    case MFUN:  ra = (value) { FUN, .fun = new(struct fun) };
                *ra.fun = *asfun(constants[imm()]);
                asfun(ra)->fp = fp;
                xch(0, ra);
                ra = nil;
                break;
    case MRET:  pc = program + fp->pc;
                fp = fp->fp;
                break;
    case MPOP:  sp--; break;
    case MBRA:  jump(); break;
    case MGET:  xch(0, fp->vals[imm()]); break;
    case MSET:  fp->vals[imm()] = *sp; break;
    case MFGET: n = imm(); xch(0, get_frame(n)->vals[imm()]); break;
    case MFSET: n = imm(); get_frame(n)->vals[imm()] = *sp; break;
    case MCALL: do_call(imm()); break;
    case MSDOT: xch(2, set_index(get_class(sp[-1]), constants[imm()], sp[0]));
                break;
    case MGDOT: ra = get_index(get_class(*sp), constants[imm()]);
                if (isfun(ra) || issubr(ra)) {
                    xch(0, *sp);
                    sp[-1] = ra;
                    do_call(1);
                } else
                    xch(1, ra);
                ra = nil;
                break;
    case MMDOT: xch(0, *sp);
                sp[-1] = get_index(get_class(*sp), constants[imm()]);
                break;
    case MCLSS: n = imm();
                ra = newobj(n);
                for (int i = -n; i++ < 0; )
                    set_index(ra, sp[i * 2 - 1], sp[i * 2]);
                xch(n * 2, ra);
                ra = nil;
                break;
    }
}

#define ARITH(OP,TEST) (n < 2 || !isint(as[0]) || !isint(as[1]) || TEST)? nil:\
                       integer(asint(as[0]) OP asint(as[1]))
value add_subr(int n, value *as) { return ARITH(+, false); }
value sub_subr(int n, value *as) { return ARITH(-, false); }
value mul_subr(int n, value *as) { return ARITH(*, false); }
value div_subr(int n, value *as) { return ARITH(/, asint(as[1]) == 0); }
value rem_subr(int n, value *as) { return ARITH(%, asint(as[1]) == 0); }

value len_subr(int n, value *as) {
    return n? integer(length(*as)): nil;
}

value insert_subr(int n, value *as) {
    if (n < 3) return nil;
    if (!isarr(as[0])) return nil;
    if (!isint(as[1])) return nil;
    arr_insert(asarr(as[0]), asint(as[1]), as[2]);
    return as[0];
}

value print_subr(int n, value *as) {
    return n? (print_value(*as), *as): nil;
}

void bake(value class, char *id, value value) {
    set_index(class, str(intern(id, -1)), value);
}

void def_global(scope *sco, char *id, value val) {
    define(sco, intern(id, -1));
    fp->vals[sco->n - 1] = val;
}

int main(int argc, char **argv) {
    for (int i = 0; i < 256; i++)
        chars[i] = str(intern((char[]){i}, 1));
    for (int i = 0; tokens[i]; i++)
        tokens[i] = intern(tokens[i], -1)->text;

    namestr = str(intern("name", -1));
    thisstr = str(intern("this", -1));
    anon = str(intern("(anonymous)", -1));

    object_class = newobj(16);
    bake(object_class, "name", str(intern("object", -1)));
    bake(object_class, "class", nil);
    bake(object_class, "print", subr(print_subr));

    for (int i = 0; i < SUBR; i++) {
        classes[i] = newobj(8);
        bake(classes[i], "name", str(intern(types[i], -1)));
        bake(classes[i], "class", object_class);
    }

    bake(classes[STR], "len", subr(len_subr));

    bake(classes[ARR], "len", subr(len_subr));
    bake(classes[ARR], "insert", subr(insert_subr));


    bake(classes[OBJ], "len", subr(len_subr));

    bake(classes[INT], "add", subr(add_subr));
    bake(classes[INT], "sub", subr(sub_subr));
    bake(classes[INT], "mul", subr(mul_subr));
    bake(classes[INT], "div", subr(div_subr));
    bake(classes[INT], "rem", subr(rem_subr));

    fp = setup_frame(65536, 0, 0);

    scope *global = new(scope);

    def_global(global, "object", object_class);
    def_global(global, "int", classes[INT]);

    opensrc(argv[1]);
    while (nextline()) {
        ast *e = after(expr(), TEOL);

        int org = vpc;
        compile(e, global);
        int stop = emit_byte(MHLT);

        pc = program + org;
        fp->pc = stop;
        value result = eval();
        fputs("# ", stdout), print_value(result), puts("");
        if (sp + 1 != stack)
            semantic(e,
                     "INTERNAL_ERROR UNBALANCED STACK: %+d",
                     (int)(sp - stack + 1));
    }

    puts("done.");
}
