#include <ctype.h>
#include <iso646.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct cell cell;
typedef struct {char *id; cell *value;} symbol;
struct cell {
    enum {UNDEF, NIL, TRUTH, CHAR, NUM, LIST,
        COMB, VAR, APP, BIN, FUN, CASE} form;
    union {
        double  num;
        int     chr, comb;
        char    *id;
        struct  {cell *lhs, *rhs; int op;};
    };
};

char    opers[][4] = {"$   ", "=!<>", ":   ", "+-  ", "*/  ", ".   "};
char    source[65536];
char    *src;
int     line;
bool    peeked;
int     token;
char    *tokid;
cell    *tokval;
cell    *True, *False, *undef, *nil, *chars[256], *comb[256];
symbol  globals[4096];
int     nglobals;
char    *interns[4096];
int     ninterns;

#define new(t,...) memcpy(malloc(sizeof(t)), &(t){__VA_ARGS__}, sizeof(t))
#define cell(f,...) new(cell, f, __VA_ARGS__)
cell *num(double n) { return cell(NUM, .num = n); }
cell *app(cell *f, cell *x) { return cell(APP, .lhs = f, .rhs = x); }
cell *cons(cell *h, cell *t) { return cell(LIST, .lhs = h, .rhs = t); }
cell *hd(cell *x) { return x->form == LIST? x->lhs: undef; }
cell *tl(cell *x) { return x->form == LIST? x->rhs: undef; }
cell *expression(), *definition();
cell *eval(cell *c, cell *s);
void *pr(char *s, ...);

bool needs_parens(cell *c) { return c->form >= APP or c->form == LIST; }

bool is_hnf(cell *c) {
    switch (c->form) {
    case UNDEF: case NIL: case TRUTH: case CHAR: case NUM: case LIST:
        return true;
    case COMB: case VAR: case APP: case BIN: case FUN: case CASE:
        return false;
    } return false;
}

bool equal(cell *a, cell *b) {
    if (a->form != b->form) return false;
    switch (a->form) {
    case UNDEF: case NIL: case TRUTH: case CHAR: case COMB:
        return a == b;
    case NUM:
        return a->num == b->num;
    case LIST:
        return equal(hd(a), hd(b)) and equal(tl(a), tl(b));
    case VAR: case APP: case BIN: case FUN: case CASE:
        return false;
    } return false;
}

cell *reverse(cell *c) {
    cell *out = nil;
    for (cell *i = c; i->form == LIST; i = tl(i))
        out = cons(hd(i), out);
    return out;
}

int is_string(cell *c) {
    return c == nil or hd(c)->form == CHAR and is_string(tl(c));
}

cell *print_string(cell *c) {
    if (c->form != LIST) return 0;
    putchar(hd(c)->chr);
    return print_string(tl(c));
}

void *pr_cell(cell *c, bool paren) {
    if (paren and needs_parens(c)) return pr("(%x)", c);
    switch (c->form) {
    case UNDEF: return pr("undef");
    case NIL:   return pr("[]");
    case TRUTH: return pr(c == True? "true": "false");
    case CHAR:  return pr("%%%c", c->chr);
    case NUM:   return pr("%g", c->num);
    case LIST:  return is_string(c)?
                        (pr("'"), print_string(c), pr("'")):
                        pr("%X:%x", eval(c->lhs, nil), eval(c->rhs, nil));
    case COMB:  return pr("%c", c->comb);
    case APP:   return pr("%x %X", c->lhs, c->rhs);
    case BIN:   return pr("(%x %c %x)", c->lhs, c->op, c->rhs);
    case VAR:   return pr("%s", c->id);
    case FUN:   return pr("\\%x=%x", c->lhs, c->rhs);
    case CASE:
        pr("case %x", c->lhs);
        for (cell *i = c->rhs; i != nil; i = tl(i))
            pr("| %x", hd(i));
        return 0;
    } return 0;
}

void *pr(char *s, ...) {
    va_list ap; va_start(ap, s);
    while (*s)
        if (*s != '%')
            putchar(*s++);
        else switch ((s += 2)[-1]) {
        case 'c': putchar(va_arg(ap, int)); break;
        case 'd': printf("%d", va_arg(ap, int)); break;
        case 'g': printf("%g", va_arg(ap, double)); break;
        case 's': printf("%s", va_arg(ap, char*)); break;
        case '!': putchar('\n'); exit(1); break;
        case 'X': pr_cell(va_arg(ap, cell*), true); break;
        case 'x': pr_cell(va_arg(ap, cell*), false); break;
        default:  putchar(s[-1]);
        }
    return 0;
}

char *intern(char *id) {
    for (int i = 0; i < ninterns; i++)
        if (not strcmp(interns[i], id))
            return interns[i];
    return interns[ninterns++] = strdup(id);
}

int next() {
    if (peeked)
        return peeked = false, token;
    for ( ; isspace(*src) or *src == '#'; src++)
        if (*src == '\n')
            line++;
        else if (*src == '#')
            while (src[1] and src[1] != '\n')
                src++;
    if (not *src)
        return token = 0;
    if (isdigit(src[*src == '-']))
        return tokval = num(strtod(src, &src)), token = 'x';
    if (*src == '%')
        return tokval = chars[(unsigned) (src += 2)[-1]], token = 'x';
    if (*src == '"' or *src == '\'') {
        for (char quote = *src++; ; src++)
            if (*src == quote)
                return src++, tokval = reverse(tokval), token = 'x';
            else if (not *src or *src == '\n')
                pr("error %d: unclosed string%!", line);
            else if (*src != '\\')
                tokval = cons(chars[(unsigned) *src], tokval);
            else switch (*++src) {
            case 't': tokval = cons(chars['\t'], tokval); break;
            case 'n': tokval = cons(chars['\n'], tokval); break;
            default:  tokval = cons(chars[(unsigned) *src], tokval);
            }
    }
    char id[32], *t = id;
    while (isalnum(*src) or *src == '_') *t++ = *src++;
    *t = 0;
    tokid = intern(id);
    if (t == id) return token = *src++;
    return token =  not strcmp(id, "true")? (tokval = True, 'x'):
                    not strcmp(id, "false")? (tokval = False, 'x'):
                    not strcmp(id, "undef")? (tokval = undef, 'x'):
                    not strcmp(id, "where")? 'w':
                    not strcmp(id, "case")? 'c':
                    'a';
}

int peek() { return peeked = next(), token; }
int want(int t) { return not (peeked = next() != t); }

cell *suffix(cell *value, int needed) {
    if (not want(needed))
        pr("error %d: need %c%!", line, needed);
    return value;
}

cell *list_expression() {
    if (want(']'))
        return nil;
    cell *hd = expression();
    cell *tl = want(',')? list_expression(): suffix(nil, ']');
    return cell(BIN, .lhs = hd, .rhs = tl, .op = ':');
}

cell *simple_expression(bool required) {
    if (want('x'))
        return tokval;
    if (want('a'))
        return cell(VAR, .id = tokid);
    if (want('('))
        return suffix(expression(), ')');
    if (want('['))
        return list_expression();
    return required? pr("error %d: need expression%!", line): 0;
}

cell *apply_expression() {
    cell *lhs = simple_expression(true), *rhs;
    while ((rhs = simple_expression(false)))
        lhs = app(lhs, rhs);
    return lhs;
}

cell *infix_expression(int level) {
    if (level == sizeof opers / sizeof *opers)
        return apply_expression();
    cell *lhs = infix_expression(level + 1);
    while (peek() and memchr(opers[level], peek(), 4)) {
        int op = next();
        cell *rhs = infix_expression(level + not strchr("$:", op));
        lhs = cell(BIN, .lhs = lhs, .rhs = rhs, .op = op);
    }
    return lhs;
}

cell *function() {
    if (want('=')) return expression();
    cell *lhs = simple_expression(true);
    cell *rhs = function();
    return cell(FUN, .lhs = lhs, .rhs = rhs);
}

cell *complex_expression() {
    if (want('c')) {
        cell *subject = expression();
        cell *rules = nil;
        while (want('|')) rules = cons(definition(), rules);
        return cell(CASE, .lhs = subject, .rhs = reverse(rules));
    }
    return want('\\')? function(): infix_expression(0);
}

cell *expression() { return complex_expression(); }

cell *definition() {
    cell *lhs = simple_expression(true);
    cell *rhs = function();
    return cell(FUN, .lhs = lhs, .rhs = rhs);
}

cell *find(char *id, cell *otherwise) {
    for (int i = nglobals; i--; )
        if (globals[i].id == id)
            return globals[i].value;
    return otherwise;
}

bool binds_var(cell *formal, char *id) {
    return formal->form == VAR and formal->id == id;
}

cell *subst(cell *c, cell *formal, cell *actual) {
    if (formal->form == BIN and formal->op == ':')
        return actual = eval(actual, nil),
            subst(subst(c, formal->lhs, hd(actual)), formal->rhs, tl(actual));
    else if (formal->form != VAR)
        return equal(formal, actual)? c: undef;

    if (c->form == VAR)
        return c->id == formal->id? actual: find(c->id, c);
    else if (c->form == APP)
        return app(subst(c->lhs, formal, actual),
                   subst(c->rhs, formal, actual));
    else if (c->form == BIN)
        return cell(BIN, .lhs = subst(c->lhs, formal, actual),
                   .rhs = subst(c->rhs, formal, actual),
                   .op = c->op);
    else if (c->form == FUN and not binds_var(c->lhs, formal->id))
        return cell(FUN, .lhs = c->lhs, .rhs = subst(c->rhs, formal, actual));
    else if (c->form == CASE) {
        cell *subject = subst(c->lhs, formal, actual);
        cell *rules = nil;
        for (cell *i = c->rhs; i != nil; i = tl(i))
            rules = cons(subst(hd(i), formal, actual), rules);
        return cell(CASE, .lhs = subject, .rhs = reverse(rules));
    }
    else return c;
}

cell *compose(cell *x, cell *y) {
    cell *lhs = cell(VAR, .id = intern("_"));
    cell *rhs = app(x, app(y, cell(VAR, .id = intern("_"))));
    return cell(FUN, .lhs = lhs, .rhs = rhs);
}

cell *eval(cell *c, cell *s) {
    #define PULL    (x = hd(s), y = hd(tl(s)), s = tl(tl(s)))
    #define GRAB    (PULL, x = eval(x, s), y = eval(y, s))
    #define UD(OUT) (x == undef or y == undef? undef: (OUT))
    #define NN(OUT) (x->form != NUM or y->form != NUM? undef: (OUT))
    cell *x, *y;

    if (not is_hnf(c)) pr("TR: %x\n", c);
    if (is_hnf(c))
        return c;
    else if (c->form == COMB)
        switch (c->comb) {
        case ':': return PULL, UD(cons(x, y));
        case '+': return GRAB, NN(num(x->num + y->num));
        case '-': return GRAB, NN(num(x->num - y->num));
        case '*': return GRAB, NN(num(x->num * y->num));
        case '/': return GRAB, NN(num(x->num / y->num));
        case '<': return GRAB, NN(x->num < y->num? True: False);
        case '>': return GRAB, NN(x->num > y->num? True: False);
        case '=': return GRAB, NN(equal(x, y)? True: False);
        case '!': return GRAB, NN(equal(x, y)? False: True);
        case '$': return eval(hd(s), cons(hd(tl(s)), tl(tl(s))));
        case '.': return GRAB, eval(compose(x, y), s);
        default:  return pr("DID NOT EXECUTE COMB %c%!", c->comb);
        }
    else if (c->form == APP)
        return eval(c->lhs, cons(c->rhs, s));
    else if (c->form == BIN)
        return eval(comb[c->op], cons(c->lhs, cons(c->rhs, s)));
    else if (c->form == FUN)
        return s == nil? c: eval(subst(c->rhs, c->lhs, hd(s)), tl(s));
    else if (c->form == VAR)
        return eval(find(c->id, undef), s);
    else if (c->form == CASE) {
        cell *actual = eval(c->lhs, nil);
        for (cell *i = c->rhs; i != nil; i = tl(i)) {
            cell *result = subst(hd(i)->rhs, hd(i)->lhs, actual);
            if (result != undef)
                return eval(result, nil);
        } return eval(undef, nil);
    }

    else return pr("DID NOT EXECUTE %x%!", c);
}

int main() {
    setbuf(stdout, 0);
    for (int i = 0; i < 256; i++)
        chars[i] = cell(CHAR, .chr = i),
        comb[i] = cell(COMB, .comb = i);
    False = cell(TRUTH, {0});
    True = cell(TRUTH, {0});
    undef = cell(UNDEF, {0});
    nil = cell(NIL, {0});
    line = 1;
    src = source;
    fread(source, 1, sizeof source, stdin);

    while (peek())
        if (want('>'))
            pr("# %x\n", eval(suffix(expression(), ';'), nil));
        else {
            cell *def = suffix(definition(), ';');
            if (def->lhs->form != VAR) pr("error %d: TLD must be var%!", line);
            globals[nglobals++] = (symbol){def->lhs->id, def->rhs};
        }
    puts("done.");
}
