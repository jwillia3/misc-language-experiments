#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
enum { SOURCE=256*1024, PCODE=64*1024, STACK=64*1024, CALLS=64*1024, CONSTS=64*1024 };
enum { tnil, tbool, tnum, tstr, tarray, thash, tfn };
typedef struct obj obj;
typedef struct GC { unsigned t,m; struct GC **me,*next; } GC;
typedef struct { GC gc; unsigned n, h; char *txt; } string;
typedef struct { GC gc; unsigned n,sz; obj *dat; } array;
struct obj { int type;
    union { int b; string *s; double n; array *a;
        struct fn *f; struct hash *h; void *p; GC *gc; };
};
typedef struct fn { GC gc; obj name,par; obj (*run)(); void *pc; } fn;
typedef struct hash { GC gc; int n,fill,sz; obj *dat, parent; } hash;
typedef struct pcode { int (*step)(); int n; } pcode;
obj nil={tnil}, True={tbool,1}, False={tbool,0};
GC *gc_live,*gc_dead,*gc_work;
unsigned mark, gctime, gcstop;

int nextpow2(int n) { int e=0; while (n) n/=2, e++; return 1<<e; }
double tonum(obj o) { return o.type==tnum? o.n: (o.type==tbool? o.b: 0); }
int istrue(obj o) { return !(o.type==tnil || (o.type==tbool && !o.b)); }
int high(obj o) { return o.type==tarray? o.a->n: 0; }
int cmp(obj a, obj b) {
    int r;
    if (a.type != b.type) return a.type - b.type;
    switch (a.type) {
    case tnil: return 0;
    case tbool: return a.b - b.b;
    case tnum: return a.n < b.n? -1: a.n > b.n? 1: 0;
    case tstr:
        if (a.s->n < b.s->n)
            return (r=memcmp(a.s->txt, b.s->txt, a.s->n))? r: -1;
        if (a.s->n > b.s->n)
            return (r=memcmp(a.s->txt, b.s->txt, b.s->n))? r: 1;
        return memcmp(a.s->txt, b.s->txt, a.s->n);
    case tfn: return a.f - b.f;
    case tarray: return a.a - b.a;
    case thash: return a.h - b.h;
    }
    return 0;
}
obj *hash_place(hash *t, obj k, int set) {
    unsigned n,i,h=(k.type==tstr? k.s->h: k.type==tnum? k.n: k.b);
    struct { obj k,v; } *x = (void*)t->dat;
    obj *p;
    for (i=h%t->sz; x[i].k.type; i=((5*i)+1+h)%t->sz, h/=32)
        if (!cmp(k, x[i].k)) return &x[i].v;
    if (t->parent.type==thash)
        if ((p = hash_place(t->parent.h, k, 0)) && p[-1].type)
            return p;
    if (set && t->fill >= 3*t->sz/4) {  // rebuild crowded table
        x=(void*)t->dat,  n=t->n;           // remember old stats
        t->sz*=2,         t->fill=(t->n=0); // create new larger
        t->dat = calloc(t->sz*2, sizeof(obj));
        for (i=0; t->n < n; i++)            // reinsert items
            if (x[i].k.type) *hash_place(t,x[i].k,1)=x[i].v;
        return free(x), hash_place(t, k, set);
    }
    if (set && k.type) x[i].k=k, t->n++, t->fill += !x[i].v.type;
    return &x[i].v;
}
obj insert(obj o, obj key, obj x) {
    int i = tonum(key);
    if (o.type == thash) return *hash_place(o.h, key, 1) = x;
    if (o.type != tarray) return nil;
    if (i < 0) i += o.a->n + 1;
    if (i < 0 || i > o.a->n) return nil;
    if (o.a->n + 1 >= o.a->sz)
        o.a->dat = realloc(o.a->dat, (o.a->sz += 32) * sizeof(obj));
    memmove(o.a->dat, o.a->dat + 1, (o.a->n - i) * sizeof(obj));
    o.a->n++;
    return o.a->dat[i] = x;
}
obj *index(obj o, obj key, int create) {
    int i = tonum(key);
    if (o.type == thash) return hash_place(o.h, key, create);
    if (o.type != tarray) return 0;
    if (i < 0) i += o.a->n;
    if (i < 0 || i >= o.a->n) return 0;
    return &o.a->dat[i];
}
obj setindex(obj o, obj k, obj v) { obj *p=index(o,k,1); return p? (*p=v): v; }
obj getindex(obj o, obj k) { obj *p=index(o,k,0); return p? *p: nil; }
void print(obj o) {
    int i,n=0;
    switch (o.type) {
    case tnil: printf("nil"); break;
    case tbool: printf(o.b? "true": "false"); break;
    case tnum: printf("%g", o.n); break;
    case tstr: fwrite(o.s->txt, 1, o.s->n, stdout); break;
    case tfn: print(o.f->name); break;
    case tarray:
        putchar('[');
        for (i=0; i<o.a->n; i++) !i || putchar(','), print(o.a->dat[i]);
        putchar(']');
        break;
    case thash:
        putchar('{');
        for (i=0; i<o.h->sz; i++) {
            if (o.h->dat[i*2].type == tnil) continue;
            print(o.h->dat[i*2]); putchar(':'); print(o.h->dat[i*2+1]);
            if (++n != o.h->n) putchar(','); else break;
        }
        putchar('}');
    }
}
obj mkobj(int type, int size) {
    obj o={type};
    o.p=malloc(size);
    o.gc->t=type; o.gc->m=mark;
    o.gc->next=gc_live; o.gc->me=&gc_live; gc_live&&(gc_live->me=&o.gc->next), gc_live=o.gc; 
    return o;
}
obj mknum(double x) { obj o={tnum}; o.n=x; return o; }
obj mkstr(char *txt, int n) {
    obj o = mkobj(tstr, sizeof *o.s);
    o.s->n = n = (n<0? strlen(txt): n);
    o.s->txt = memcpy(malloc(n+1), txt, n);
    o.s->txt[n] = 0;
    for (o.s->h=5381, n=n>32?32:n; n--; ) o.s->h=o.s->h*33+*txt++;
    return o;
}
obj mkarray() {
    obj o = mkobj(tarray, sizeof *o.a);
    o.a->n = o.a->sz = 0; o.a->dat = 0;
    return o;
}
obj mkhash(obj parent, int cap) {
    obj o = mkobj(thash, sizeof *o.h);
    o.h->n = o.h->fill = 0; o.h->sz = cap<8? nextpow2(cap): 8;
    o.h->parent = parent;
    o.h->dat = calloc(o.h->sz * 2, sizeof(obj));
    return o;
}
obj mkfn(obj name, obj run(), obj par, pcode *pc) {
    obj o = mkobj(tfn, sizeof *o.f);
    o.f->name = name; o.f->par = par;
    o.f->run = run; o.f->pc = pc;
    return o;
}

// --------------------------------------------- VIRTUAL MACHINE
pcode   prog[PCODE], *calls[CALLS], *pc=prog, *vpc=prog, **cp=calls-1;
obj     stack[STACK], *sp=stack-1, *lp=stack-1, ctab[CONSTS], env, my;
int     ctabn;
int store(obj o) {
    int i=0;
    ctab[ctabn] = o;
    while (cmp(ctab[i],o)) i++;
    return i==ctabn && ctabn++, i;
}
int emitn(int step(), int n) {
    vpc->step = step;
    vpc->n = n;
    return vpc++-prog;
}
int emit(int step()) { return emitn(step, 0); }
int emito(int step(), obj o) { return emitn(step, store(o)); }
void patch(int jmp, int to) {
    int next = jmp;
    while ((jmp = next) >= 0) {
        next = prog[jmp].n;
        prog[jmp].n = to - jmp - 1;
    }
}
void colour(GC *g, int m, GC **list) {
    *g->me=g->next;
    if (g->next) g->next->me = g->me;
    g->next=*list;
    if (*list) (**list).me=&g->next;
    *list=g; g->me=list;
    g->m = mark+m;
}
void grey(obj *o, int n) {
    for ( ; n--; o++)
        if (o->type>=tstr && o->gc->m<mark)
            colour(o->gc, 1, &gc_work);
}
void black(GC *g) {
    colour(g, 2, &gc_live);
    if (g->t==thash) {
        hash *h=(void*)g;
        grey(&h->parent,1);
        grey(h->dat,h->sz);
    } else if (g->t==tarray) {
        array *a=(void*)g;
        grey(a->dat,a->n);
    } else if (g->t==tfn) {
        fn *f=(void*)g;
        grey(&f->name,1);
        grey(&f->par,1);
    }
}
void destroy(GC *g) {
    if (g->t==tstr) free(((string*)g)->txt);
    if (g->t==tarray) free(((array*)g)->dat);
    if (g->t==thash) free(((hash*)g)->dat);
    free(g);
}
void collect() {
    if (gcstop || gctime--) return;
    gctime = 10000;
    mark += 4;               // step by 4 so it overflows evenly
    gc_dead = gc_live;
    gc_dead->me = &gc_dead;
    gc_live = gc_work=0;
    grey(stack, sp-stack+1);
    grey(&env,1);
    grey(&my,1);
    grey(ctab,ctabn);
    while (gc_work) black(gc_work);
    while (gc_work=gc_dead) {
        gc_dead = gc_dead->next;
        destroy(gc_work);
    }
}
obj push(obj o) { return *++sp = o; }
obj pop() { return *sp--; }
void drop(int n) { sp -= n; }
obj xch(int n, obj o) { drop(n); return push(o); }
obj replace(obj x) { obj tmp=*sp; *sp=x; return tmp; }
obj at(int n) { return sp[-n]; }

#define RUN     while (pc++->step()==1)
#define HALT    return 0
#define CONT    return 1
#define PCO     ctab[pc[-1].n]
#define PCN     pc[-1].n
int phalt() { HALT; }
int pdrop() { pop(); CONT; }
int ppush() { push(PCO); CONT; }
int phash() { push(mkhash(nil, PCN)); CONT; }
int phashitem() { setindex(at(2),at(1),at(0)); drop(2); CONT; }
int parray() { push(mkarray()); CONT; }
int parrayitem() { insert(at(1),mknum(-1),at(0)); pop(); CONT; }
int pneg() { xch(1, mknum(-tonum(*sp))); CONT; }
int pnot() { xch(1, istrue(*sp)? False: True); CONT; }
int padd() { xch(2, mknum(tonum(at(1)) + tonum(at(0)))); CONT; }
int psub() { xch(2, mknum(tonum(at(1)) - tonum(at(0)))); CONT; }
int pmul() { xch(2, mknum(tonum(at(1)) * tonum(at(0)))); CONT; }
int pdiv() { xch(2, mknum(tonum(at(1)) / tonum(at(0)))); CONT; }
int plt() { xch(2, cmp(at(1), at(0))<0? True: False); CONT; }
int ple() { xch(2, cmp(at(1), at(0))<=0? True: False); CONT; }
int pgt() { xch(2, cmp(at(1), at(0))>0? True: False); CONT; }
int pge() { xch(2, cmp(at(1), at(0))>=0? True: False); CONT; }
int peq() { xch(2, cmp(at(1), at(0))==0? True: False); CONT; }
int pne() { xch(2, cmp(at(1), at(0))!=0? True: False); CONT; }
int pmod() { xch(2, mknum(fmod(tonum(at(1)), tonum(at(0))))); CONT; }
int pmy() { push(my); CONT; }
int pnomy() { push(at(0)); sp[-1] = nil; CONT; }
int pgetv() { push(getindex(env, PCO)); CONT; }
int psetv() { setindex(env, PCO, at(0)); CONT; }
int pgeta() { xch(2, getindex(at(1), at(0))); CONT; }
int pgetam() { xch(1, getindex(at(1), at(0))); CONT; }
int pgeta2() { push(getindex(at(1), at(0))); CONT; }
int pseta() { xch(3, setindex(at(2), at(1), at(0))); CONT; }
int pbra() { pc += PCN; CONT; }
int pbrf() { if (!istrue(pop())) pc += PCN; CONT; }
void p_ret(obj o) { pc=*cp--; env=env.h->parent; my=pop(); push(o); };
int p_call(int n) { // return true if caller needs to call RUN
    if (at(n).type == tfn) {
        obj f = at(n), oldenv=env, oldmy=env;
        for ( ; n > high(f.f->par); n--) pop();
        for ( ; n < high(f.f->par); n++) push(nil);
        if (f.f->run) {
            gcstop++;
            p_ret(f.f->run());
            sp += n + 1;
            gcstop--;
            return 0;
        }
        *++cp=pc;
        env=mkhash(nil, n); // DO NOT SET PARENT; force creation not modification
        pc=f.f->pc;
        while (n--)
            setindex(env, getindex(f.f->par, mknum(n)), pop());
        env.h->parent = oldenv; // set parent only now
        pop();
        my=pop();
        push(oldmy);
        return 1;
    } else {
        xch(n+2, nil);
        return 0;
    }
}
int pret() { p_ret(pop()); collect(); CONT; }
int pcall() { p_call(PCN); CONT; }
int pprint() { print(pop()); putchar(PCN? ' ': '\n'); CONT; }

//------------------------------------------------------- SOURCE
typedef struct Ast { int op; obj o; struct Ast *l, *r, *next; } Ast;
char    source[SOURCE], token[SOURCE], *tp=source, tok, back;
int     ln=1, Cnt=-1, Brk=-1, loop, in_fn;
double  tokv;
int (*maths[])() = {padd,psub,pmul,pdiv,pmod,plt,pgt,peq,pne,ple,pge};
char *pun[]={"()[]{}=;:,.!+-*/%<>", "==!=<=>=+=-=*=/=%="};
char *kwd[]={"nil","my","true","false","print","while","if","else","fn","return","continue","break",0};
enum {TNUM=1,TSTR,TNAME,TLP,TRP,TLB,TRB,TLC,TRC,TAS,TSEMI,TCOL,TCOM,TDOT,TBANG,
    TADD,TSUB,TMUL,TDIV,TMOD,TLT,TGT,TEQ,TNE,TLE,TGE,TADDA,TSUBA,TMULA,TDIVA,TMODA,
    TNIL,TMY,TTRUE,TFALSE,TPRINT,TWHILE,TIF,TELSE,TFN,TRET,TCNT,TBRK };

int syntax(char *msg) { printf("error %d: %s\n", ln, msg); exit(1); }
int next() {
    char *t=token,q,*p,*d,**k;
    if (back) return back=0, tok;
    for (;;)
        if (*tp=='\n') tp++, ln++;
        else if (isspace(*tp)) tp++;
        else if (*tp=='#') while (*tp && *tp != '\n') tp++;
        else break;
    if (!*tp) return tok=0;
    if (isdigit(*tp)) return tokv=strtod(tp, &tp), tok=TNUM;
    if (isalnum(*tp) || *tp=='_') {
        while (isalnum(*tp) || *tp=='_') *t++=*tp++;
        for (k=kwd, *t=0; *k; k++)
            if (!strcmp(*k,token)) return tok=TNIL+k-kwd;
        return tok=TNAME;
    } else if (*tp=='\'' || *tp=='"') {
        for (q=*tp++; *tp && *tp != q; tp++)
            if (*tp=='\\')
                switch (*++tp) {
                case 'n': *t++='\n'; break;
                case 'r': *t++='\r'; break;
                case 't': *t++='\t'; break;
                default: *t++=*tp; break;
                }
            else *t++ = *tp;
        if (!*tp) syntax("unclosed string");
        return *t=0, tp++, tok=TSTR;
    } else if (p=strchr(*pun, *tp)) {
        for (d=pun[1]; *d; d+=2)
            if (*d==*tp && d[1]==tp[1])
                return tp+=2, tok=TEQ+(d-pun[1])/2;
        return tp++,    tok=TLP+(p-*pun);
    } else return syntax("unrecognized token"), 0;
}
int want(int t) { return (back = next() != t)? 0: tok; }
int range(int a,int b) { return (back = next()<a || tok>b)? 0: tok; }
int peek(int t) { return back=next(), tok==t? tok: 0; }
char *need(int t, char *msg) { want(t)||syntax(msg); return token; }
Ast *tree(int op, obj o, Ast *l, Ast *r) {
    Ast *a = malloc(sizeof *a);
    a->op = op; a->o = o;
    a->l = l; a->r = r; a->next = 0;
    return a;
}
Ast *treelr(int op, Ast *l, Ast *r) { return tree(op,nil,l,r); }
Ast *treeo(int op, obj o) { return tree(op,o,0,0); }
Ast *expr();
Ast *stmt();
int count_ast(Ast *a) { int n=0; for ( ; a; a=a->next) n++; return n; }
Ast *csv(Ast *item(), int t, char *msg) {
    Ast *all=0, **p = &all;
    do {
        if (peek(t)) break;
        *p = item();
        p = &(**p).next;
    } while (want(TCOM));
    if (t) need(t, msg);
    return all;
}
Ast *pair() {
    Ast *t = treelr(TCOL, 0, 0);
    t->l = treeo(TSTR, mkstr(need(TNAME, "need name"), -1));
    need(TCOL, "need colon");
    return t->r = expr(), t;
}
Ast *function(obj name) {
    obj f = mkfn(name, 0, mkarray(4), 0);
    Ast *a;
    need(TLP, "need params");
    do {
        if (peek(TRP)) break;
        insert(f.f->par, mknum(-1), mkstr(need(TNAME, "need name"),-1));
    } while (want(TCOM));
    need(TRP, "params not closed");
    in_fn++;
    a = treelr(TFN, stmt(), 0);
    in_fn--;
    a->o = f;
    return a;
}
Ast *atom() {
    if (want(TNUM)) return treeo(TNUM, mknum(tokv));
    else if (want(TSTR)) return treeo(TSTR, mkstr(token, -1));
    else if (want(TTRUE)) return treeo(TNIL, True);
    else if (want(TFALSE)) return treeo(TNIL, False);
    else if (want(TNIL)) return treeo(TNIL, nil);
    else if (want(TMY)) return treeo(TMY, nil);
    else if (want(TNAME)) return treeo(TNAME, mkstr(token, -1));
    else if (want(TLB)) return treelr(TRB, csv(expr,TRB,"array not closed"), 0);
    else if (want(TLC)) return treelr(TRC, csv(pair,TRC,"hash not closed"), 0);
    else if (want(TLP)) {
        Ast *a = expr();
        need(TRP, "paren not closed");
        return a;
    } else if (want(TFN))
        return function(nil);
    else return syntax("need expression"), 0;
}
Ast *suffix() {
    Ast *a = atom();
    for (;;)
        if (want(TLP))
            a = treelr(TLP, a, csv(expr,TRP,"call not closed"));
        else if (want(TLB)) {
            a = treelr(TLB, a, expr());
            need(TRB, "index not closed");
        } else if (want(TDOT))
            a = treelr(TDOT, a, treeo(TSTR, mkstr(need(TNAME, "need name"),-1)));
        else return a;
}
Ast *prefix() {
    if (want(TSUB)) return treelr(TSUB, prefix(), 0);
    if (want(TBANG)) return treelr(TBANG, prefix(), 0);
    return suffix();
}
#define BIN(SUB,LO,HI) int t; Ast *a=SUB(); while (t=range(LO,HI)) a=treelr(t,a,SUB()); return a
Ast *mul_expr() { BIN(prefix,TMUL,TMOD); }
Ast *add_expr() { BIN(mul_expr,TADD,TSUB); }
Ast *rel_expr() { BIN(add_expr,TLT,TGE); }
Ast *expr() {
    Ast *l = rel_expr();
    if (want(TAS) || range(TADDA, TMODA)) {
        int t = tok;
        if (l->op != TNAME && l->op != TLB && l->op != TDOT) syntax("bad target");
        return treelr(t, l, expr());
    } else return l;
}
Ast *stmt() {
    int semi=0;
    Ast *a, *c, **p;
    if (want(TPRINT)) { semi = 1; a = treelr(TPRINT, csv(expr,0,0), 0); }
    else if (want(TLC)) {
        a = treelr(TLC, 0, 0);
        for (p=&a->l; !want(TRC); p=&(**p).next) *p = stmt();
    } else if (want(TBRK) || want(TCNT)) {
        semi=1;
        if (!loop) syntax("outside of loop");
        a = treelr(tok, 0, 0);
    } else if (want(TIF)) {
        a = treelr(TIF, treelr(TIF, 0, 0), 0);
        a->l->l = expr();
        a->l->r = stmt();
        a->r = want(TELSE)? stmt(): 0;
    } else if (want(TWHILE)) {
        a = treelr(TWHILE, expr(), 0);
        loop++; a->r = stmt(); loop--;
    } else if (peek(TSEMI)) {
        semi = 1;
        a = treelr(TSEMI, 0, 0);
    } else if (want(TFN))
        a = treelr(TSEMI, function(mkstr(need(TNAME, "need name"),-1)), 0);
    else if (want(TRET)) {
        if (!in_fn) syntax("not in function");
        semi = 1;
        a = treelr(TRET, expr(), 0);
    } else { semi = 1; a = treelr(TSEMI, expr(), 0); }
    if (semi) need(TSEMI, "need semicolon");
    return a;
}
void compile(Ast *a);
void compile_list(Ast *i, int (*action)()) {
    for ( ; i; i=i->next) { compile(i); if (action) emit(action); }
}
void compile_lr(Ast *a) { compile(a->l); compile(a->r); }
void compile(Ast *a) {
    int ob=Brk, oc=Cnt, x;
    Ast *i;
    if (a->op==TNUM||a->op==TSTR||a->op==TNIL||a->op==TFALSE||a->op==TTRUE)
        emito(ppush, a->o);
    else if (a->op == TMY) emit(pmy);
    else if (a->op == TNAME) emito(pgetv, a->o);
    else if (a->op == TSEMI && !a->l); // empty stmt
    else if (a->op == TSEMI) compile_list(a->l, pdrop); // expr as stmt
    else if (a->op == TLC) compile_list(a->l, 0);  // code block
    else if (a->op == TAS) {
        if (a->l->op==TNAME) { compile(a->r); emito(psetv, a->l->o); }
        else  { compile_lr(a->l); compile(a->r); emit(pseta); }
    } else if (a->op >= TADDA && a->op <= TMODA) {
        if (a->l->op==TNAME) emito(pgetv, a->l->o);
        else { compile_lr(a->l); emit(pgeta2); }
        compile(a->r);
        emit(maths[a->op - TADDA]);
        emito(a->l->op==TNAME? psetv: pseta, a->l->o);
    } else if (a->op == TSUB && !a->r) {            // NEGATION
        compile(a->l);
        emit(pneg);
    } else if (a->op == TBANG && !a->r) {
        compile(a->l);
        emit(pnot);
    } else if (a->op >= TADD && a->op <= TGE) {
        compile_lr(a);
        emit(maths[a->op - TADD]);
    } else if (a->op == TRC || a->op == TRB) {     // array & hash
        emitn(a->op==TRB? parray: phash, count_ast(a->l));
        compile_list(a->l, a->op==TRB? parrayitem: phashitem);
    } else if (a->op == TCOL) compile_lr(a);
    else if (a->op == TLB || a->op == TDOT) {
        compile_lr(a);
        emit(pgeta);
    } else if (a->op == TLP) {
        if (a->l->op == TDOT) {
            compile_lr(a->l);
            emit(pgetam);
        } else {
            emito(ppush, nil);
            compile(a->l);
        }   
        compile_list(a->r, 0);
        emitn(pcall, count_ast(a->r));
    } else if (a->op == TPRINT)
        for (i=a->l; i; i=i->next) { compile(i); emitn(pprint, !!i->next); }
    else if (a->op == TIF) {
        int elsej, endj;
        compile(a->l->l);
        elsej = emitn(pbrf, -1);
        compile(a->l->r);
        endj = emitn(pbra, -1);
        patch(elsej, vpc - prog);
        if (a->r) compile(a->r);
        patch(endj, vpc - prog);
    } else if (a->op == TWHILE) {
        Cnt = vpc - prog;
        compile(a->l);
        Brk = emitn(pbrf, -1);
        compile(a->r);
        emitn(pbra, Cnt - (vpc - prog) - 1);
        patch(Brk, vpc-prog);
        Brk = ob; Cnt = oc;
    } else if (a->op == TBRK) ob = Brk = emitn(pbra, Brk);
    else if (a->op == TCNT) emitn(pbra, Cnt - (vpc - prog) - 1);
    else if (a->op == TFN) {
        x = emitn(pbra, -1);
        a->o.f->pc = vpc;
        compile(a->l);
        emito(ppush, nil);
        emit(pret);
        patch(x, vpc - prog);
        emito(ppush, a->o);
        if (a->o.f->name.type != tnil)
            emito(psetv, a->o.f->name);
    } else if (a->op == TRET) {
        compile(a->l);
        emit(pret);
    } else { printf("unhandled case %d\n", a->op); exit(1); }
}

int main(int argc, char **argv) {
    Ast *all=treelr(TLC,0,0), **p = &all->l;
    FILE *f = fopen(argv[1]? argv[1]: "", "r");
    if (!f) {
        printf(argv[1]? "cannot open %s\n": "no source\n", argv[1]);
        exit(1);
    }
    fread(source, 1, SOURCE, f);
    fclose(f);
    env = mkhash(nil, 64);
    while (back = next()) { *p = stmt(); p = &(**p).next; }
    compile(all);
    emit(phalt);                      // make sure program halts
    RUN;
    if (sp != stack-1) printf("STACK OFF: %d\n", sp-stack+1);
}
