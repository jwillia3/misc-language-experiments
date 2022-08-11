GC=-lgc -DUSE_GC

run: lisp
	./lisp test.lisp

lisp: lisp.c
	$(CC) $(GC) -g -O2 -olisp lisp.c
