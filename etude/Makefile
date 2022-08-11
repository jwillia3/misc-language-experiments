CFLAGS=-g -Wall -Wno-dangling-else -Wno-parentheses
GC=-DGC
test: ml
	./ml test.ml

ml: ml.c
	$(CC) $(CFLAGS) $(GC) ml.c -lgc -oml
