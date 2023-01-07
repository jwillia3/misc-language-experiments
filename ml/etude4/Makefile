CFLAGS=-g -Wall
GC=-Dmalloc=GC_malloc -Drealloc=GC_realloc
LIBS=-lgc

all: ml
	./ml test.ml

ml: ml.c
	$(CC) $(CFLAGS) $(GC) -oml ml.c $(LIBS)
