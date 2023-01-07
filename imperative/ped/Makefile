GC=-Dmalloc=GC_malloc -Drealloc=GC_realloc
LIB=-lgc
CFLAGS=-g -O2 -Wall

all: ped
	./ped test.ped

ped: ped.c
	$(CC) $(CFLAGS) $(GC) -oped ped.c $(LIB)
