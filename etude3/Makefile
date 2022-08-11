GC=-lgc -Dmalloc=GC_malloc -Drealloc=GC_realloc

all: boot
	./boot test.ml

boot: boot.c
	$(CC) $(GC) -g -Wall -Wno-dangling-else -oboot boot.c
