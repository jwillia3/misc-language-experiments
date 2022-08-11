test:	proto
	./proto proto.ml

proto:	proto.c
	$(CC) proto.c -oproto -lgc -Dmalloc=GC_malloc
