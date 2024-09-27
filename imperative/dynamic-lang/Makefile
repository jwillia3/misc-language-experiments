.test: lang
	./lang test.lang

lang: lang.c
	$(CC) `pkg-config --cflags bdw-gc` -O2 -g -olang lang.c -lm `pkg-config --libs bdw-gc`
