CFLAGS=-Werror -Wall -Wextra -g -fsanitize=address
boot:	boot.c
	$(CC) $(CFLAGS) -oboot boot.c
test:	boot test.al
	ASAN_OPTIONS=detect_leaks=0 ./boot <test.al
