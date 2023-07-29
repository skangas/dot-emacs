CC=gcc
CFLAGS = -g -Wall -Wpedantic -Wextra -Werror

main: main.o

.PHONY: clean

clean:
	@rm -f *.o
