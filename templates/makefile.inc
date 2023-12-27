# -*- mode: makefile -*-
CC=gcc
CFLAGS = -g3 -Wall -Wpedantic -Wextra -Werror -Wno-unused-parameter -Wno-unused-function -fsanitize=undefined -﻿fsanitize-trap -﻿Wdouble-promotion -Wconversion

main: main.o

.PHONY: clean

clean:
	@rm -f *.o
