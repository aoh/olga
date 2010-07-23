
CFLAGS=-Wall -O2
PREFIX=$(HOME)

olgame: olgame.c
	gcc $(CFLAGS) -o olgame olgame.c

olgame.c: owl *.scm */*.scm
	./owl -c olgame.scm

owl.c:
	wget http://owl-lisp.googlecode.com/svn/trunk/owl.c.bz2
	bzip2 -d owl.c.bz2

owl: owl.c
	gcc -O2 -o owl owl.c

grale.c:
	wget http://grale.googlecode.com/svn/trunk/grale.c

grale: grale.c
	gcc -O2 -lSDL -o grale grale.c

test: grale olgame
	PATH=$PATH:. ./olgame
	
#
#everything: bin/five bin/soko bin/sudoku bin/flip bin/reversi bin/ataxx
#
#### gomoku 
#
#bin/five: five.c 
#	gcc $(CFLAGS) -o bin/five five.c
#
#five.c: five.scm owl
#	./owl -c five.scm
#
##### sokoban
#
#bin/soko: soko.c 
#	gcc $(CFLAGS) -o bin/soko soko.c
#
#soko.c: soko.scm owl
#	./owl -c soko.scm
#
#### sudoku
#
#bin/sudoku: sudoku.c 
#	gcc $(CFLAGS) -o bin/sudoku sudoku.c
#
#sudoku.c: sudoku.scm owl
#	./owl -c sudoku.scm
#
#### flip
#
#bin/flip: flip.c 
#	gcc $(CFLAGS) -o bin/flip flip.c
#
#flip.c: flip.scm owl
#	./owl -c flip.scm
#
#### reversi
#
#bin/reversi: reversi.c 
#	gcc $(CFLAGS) -o bin/reversi reversi.c
#
#reversi.c: reversi.scm ai.scm match.scm owl
#	./owl -c reversi.scm
#
#### ataxx
#
#bin/ataxx: ataxx.c 
#	gcc $(CFLAGS) -o bin/ataxx ataxx.c
#
#ataxx.c: ataxx.scm ai.scm match.scm owl
#	./owl -c ataxx.scm
#
#### owl (needed for scm -> c phase)
#
#owl: owl.c
#	# no need to put in bin/
#	gcc $(CFLAGS) -o owl owl.c
#
#owl.c: owl.c.gz
#	gzip -c -d owl.c.gz > owl.c
#
#install: everything # error, also copies owl
#	mkdir -p $(PREFIX)/bin
#	cp bin/* $(PREFIX)/bin
#
#clean: 
#	rm owl *.c bin/*
#
