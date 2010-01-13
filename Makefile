
CFLAGS=-Wall -O2
PREFIX=$(HOME)

everything: bin/five bin/soko bin/sudoku bin/flip bin/reversi

### gomoku 

bin/five: five.c 
	gcc $(CFLAGS) -o bin/five five.c

five.c: five.scm owl
	./owl five.scm

#### sokoban

bin/soko: soko.c 
	gcc $(CFLAGS) -o bin/soko soko.c

soko.c: soko.scm owl
	./owl soko.scm

### sudoku

bin/sudoku: sudoku.c 
	gcc $(CFLAGS) -o bin/sudoku sudoku.c

sudoku.c: sudoku.scm owl
	./owl sudoku.scm

### flip

bin/flip: flip.c 
	gcc $(CFLAGS) -o bin/flip flip.c

flip.c: flip.scm owl
	./owl flip.scm

### reversi

bin/reversi: reversi.c 
	gcc $(CFLAGS) -o bin/reversi reversi.c

reversi.c: reversi.scm owl
	./owl reversi.scm

### owl (needed for scm -> c phase)

owl: owl.c
	# no need to put in bin/
	gcc $(CFLAGS) -o owl owl.c

owl.c: owl.c.gz
	gzip -c -d owl.c.gz > owl.c

install: everything # error, also copies owl
	mkdir -p $(PREFIX)/bin
	cp -i bin/* $(PREFIX)/bin

clean: 
	rm owl *.c bin/*

