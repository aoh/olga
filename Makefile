
CFLAGS=-Wall -O2
PREFIX=$(HOME)

everything: bin/five bin/soko bin/sudoku bin/flip

### gomoku 

bin/five: five.c 
	gcc $(CFLAGS) -o bin/five five.c

five.c: five.scm bin/owl
	bin/owl five.scm

#### sokoban

bin/soko: soko.c 
	gcc $(CFLAGS) -o bin/soko soko.c

soko.c: soko.scm bin/owl
	bin/owl soko.scm

### sudoku

bin/sudoku: sudoku.c 
	gcc $(CFLAGS) -o bin/sudoku sudoku.c

sudoku.c: sudoku.scm bin/owl
	bin/owl sudoku.scm

### flip

bin/flip: flip.c 
	gcc $(CFLAGS) -o bin/flip flip.c

flip.c: flip.scm bin/owl
	bin/owl flip.scm

### owl (needed for scm -> c phase)

bin/owl: owl.c
	gcc $(CFLAGS) -o bin/owl owl.c

owl.c: owl.c.gz
	gzip -c -d owl.c.gz > owl.c

install:
	mkdir -p $(PREFIX)/bin
	cp bin/* $(PREFIX)/bin
