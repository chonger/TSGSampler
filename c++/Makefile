ARGS=/home/chonger/data/2.pack /home/chonger/data/2.cpack
TOYARGS=/home/chonger/data/toy.pack /home/chonger/data/toy.cpack

all:
	make -C src/.

toy:
	bin/tsgtest $(TOYARGS) log1

bench:
	bin/tsgtest $(ARGS) log1
	bin/tsgtest $(ARGS) log2
	bin/tsgtest $(ARGS) log3
	bin/tsgtest $(ARGS) log4
	bin/tsgtest $(ARGS) log5

debug:
	gdb bin/tsgtest $(ARGS)

valgrind:
	valgrind -v bin/tsgtest $(ARGS)
