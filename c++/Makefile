ARGS=/home/chonger/data/train.pack /home/chonger/data/train.cpack
TOYARGS=/home/chonger/data/toy.pack /home/chonger/data/toy.cpack

all:
	make -C src/.


test:
	bin/tsgtest $(TOYARGS)

debug:
	gdb bin/tsgtest $(ARGS)

valgrind:
	valgrind -v bin/tsgtest $(ARGS)
