ARGS=/home/chonger/data/train.pack /home/chonger/data/train.cpack

all:
	make -C src/.

test:
	bin/tsgtest $(ARGS)

debug:
	gdb bin/tsgtest $(ARGS)

valgrind:
	valgrind -v bin/tsgtest $(ARGS)
