ARGS=/home/chonger/data/fft.pack /home/chonger/data/fft.cpack 
ARGS2=/home/chonger/data/fft.cpack /home/chonger/data/fft.cpack 
TOYARGS=/home/chonger/data/toy.pack /home/chonger/data/toy.cpack 
TARGS=/home/chonger/data/enb/spack.russell.txt /home/chonger/data/cpack.russell.txt


all:
	make -C src/.

toy:
	bin/tsgtest $(TOYARGS) log1

run:
	bin/tsgtest $(ARGS) runlog /home/chonger/data/schedule.txt

cont: 
	bin/tsgtest $(ARGS2) runlog /home/chonger/data/schedule.txt cont

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
