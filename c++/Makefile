ARGS=/home/chonger/data/fft.pack /home/chonger/data/fft.cpack 
ARGS2=/home/chonger/data/fft.cpack /home/chonger/data/fft.cpack 
TOYARGS=/home/chonger/data/toy.pack /home/chonger/data/toy.cpack 
TARGS=/home/chonger/data/enb/spack.russell.txt /home/chonger/data/cpack.russell.txt
ARGA=/home/chonger/data/airplane/airplane.pack /home/chonger/data/airplane/airplane.cpack

all:
	make -C src/.

toy:
	bin/tsgtest $(TOYARGS) log1 /home/chonger/data/schedule.txt

run:
	bin/tsgtest $(ARGS) runlog /home/chonger/data/schedule.txt 100 100


trun:
	bin/tsgtest /home/chonger/data/fft.pack /home/chonger/data/fft11.cpack runlog /home/chonger/data/schedule.txt 100 100 &
	bin/tsgtest /home/chonger/data/fft.pack /home/chonger/data/fft01.cpack runlog /home/chonger/data/schedule.txt 1 100 &
	bin/tsgtest /home/chonger/data/fft.pack /home/chonger/data/fft10.cpack runlog /home/chonger/data/schedule.txt 100 1 &
	bin/tsgtest /home/chonger/data/fft.pack /home/chonger/data/fft00.cpack runlog /home/chonger/data/schedule.txt 1 1 &

air:
	bin/tsgtest $(ARGA) airlog /home/chonger/data/airplane/schedule.txt

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
