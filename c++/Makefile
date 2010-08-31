ARGS=/home/chonger/data/train.pack /home/chonger/data/train.cpack 
ARGS2=/home/chonger/data/fft.cpack /home/chonger/data/fft.cpack 
TOYARGS=/home/chonger/data/toyGen.pack /home/chonger/data/toyGen.cpack 
TARGS=/home/chonger/data/enb/spack.russell.txt /home/chonger/data/cpack.russell.txt
ARGA=/home/chonger/data/airplane/airplane.pack /home/chonger/data/airplane/airplane.cpack

2ARGS=/home/chonger/data/toyDouble.pack /home/chonger/data/toyDouble.cpack

all:
	make -C src/.

twoC:
	make -C src/. dbl

toy:
	bin/tsgsample /home/chonger/data/toyGen.pack /home/chonger/data/toyGen.cpack runlog /home/chonger/data/schedule.txt 0 0 

run:
	bin/tsgsample $(ARGS) runlog /home/chonger/data/schedule.txt 0 0

dbl:
	bin/twosample $(2ARGS) 2Xrunlog 10

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
