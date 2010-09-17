BINDIR=../bin/
INCL=../lib/sparsehash/include
SRCFILES=ParseTree.cpp DP.cpp HDP.cpp DoubleChunker.cpp
HEADERS=ParseTree.hpp DP.hpp HDP.hpp DoubleChunker.hpp
LIBS=-lgsl -lgslcblas -lm -pthread -lpthread -lboost_thread

DEBUGFLAGS=-g -DTSGDEBUG
REGFLAGS=-O2

all: 
	g++ $(REGFLAGS) -Wall $(LIBS) -o $(BINDIR)hdpsample -I$(INCL) DoubleSample.cpp $(SRCFILES) 

