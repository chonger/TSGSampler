BINDIR=../bin/
INCL=../lib/sparsehash/include
SRCFILES=ParseTree.cpp TreeChunker.cpp DP.cpp
HEADERS=ParseTree.hpp TreeChunker.hpp DP.cpp
LIBS=-lgsl -lgslcblas -lm -pthread -lpthread -lboost_thread-mt

DEBUGFLAGS=-g -DTSGDEBUG
REGFLAGS=-O2

all: 
	g++ $(REGFLAGS) -Wall $(LIBS) -o $(BINDIR)tsgsample -I$(INCL) Sample.cpp $(SRCFILES) 

dbl:
	g++ $(DEBUGFLAGS) -Wall $(LIBS) -o $(BINDIR)twosample -I$(INCL) DoubleSample.cpp $(SRCFILES) 		
