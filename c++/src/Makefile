BINDIR=../bin/
INCL=../lib/sparsehash/include
SRCFILES=Sample.cpp ParseTree.cpp TreeChunker.cpp DP.cpp
HEADERS=ParseTree.hpp TreeChunker.hpp
LIBS=-lgsl -lgslcblas -lm -pthread -lpthread -lboost_thread-mt

DEBUGFLAGS=-g -DTSGDEBUG
REGFLAGS=-O2

all: 
	g++ $(REGFLAGS) -Wall $(LIBS) -o $(BINDIR)tsgtest -I$(INCL) $(SRCFILES) 
