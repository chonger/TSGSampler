BINDIR=../bin/
INCL=../lib/sparsehash/include
SRCFILES=Sample.cpp ParseTree.cpp TreeChunker.cpp
HEADERS=ParseTree.hpp TreeChunker.hpp
LIBS=-lgsl -lgslcblas -lm

DEBUGFLAGS=-g -DTSGDEBUG
REGFLAGS=-O2

all: 
	g++ $(REGFLAGS) -Wall -o $(BINDIR)tsgtest -I$(INCL) $(SRCFILES) $(LIBS)
