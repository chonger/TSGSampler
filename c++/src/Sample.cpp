#include "ParseTree.hpp"
#include "TreeChunker.hpp"
#include <stdio.h>
#include <string>
#include <fstream>
#include <ctime>
#include <fstream>
#include <iostream>
#include <signal.h>

TreeChunker* chunker = NULL;
std::string outpath;

void readLEbytes(std::ifstream& ifs, char* data, size_t bytes) {
    for(int i = bytes - 1;i>=0;--i) {
        ifs.read(data + i,1);
    }
}

void abortFunc(int sig) {
    if(chunker != NULL)
        chunker->stop = true;
}


struct ScheduleItem {
    size_t nIter;
    double smoothS;
    double smoothE;

    ScheduleItem(size_t n, double s, double e) :
        nIter(n), smoothS(s), smoothE(e)  
    {}


};


int main(int argc, const char* argv[]) {
    srand(time(NULL));
    
    signal(SIGABRT,&abortFunc);
    signal(SIGTERM,&abortFunc);
    signal(SIGINT,&abortFunc);
    
    const char* filename = argv[1];

    std::ifstream ifs(filename,std::ios::binary);
    if(!ifs.is_open()) {
        printf("Invalid file at %s\n",filename);
        exit(-2);
    }
    
    printf("Reading packed TSG from %s\n",filename);

    size_t numRules;
    readLEbytes(ifs,reinterpret_cast<char*>(&numRules),sizeof(size_t));
    printf("%d rules\n",numRules) ;

    //read pcfg probs
    double* probs = new double[numRules];
    for(size_t i=0;i<numRules;++i) {
        readLEbytes(ifs,reinterpret_cast<char*>(probs + i),sizeof(double));
        //printf("PCFG PROB = %f\n",probs[i]);
    }

    //read head indexes
    size_t* lhsmap = new size_t[numRules];
    for(size_t i=0;i<numRules;++i) {
        readLEbytes(ifs,reinterpret_cast<char*>(lhsmap + i),sizeof(size_t));
        //printf("LHS Index = %d\n",lhsmap[i]);
    }

    size_t numLHS;
    readLEbytes(ifs,reinterpret_cast<char*>(&numLHS),sizeof(size_t));
    printf("%d NTs\n",numLHS);

    for(size_t i=0;i<numRules;++i) {
        if(lhsmap[i] >= numLHS) {
            printf("Bad lhs = %d\n",i);
            throw -1;
        }
    }
    
    //read betas
    double* betas = new double[numLHS];
    for(size_t i=0;i<numLHS;++i) {
        readLEbytes(ifs,reinterpret_cast<char*>(betas + i),sizeof(double));
        //printf("BETA = %f\n",betas[i]);
    }

    //read alphas
    double* alphas = new double[numLHS];
    for(size_t i=0;i<numLHS;++i) {
        readLEbytes(ifs,reinterpret_cast<char*>(alphas + i),sizeof(double));
        //printf("ALPHA = %f\n",alphas[i]);
    }

    size_t numTrees;
    readLEbytes(ifs,reinterpret_cast<char*>(&numTrees),sizeof(size_t));
    printf("%d Trees\n",numTrees);

    ParseTree* ptrees = new ParseTree[numTrees];
    
    for(size_t i=0;i<numTrees;++i) {
        size_t numNodes;
        readLEbytes(ifs,reinterpret_cast<char*>(&numNodes),sizeof(size_t));
        //printf("%d nodes\n",numNodes);

        TreeNode nodes[numNodes];
        bool markers[numNodes];
        
        for(size_t j=0;j<numNodes;++j) {
            size_t index;
            char isTerm;
            size_t head;
            size_t parent;
            size_t sibling;
            size_t lexhead;
            size_t type;
            readLEbytes(ifs,reinterpret_cast<char*>(&index),sizeof(size_t));
            readLEbytes(ifs,reinterpret_cast<char*>(&isTerm),sizeof(char));
            readLEbytes(ifs,reinterpret_cast<char*>(&head),sizeof(size_t));
            readLEbytes(ifs,reinterpret_cast<char*>(&parent),sizeof(size_t));
            readLEbytes(ifs,reinterpret_cast<char*>(&sibling),sizeof(size_t));
            readLEbytes(ifs,reinterpret_cast<char*>(&lexhead),sizeof(size_t));
            readLEbytes(ifs,reinterpret_cast<char*>(&type),sizeof(size_t));

            nodes[j] = TreeNode(index,isTerm,head,parent,sibling,lexhead,type);
            //printf("NODE HEAD - %d\n",nodes[j].lexHead);
        }
        for(size_t j=0;j<numNodes;++j) {
            char mark;
            readLEbytes(ifs,reinterpret_cast<char*>(&mark),sizeof(char));
            markers[j] = (mark != 0);
            //printf("%d\n",markers[j]);
        }

        ptrees[i] = ParseTree(nodes,markers,numNodes);
    }
    ifs.close();        

    chunker = new TreeChunker(lhsmap,probs,numRules,betas,alphas,ptrees,numTrees,numLHS);
    outpath = argv[2];
    
    printf("Starting to Sample\n");

    if(argc > 5 && std::string(argv[5]) == "cont")
        chunker->outstream.open(argv[3],ios::app);
    else
        chunker->outstream.open(argv[3]);


    //read schedule
    std::vector<ScheduleItem> schedule;

    std::ifstream sfs(argv[4]);
    
    while(sfs) {
        
        int nIter;
        double st;
        double en;

        sfs >> nIter;
        if(sfs.eof())
            break;
        sfs >> st;
        sfs >> en;

        printf("SCHEDULE - %d iterations smoothed from %f to %f\n",nIter,st,en);
        
        ScheduleItem item(nIter,st,en);
        
        schedule.push_back(item);
    }
    sfs.close();
    
    for(std::vector<ScheduleItem>::iterator it = schedule.begin(); it != schedule.end(); ++it) {
        chunker->resample(it->nIter,it->smoothS,it->smoothE,0,0);
    }

    chunker->outstream.close();
    chunker->packResults(outpath.c_str());
    delete chunker;
    chunker = NULL;

    return 0;
}
