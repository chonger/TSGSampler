#include "ParseTree.hpp"
#include "TreeChunker.hpp"
#include <stdio.h>
#include <string>
#include <fstream>
#include <ctime>

void readLEbytes(std::ifstream& ifs, char* data, size_t bytes) {
    for(int i = bytes - 1;i>=0;--i) {
        ifs.read(data + i,1);
    }
}

int main(int argc, const char* argv[]) {
    srand(time(NULL));

    const char* filename = argv[1];

    std::ifstream ifs(filename,std::ios::binary);

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
            readLEbytes(ifs,reinterpret_cast<char*>(&index),sizeof(size_t));
            readLEbytes(ifs,reinterpret_cast<char*>(&isTerm),sizeof(char));
            readLEbytes(ifs,reinterpret_cast<char*>(&head),sizeof(size_t));
            readLEbytes(ifs,reinterpret_cast<char*>(&parent),sizeof(size_t));
            readLEbytes(ifs,reinterpret_cast<char*>(&sibling),sizeof(size_t));
            
            nodes[j] = TreeNode(index,isTerm,head,parent,sibling);
            //nodes[j].printMe();
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

    TreeChunker chunker(lhsmap,probs,betas,alphas,ptrees,numTrees,numLHS);

    printf("SAMPLING\n");
    chunker.resample(15000,5.0,1.0);

    chunker.resample(5000,1.0,1.0);

    chunker.packResults(argv[2]);
    return 0;
}
