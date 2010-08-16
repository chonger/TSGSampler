#ifndef TSG_TREEDATA
#define TSG_TREEDATA 1

#include <iostream>
#include "Util.hpp"

struct TreeData {

    TreeData() {}
    
    TreeData(std::ifstream& ifs) {
        
        size_t numRules;
        readLEbytes(ifs,reinterpret_cast<char*>(&numRules),sizeof(size_t));
        printf("%d rules\n",numRules) ;
        
        //read pcfg probs
        pcfg = new double[numRules];
        for(size_t i=0;i<numRules;++i) {
            readLEbytes(ifs,reinterpret_cast<char*>(pcfg + i),sizeof(double));
            //printf("PCFG PROB = %f\n",probs[i]);
        }
        
        //read head indexes
        lhsMap = new size_t[numRules];
        for(size_t i=0;i<numRules;++i) {
            readLEbytes(ifs,reinterpret_cast<char*>(lhsMap + i),sizeof(size_t));
            //printf("LHS Index = %d\n",lhsmap[i]);
        }
        
        readLEbytes(ifs,reinterpret_cast<char*>(&nLHS),sizeof(size_t));
        printf("%d NTs\n",nLHS);
        
        for(size_t i=0;i<numRules;++i) {
            if(lhsMap[i] >= nLHS) {
                printf("Bad lhs = %d\n",i);
                throw -1;
            }
        }
        
        readLEbytes(ifs,reinterpret_cast<char*>(&ntrees),sizeof(size_t));
        printf("%d Trees\n",ntrees);

        trees = new ParseTree[ntrees];
        
        for(size_t i=0;i<ntrees;++i) {
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
            
            trees[i] = ParseTree(nodes,markers,numNodes);
        }
        
        lhsTotals = new size_t[nLHS];
        for(size_t i=0;i<nLHS;++i) {
            lhsTotals[i] = 0;
        }

        for(size_t i=0;i<ntrees;++i) {
            ParseTree& pt = trees[i];
            
            for(size_t j=0;j<pt.size;++j) {
                
                size_t lhsInd = lhsMap[pt.nodelist[j].index];
                lhsTotals[lhsInd] += 1;
                
            }
        }
    }
    
    void writeData(std::ofstream& ofs ) {
        
        writeBEbytes(ofs,reinterpret_cast<char*>(&(numRules)),sizeof(size_t));
        
        //write pcfg probs
        for(size_t i=0;i<numRules;++i) {
            writeBEbytes(ofs,reinterpret_cast<char*>(pcfg + i),sizeof(double));
        }
        
        //write head indexes
        for(size_t i=0;i<numRules;++i) {
            writeBEbytes(ofs,reinterpret_cast<char*>(lhsMap + i),sizeof(size_t));
        }
                
        writeBEbytes(ofs,reinterpret_cast<char*>(&(nLHS)),sizeof(size_t));        
        
        writeBEbytes(ofs,reinterpret_cast<char*>(&(ntrees)),sizeof(size_t));
        
        for(size_t i=0;i<ntrees;++i) {
            size_t numNodes = trees[i].size;
            writeBEbytes(ofs,reinterpret_cast<char*>(&numNodes),sizeof(size_t));
            
            for(size_t j=0;j<numNodes;++j) {
                
                TreeNode n = trees[i].nodelist[j];
                
                writeBEbytes(ofs,reinterpret_cast<char*>(&(n.index)),sizeof(size_t));
                writeBEbytes(ofs,reinterpret_cast<char*>(&(n.isTerminal)),sizeof(char));
                writeBEbytes(ofs,reinterpret_cast<char*>(&(n.head)),sizeof(size_t));
                writeBEbytes(ofs,reinterpret_cast<char*>(&(n.parent)),sizeof(size_t));
                writeBEbytes(ofs,reinterpret_cast<char*>(&(n.sibling)),sizeof(size_t));
                writeBEbytes(ofs,reinterpret_cast<char*>(&(n.lexHead)),sizeof(size_t));
                writeBEbytes(ofs,reinterpret_cast<char*>(&(n.type)),sizeof(size_t));
            }
            
            for(size_t j=0;j<numNodes;++j) {
                char mark = trees[i].markers[j];
                writeBEbytes(ofs,reinterpret_cast<char*>(&mark),sizeof(char));
            }
            
        }
        
    }
    
    ~TreeData() {
        if(pcfg != NULL) 
            delete[] pcfg;
        pcfg = NULL;

        if(trees != NULL)
            delete[] trees;
        trees = NULL;

        if(lhsMap != NULL)
            delete[] lhsMap;
        lhsMap = NULL;

        if(lhsTotals != NULL)
            delete[] lhsTotals;
        lhsTotals = NULL;
    }
    
    size_t* lhsMap;
    
    //for base prob dist
    double* pcfg;
    size_t numRules;
    
    ParseTree* trees;
    size_t ntrees;
    size_t nLHS;
    size_t* lhsTotals;    

};


class DoubleData : public TreeData {
public:
    DoubleData(std::ifstream& ifs) : TreeData(ifs) {
        readLEbytes(ifs,reinterpret_cast<char*>(&backGend),sizeof(size_t));
        printf("%d are background, %d are specific\n",backGend,ntrees - backGend);
    }

    void write2Data(std::ofstream& ofs ) {
        writeData(ofs);
        writeBEbytes(ofs,reinterpret_cast<char*>(&(backGend)),sizeof(size_t));
    }

    size_t backGend;
    
};


#endif
