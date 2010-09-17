#ifndef TSG_TREEDATA
#define TSG_TREEDATA 1

#include <iostream>
#include "Util.hpp"

struct TreeData {
    
    TreeData(std::ifstream& ifs) {
        
        readLEbytes(ifs,reinterpret_cast<char*>(&ntrees),sizeof(size_t));
        printf("%d Trees\n",ntrees);

        trees = new ParseTree[ntrees];
        
        for(size_t i=0;i<ntrees;++i) {
            size_t numNodes;
            readLEbytes(ifs,reinterpret_cast<char*>(&numNodes),sizeof(size_t));
            //printf("%d nodes\n",numNodes);
            
            TreeNode nodes[numNodes];
            bool markers[numNodes];
            NodeOffset warps[numNodes];
            
            for(size_t j=0;j<numNodes;++j) {
                size_t index;
                char isTerm;
                size_t head;
                size_t parent;
                size_t sibling;
                size_t lexhead;
                size_t type;
                size_t aspect;
                size_t foot;
                readLEbytes(ifs,reinterpret_cast<char*>(&index),sizeof(size_t));
                readLEbytes(ifs,reinterpret_cast<char*>(&isTerm),sizeof(char));
                readLEbytes(ifs,reinterpret_cast<char*>(&head),sizeof(size_t));
                readLEbytes(ifs,reinterpret_cast<char*>(&parent),sizeof(size_t));
                readLEbytes(ifs,reinterpret_cast<char*>(&sibling),sizeof(size_t));
                readLEbytes(ifs,reinterpret_cast<char*>(&lexhead),sizeof(size_t));
                readLEbytes(ifs,reinterpret_cast<char*>(&type),sizeof(size_t));
                readLEbytes(ifs,reinterpret_cast<char*>(&aspect),sizeof(size_t));
                readLEbytes(ifs,reinterpret_cast<char*>(&foot),sizeof(size_t));
                size_t segTop = head;
                nodes[j] = TreeNode(index,isTerm,head,parent,sibling,lexhead,type,aspect,foot,segTop);
                //printf("NODE HEAD - %d\n",nodes[j].lexHead);
            }
            for(size_t j=0;j<numNodes;++j) {
                char mark;
                readLEbytes(ifs,reinterpret_cast<char*>(&mark),sizeof(char));
                markers[j] = (mark != 0);
                //printf("%d\n",markers[j]);
            }

            
            for(size_t j=0;j<numNodes;++j) {
                size_t wrp = 0;
                //readLEbytes(ifs,reinterpret_cast<char*>(&wrp),sizeof(size_t));
                warps[j] = wrp;
            }
            
            
            trees[i] = ParseTree(nodes,markers,warps,numNodes);
        }
        
    }
    
    void write(std::ofstream& ofs ) {
        
        writeBEbytes(ofs,reinterpret_cast<char*>(&(ntrees)),sizeof(size_t));
        
        for(size_t i=0;i<ntrees;++i) {
            size_t numNodes = trees[i].size;
            writeBEbytes(ofs,reinterpret_cast<char*>(&numNodes),sizeof(size_t));
            
            for(size_t j=0;j<numNodes;++j) {
                
                TreeNode n = trees[i].nodelist[j];

                size_t index = n.index;
                char isTerm = n.isTerminal;
                size_t head = n.head;
                size_t parent = n.parent;
                size_t sibling = n.sibling;
                size_t lexhead = n.lexHead;
                size_t type = n.type;
                size_t aspect = n.aspect;
                size_t foot = n.foot;
                writeBEbytes(ofs,reinterpret_cast<char*>(&(index)),sizeof(size_t));
                writeBEbytes(ofs,reinterpret_cast<char*>(&(isTerm)),sizeof(char));
                writeBEbytes(ofs,reinterpret_cast<char*>(&(head)),sizeof(size_t));
                writeBEbytes(ofs,reinterpret_cast<char*>(&(parent)),sizeof(size_t));
                writeBEbytes(ofs,reinterpret_cast<char*>(&(sibling)),sizeof(size_t));
                writeBEbytes(ofs,reinterpret_cast<char*>(&(lexhead)),sizeof(size_t));
                writeBEbytes(ofs,reinterpret_cast<char*>(&(type)),sizeof(size_t));
                writeBEbytes(ofs,reinterpret_cast<char*>(&(aspect)),sizeof(size_t));
                writeBEbytes(ofs,reinterpret_cast<char*>(&(foot)),sizeof(size_t));
            }
            
            for(size_t j=0;j<numNodes;++j) {
                char mark = trees[i].markers[j];
                writeBEbytes(ofs,reinterpret_cast<char*>(&mark),sizeof(char));
            }
            
        }
        
    }
    
    ~TreeData() {
        if(trees != NULL)
            delete[] trees;
        trees = NULL;
    }
    
    ParseTree* trees;
    size_t ntrees;
    
};

#endif
