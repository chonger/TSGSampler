#include "ParseTree.hpp"
#include "TreeChunker.hpp"
#include <stdio.h>
#include <string>
#include <google/dense_hash_map>


//typedef google::dense_hash_map<Segment,int,SegmentHash,SegmentEq> TreeHashMap;

int main() {
    srand(1234);
    
    std::string rules[5];
    rules[0] = "R -> S";
    rules[1] = "S -> A B";
    rules[2] = "A -> c";
    rules[3] = "B -> S";
    rules[4] = "B -> d";

    size_t* rhsmap = new size_t[5];
    rhsmap[0] = 0;
    rhsmap[1] = 1;
    rhsmap[2] = 2;
    rhsmap[3] = 3;
    rhsmap[4] = 3;

    double* pcfg = new double[5];
    pcfg[0] = 1.0;
    pcfg[1] = 1.0;
    pcfg[2] = 1.0;
    pcfg[3] = .4;
    pcfg[4] = .6;

    double* beta = new double[4];
    beta[0] = .5;
    beta[1] = .5;
    beta[2] = .5;
    beta[3] = .5;


    double* alpha = new double[4];
    alpha[0] = 10;
    alpha[1] = 10;
    alpha[2] = 10;
    alpha[3] = 10;

    
    TreeNode* nodes = new TreeNode[7];
    nodes[0] = TreeNode(0,false,0,0,0); //root
    nodes[1] = TreeNode(1,false,1,1,0);
    nodes[2] = TreeNode(2,true,2,1,1);
    nodes[3] = TreeNode(3,false,3,2,0);
    nodes[4] = TreeNode(1,false,4,1,0);
    nodes[5] = TreeNode(2,true,5,1,1);
    nodes[6] = TreeNode(4,true,6,2,0);

    bool* markers = new bool[7];
    markers[0] = true;
    markers[1] = false;
    markers[2] = false;
    markers[3] = false;
    markers[4] = false;
    markers[5] = false;
    markers[6] = false;


    ParseTree tree(nodes,markers,7);
    delete[] nodes;
    delete[] markers;
    ParseTree* pt = new ParseTree[1];
    pt[0] = tree;
    
    TreeChunker chunker(rhsmap,pcfg,beta,alpha,pt,1,4);

    chunker.resample(1.0);

    /**
    printf("Creating segref\n");
    
    ParseTree nulltree = ParseTree(NULL,NULL,0);
    TreeHashMap treemap = TreeHashMap();
    treemap.set_empty_key(Segment(&nulltree,(NodeOffset)0));
    for(int i=0;i<7;++i) {
        Segment sr = Segment(&pt,i);
        printf("Walking tree\n");
        for(Segment::iterator iter = sr.begin(); iter != sr.end(); ++iter) {        
            printf("%s\n",rules[iter.n->index].c_str());
        }
        printf("HASH = %d\n",sr.hashCode());
        treemap.insert(std::make_pair(sr,1));
    }

    for(TreeHashMap::iterator iter = treemap.begin(); iter != treemap.end(); ++iter) {
        const Segment& sr = iter->first;
        for(Segment::iterator iter2 = sr.begin(); iter2 != sr.end(); ++iter2) {
            printf("%s\n",rules[iter2.n->index].c_str());
        }
        printf("%d\n",iter->second);
        iter->second = 2;
    }
    for(TreeHashMap::iterator iter = treemap.begin(); iter != treemap.end(); ++iter) {
        const Segment& sr = iter->first;
        for(Segment::iterator iter2 = sr.begin(); iter2 != sr.end(); ++iter2) {
            printf("%s\n",rules[iter2.n->index].c_str());
        }
        printf("%d\n",iter->second);
    }
    */
    
    
    return 0;
}
