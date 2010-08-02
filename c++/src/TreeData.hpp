#ifndef TSG_TREEDATA
#define TSG_TREEDATA 1

struct TreeData {

    TreeData(size_t* lhsmap_, double* pcfg_,size_t numRules_,
             ParseTree* trees_, size_t ntrees_, size_t nLHS_) :
            lhsMap(lhsmap_),
            pcfg(pcfg_), numRules(numRules_),
            trees(trees_), ntrees(ntrees_), nLHS(nLHS_)
    {
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

    ~TreeData() {
        delete[] pcfg;
        delete[] trees;
        delete[] lhsMap;
        delete[] lhsTotals;
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


#endif
