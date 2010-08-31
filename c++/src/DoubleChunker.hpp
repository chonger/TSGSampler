#ifndef ENBUSKE_2CHUNK
#define ENBUSKE_2CHUNK 1

#include "ParseTree.hpp"
#include "TreeData.hpp"
#include "DP.hpp"
#include <vector>
#include <math.h>
#include <ctime>
#include <fstream>
#include <iostream>
#include <boost/thread.hpp>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include "HDP.hpp"

class DoubleChunker {
public:
    DoubleChunker(std::ifstream& ifs)
    {

        dblData = new DoubleData(ifs);
        hdp = new HDP(dblData,ifs,2,0,0);

        //only add the samples for the specific data
        for(size_t i=dblData->backGend + 1;i<dblData->ntrees;++i) {
            ParseTree& pt = dblData->trees[i];
            
            for(size_t j=0;j<pt.size;++j) {
                samples.push_back(std::make_pair(&(pt),j));            
            }
        }


        
    }
    
    ~DoubleChunker() {
        delete hdp;
        hdp = NULL;
        delete dblData;
        dblData= NULL;
    }

    void sampleNode(ParseTree* tree, NodeOffset offset);
    
    void resample(size_t iterations);

    void packResults(const char* filename);

    std::ofstream outstream;

    bool stop;
    
private:

    char sampleAspect(std::vector<double>&,double);

    std::vector<std::pair<ParseTree*,NodeOffset> > samples;
    
    HDP* hdp;
    DoubleData *dblData;
    
};


#endif
