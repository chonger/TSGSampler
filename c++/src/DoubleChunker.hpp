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

struct SampleData {
    ParseTree* tree;
    NodeOffset offset;
    size_t dataIndex;

    SampleData(ParseTree* tree_, NodeOffset offset_, size_t dataIndex_) :
        tree(tree_), offset(offset_), dataIndex(dataIndex_) {
    }
};

class DoubleChunker {
public:
    DoubleChunker(std::ifstream& ifs)
    {
        pcfg = new PCFG(ifs);
        
        readLEbytes(ifs,reinterpret_cast<char*>(&numTreeSets),sizeof(size_t));
        for(size_t i=0;i<numTreeSets;++i) {
            treeData.push_back(new TreeData(ifs));
        }
        
        hdp = new HDP(pcfg,ifs,0,0);

        //read mixture weights
        for(size_t i=0;i<numTreeSets;++i) {
            std::vector<std::vector<double> > treeToDP;
            for(size_t j=0;j<hdp->numDP;++j) {
                std::vector<double> toSym;
                for(size_t j=0;j<pcfg->nLHS;++j) {
                    double w = 0;
                    readLEbytes(ifs,reinterpret_cast<char*>(&w),sizeof(double));
                    toSym.push_back(w);
                }
                treeToDP.push_back(toSym);
            }
            mixWeights.push_back(treeToDP);
        }

        /**
         * Initialize the lhs counts of the base and derived DPs
         * to do this just add the trees in with their current aspect
         * 
         */

        for(std::vector<TreeData*>::iterator iter = treeData.begin();
            iter != treeData.end(); ++iter) {

            TreeData* td = *iter;
            
            for(size_t i=0;i<td->ntrees;++i) {
                ParseTree& pt = td->trees[i];
                
                for(size_t j=0;j<pt.size;++j) {
                    if(pt.markers[j]) {
                        Segment seg(&pt,j);
                        for(size_t k=0;k<pt.size;++k) {
                            if(seg.warps[k] != 0)
                                throw "!";
                        }
                        hdp->insertTo(pt.nodelist[j].aspect,seg);
                    }
                }
            }
        }

        //add samples
        //only add the samples for the specific data
        for(size_t k=0;k<treeData.size();++k) {

            TreeData* td = treeData[k];
            
            for(size_t i=0;i<td->ntrees;++i) {
                ParseTree& pt = td->trees[i];

                for(size_t j=0;j<pt.size;++j) {
                    samples.push_back(SampleData(&(pt),j,k));            
                }
            }
        }
        
        
    }
    
    ~DoubleChunker() {
        for(std::vector<TreeData*>::iterator iter = treeData.begin();
            iter != treeData.end(); ++iter) {
            delete (*iter);
        }
        treeData.clear();
        delete hdp;
        hdp = NULL;
        delete pcfg;
        pcfg = NULL;
    }

    void sampleNode(SampleData& sample);
    
    void resample(size_t iterations);

    void packResults(const char* filename);

    std::ofstream outstream;

    bool stop;
    
private:

    char sampleAspect(std::vector<double>&,double);

    std::vector<SampleData> samples;

    size_t numTreeSets;
    PCFG* pcfg;
    HDP* hdp;
    std::vector<TreeData*> treeData;
    //for each tree set, set the weights for each NP
    std::vector<std::vector<std::vector<double> > > mixWeights;

};




#endif
