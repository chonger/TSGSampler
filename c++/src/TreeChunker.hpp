#ifndef TSG_TREECHUNKER
#define TSG_TREECHUNKER 1

#include "ParseTree.hpp"
#include "TreeData.hpp"
#include "DP.hpp"
#include <vector>
#include <math.h>
#include <ctime>
#include <fstream>
#include <iostream>
#include <boost/thread.hpp>


class ChunkThread {
public:

    ChunkThread(DP* dp_, TreeData* tData_, std::vector<std::pair<ParseTree*,NodeOffset> >& samples_) :
        dp(dp_), tData(tData_), samples(samples_) {
        
    }

    void go() {
        
        m_thread = boost::shared_ptr<boost::thread>(new boost::thread(boost::bind(&ChunkThread::dowork, this)));
    }

    void dowork() {
        printf("THREAD STARTED!, %d samples\n",samples.size());
        //resample cut labels
        for(std::vector<std::pair<ParseTree*,NodeOffset> >::iterator iter = samples.begin();
            iter != samples.end(); ++iter) {
            sampleNode(iter->first,iter->second);
        }
        
        printf("THREAD FINISHED!\n");
    }

    void sampleNode(ParseTree* tree, NodeOffset offset);
    

    boost::shared_ptr<boost::thread> m_thread;
    DP* dp;
    TreeData* tData;
    std::vector<std::pair<ParseTree*,NodeOffset> >& samples;
};

class TreeChunker {
public:
    
    TreeChunker(TreeData* tData_,std::ifstream& ifs,size_t nThreads);
    
    ~TreeChunker() {
        delete tData;
        delete dp;
        for(size_t i=0;i<nThreads;++i) {
            delete threads[i];
        }
    }

    void resample(size_t iterations);

    void packResults(const char* filename);

    std::ofstream outstream;

    bool stop;

    DP* dp;
    TreeData* tData;


protected:

    void shuffleSamples();
    std::vector<std::vector<std::pair<ParseTree*,NodeOffset> > > samples;
    std::vector<ChunkThread*> threads;


    size_t nThreads;
    
private:
    

    //EXTRA MONITORING VARIABLES
    
    size_t acceptCount;
    size_t acceptTotal;
    double avgAcc;
    double avgP;
    double pTotal;

    size_t numChanged;
    size_t totalLabels;

    
};

class NormalChunker : public TreeChunker {
public:
    NormalChunker(TreeData* tData_, std::ifstream& ifs, size_t nThreads_) :
        TreeChunker(tData_,ifs,nThreads_) {
                
        dp = new NormalDP(tData,ifs);
        
        for(size_t i=0;i<nThreads;++i) {
            DP* copy = new CopyDP(dp);
            threads.push_back(new ChunkThread(copy,tData,samples[i]));            
        }
    }
};


class TagChunker : public TreeChunker {
public:
    TagChunker(TreeData* tData_, std::ifstream& ifs, size_t nThreads_,double headCut, double noTag) :
        TreeChunker(tData_,ifs,nThreads_) {
        
        dp = new TagDP(tData,ifs,headCut,noTag);
        dp->fromTreeData();
        
        for(size_t i=0;i<nThreads;++i) {
            DP* copy = new CopyDP(dp);
            threads.push_back(new ChunkThread(copy,tData,samples[i]));            
        }
    }
};


class DoubleChunker {
public:
    DoubleChunker(std::ifstream& ifs)
    {
        printf("Constructing Double Chunker\n");
        
        //read the background
        dblData = new DoubleData(ifs);
                
        //read the main DP
        backDP = new BackDP(dblData,ifs);
        backDP->fromTreeData();
        
        //read the spec DP
        specDP = new SpecDP(dblData,ifs);
        specDP->fromTreeData();

        for(size_t i=0;i<dblData->nLHS;++i) {
            readLEbytes(ifs,reinterpret_cast<char*>(specInd + i),sizeof(double));
        }
        
        for(size_t i=dblData->backGend + 1;i<dblData->ntrees;++i) {
            ParseTree& pt = dblData->trees[i];
            
            for(size_t j=0;j<pt.size;++j) {
                if(j > 0) //we never need to resample the root
                    samples.push_back(std::make_pair(&(pt),j));            
            }
        }

        specInd = new double[dblData->nLHS];
        for(size_t i=0;i<dblData->nLHS;++i) {
            specInd[i] = .5;
        }

    }
    
    ~DoubleChunker() {
        delete specDP;
        specDP = NULL;
        delete backDP;
        backDP = NULL;
        delete dblData;
        dblData= NULL;
        delete specInd;
        specInd = NULL;
    }

    void sampleNode(ParseTree* tree, NodeOffset offset);
    
    void resample(size_t iterations);

    void packResults(const char* filename);

    std::ofstream outstream;

    bool stop;
    
private:
    size_t getCount(TreeHashMap::iterator& iter, DP* map, bool inMap, bool minus);
    char sampleAspect(double p1, double p2);
    void minusFromMap(TreeHashMap::iterator& iter, DP* map, size_t head);
    bool addToMap(TreeHashMap::iterator& iter, DP* dp, size_t head);

    std::vector<std::pair<ParseTree*,NodeOffset> > samples;
    DP *specDP,*backDP;
    DoubleData *dblData;

    double *specInd;
    
};




#endif
