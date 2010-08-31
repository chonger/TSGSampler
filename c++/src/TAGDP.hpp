#ifndef ENBUSKE_TAG
#define ENBUSKE_TAG 1

#include "DP.hpp" //only here for TreeHashMap
#include <fstream>
#include <iostream>

/***
 *
 * IDEA - Resampling the alpha parameter serves to smooth our observation
 * with a base distribution.  Kneser-Ney does the same thing.  Can we use
 * the kneser ney discounting thing to resample alpha?  This would be easier
 *
 * Golf N' Stuff
 * 
 */ 

class TAGSampler {
public:

    /**
     * FORMAT - 
     *
     * TreeData
     * TSG Alphas
     * TAG Alphas
     * TAG Mixture
     * TSG Base Betas
     * TAG Base Betas
     * 
     */ 
    
    TAGSampler(std::ifstream& ifs) {

        tData = new TreeData(ifs);
        
        nulltree1 = new ParseTree(NULL,NULL,0);
        nulltree2 = new ParseTree(NULL,NULL,0);
        tsg.set_empty_key(Segment(nulltree1,(NodeOffset)0));
        tsg.set_deleted_key(Segment(nulltree2,(NodeOffset)0));
        tag.set_empty_key(Segment(nulltree1,(NodeOffset)0));
        tag.set_deleted_key(Segment(nulltree2,(NodeOffset)0));

        //read alphas
        tsgAlpha = new double[tData->nLHS];
        for(size_t i=0;i<tData->nLHS;++i) {
            readLEbytes(ifs,reinterpret_cast<char*>(tsgAlpha + i),sizeof(double));
        }
        tagAlpha = new double[tData->nLHS];
        for(size_t i=0;i<tData->nLHS;++i) {
            readLEbytes(ifs,reinterpret_cast<char*>(tagAlpha + i),sizeof(double));
        }

        //read tag mixtures
        tagMixture = new double[tData->nLHS];
        for(size_t i=0;i<tData->nLHS;++i) {
            readLEbytes(ifs,reinterpret_cast<char*>(tagMixture + i),sizeof(double));
        }
        
        //initialize lhs counts (these are used only in alpha resampling!)
        tsgLHSCounts = new double[tData->nLHS];
        tagLHSCounts = new double[tData->nLHS];

        tsgBase = new HBase(tData,ifs,baseLHSCounts,headCut,noTag);
        tagBase = new HBase(tData,ifs,baseLHSCounts,headCut,noTag);
        
    }

    ~TAGSampler() {
        delete tData;
        tData = NULL;
        delete nulltree1;
        nulltree1 = NULL;
        delete nulltree2;
        nulltree2 = NULL;
        delete tsgLHSCounts;
        tsgLHSCounts = NULL;
        delete tagLHSCounts;
        tagLHSCounts = NULL;
        delete tsgAlpha;
        tsgAlpha = NULL;
        delete tagAlpha;
        tagAlpha = NULL;
        delete tagBASE;
        tagBase = NULL;
        delete tsgBASE;
        tsgBase = NULL;
        delete tagMixture;
        tagMixture = NULL;
    }

    void packResults(const char* filename) {
        printf("Writing data to %s\n",filename);
        std::ofstream ofs(filename,std::ios::binary);
        
        tData->write2Data(ofs);

        //write alphas
        for(size_t i=0;i<tData->nLHS;++i) {
            writeBEbytes(ofs,reinterpret_cast<char*>(tagAlpha + i),sizeof(double));
        }
        for(size_t i=0;i<tData->nLHS;++i) {
            writeBEbytes(ofs,reinterpret_cast<char*>(tsgAlpha + i),sizeof(double));
        }
        for(size_t i=0;i<tData->nLHS;++i) {
            writeBEbytes(ofs,reinterpret_cast<char*>(tagMixture + i),sizeof(double));
        }
        
        tagBase->write(ofs);
        tsgBase->write(ofs);
    
        ofs.close();
    }

    //if its true then its a tag
    bool sampleTag(size_t lhs) {
        float randval = (rand() / ((float) RAND_MAX + 1));
        return(randval > tagMixture[lhs])
    }

    void resampleParams();
    
    void resample(size_t iterations);

    std::ofstream outstream;

    bool stop;
    
private:

    void remove(Segment& seg) {
        
    }

    void insertTAG(Segment& seg) {

    }

    void insertTSG(Segment& seg) {

    }

    double tagScore(Segment& seg) {
        return 0;
    }

    double tsgScore(Segment& seg) {
        return 0;
    }
    
    void sampleNode(ParseTree* tree, NodeOffset offset);

    ParseTree* nulltree1;
    ParseTree* nulltree2;

    TreeHashMap tsg;
    TreeHashMap tag;

    double* tsgLHSCounts;
    double* tsgAlpha;
    double* tagLHSCounts;
    double* tagAlpha;

    double* tagMixture
    
    BASE* tagBase;
    BASE* tsgBase;
    
    TreeData *tData;
    
};

#endif
