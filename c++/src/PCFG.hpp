#ifndef ENBUSKE_PCFG
#define ENBUSKE_PCFG

#include <iostream>
#include "Util.hpp"

class PCFG {
public:

    PCFG(std::ifstream& ifs) {
        
        readLEbytes(ifs,reinterpret_cast<char*>(&numRules),sizeof(size_t));
        printf("%d rules\n",numRules) ;
        
        //read pcfg probs
        probs = new double[numRules];
        for(size_t i=0;i<numRules;++i) {
            readLEbytes(ifs,reinterpret_cast<char*>(probs + i),sizeof(double));
            //printf("PCFG PROB = %f\n",probs[i]);
        }
        
        //read head indexes
        lhsMap = new size_t[numRules];
        for(size_t i=0;i<numRules;++i) {
            readLEbytes(ifs,reinterpret_cast<char*>(lhsMap + i),sizeof(size_t));
            //printf("LHS Index = %d\n",lhsMap[i]);
        }
        
        readLEbytes(ifs,reinterpret_cast<char*>(&nLHS),sizeof(size_t));
        printf("%d NTs\n",nLHS);

        //a check for overrunning nLHS
        for(size_t i=0;i<numRules;++i) {
            if(lhsMap[i] >= nLHS) {
                printf("Bad lhs = %d\n",i);
                throw -1;
            }
        }

    }
    
    void write(std::ofstream& ofs ) {
        
        writeBEbytes(ofs,reinterpret_cast<char*>(&(numRules)),sizeof(size_t));
        
        //write pcfg probs
        for(size_t i=0;i<numRules;++i) {
            writeBEbytes(ofs,reinterpret_cast<char*>(probs + i),sizeof(double));
        }
        
        //write head indexes
        for(size_t i=0;i<numRules;++i) {
            writeBEbytes(ofs,reinterpret_cast<char*>(lhsMap + i),sizeof(size_t));
        }
                
        writeBEbytes(ofs,reinterpret_cast<char*>(&(nLHS)),sizeof(size_t));
        
    }

    ~PCFG() {
        if(probs != NULL) 
            delete[] probs;
        probs = NULL;

        if(lhsMap != NULL)
            delete[] lhsMap;
        lhsMap = NULL;
    }

    size_t* lhsMap;
    double* probs;
    size_t numRules;
    size_t nLHS;

private:

    PCFG() {}
    PCFG(const PCFG& o) {}
    
};


#endif
