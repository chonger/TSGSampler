#include "ParseTree.hpp"
#include "TreeChunker.hpp"
#include "DP.hpp"
#include <stdio.h>
#include <string>
#include <fstream>
#include <ctime>
#include <fstream>
#include <iostream>
#include <signal.h>

TreeChunker* chunker = NULL;
std::string outpath;

void abortFunc(int sig) {
    if(chunker != NULL)
        chunker->stop = true;
}


struct ScheduleItem {
    size_t nIter;
    double smoothS;
    double smoothE;

    ScheduleItem(size_t n, double s, double e) :
        nIter(n), smoothS(s), smoothE(e)  
    {}


};


int main(int argc, const char* argv[]) {
    srand(time(NULL));
    
    
    signal(SIGABRT,&abortFunc);
    signal(SIGTERM,&abortFunc);
    signal(SIGINT,&abortFunc);
    
    const char* filename = argv[1];

    std::ifstream ifs(filename,std::ios::binary);
    if(!ifs.is_open()) {
        printf("Invalid file at %s\n",filename);
        exit(-2);
    }
    
    printf("Reading packed TSG from %s\n",filename);

    TreeData* tData = new TreeData(ifs);


    
    //TreeData* tData = new TreeData(lhsmap,probs,numRules,ptrees,numTrees,numLHS);    

    double headCut = atoi(argv[5]);
    double noTag = atoi(argv[6]);

    printf("%E %E\n",headCut,noTag);
    
    chunker = new TagChunker(tData,ifs,3,headCut,noTag); 


    
    ifs.close();            
    
    outpath = argv[2];
    
    printf("Starting to Sample\n");

    if(argc > 5 && std::string(argv[5]) == "cont")
        chunker->outstream.open(argv[3],std::ios::app);
    else
        chunker->outstream.open(argv[3]);


    //read schedule
    std::vector<ScheduleItem> schedule;

    std::ifstream sfs(argv[4]);
    
    while(sfs) {
        
        int nIter;
        double st;
        double en;

        sfs >> nIter;
        if(sfs.eof())
            break;
        sfs >> st;
        sfs >> en;

        printf("SCHEDULE - %d iterations with smoothing disabled\n",nIter);
        
        ScheduleItem item(nIter,st,en);
        
        schedule.push_back(item);
    }
    sfs.close();
    
    for(std::vector<ScheduleItem>::iterator it = schedule.begin(); it != schedule.end(); ++it) {
        chunker->resample(it->nIter);
    }

    chunker->outstream.close();
    chunker->packResults(outpath.c_str());
    delete chunker;
    chunker = NULL;

    return 0;
}
