#include "ParseTree.hpp"
#include "DoubleChunker.hpp"
#include "TAGDP.hpp"
#include "DP.hpp"
#include <stdio.h>
#include <string>
#include <fstream>
#include <ctime>
#include <fstream>
#include <iostream>
#include <signal.h>

TAGSampler* chunker = NULL;
std::string outpath;

void abortFunc(int sig) {
    if(chunker != NULL)
        chunker->stop = true;
}

//should make this entry point called Enbuske and have it select the type of chunker.

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
    
    printf("Reading packed TAG/TSG from %s\n",filename);

    chunker = new TAGSampler(ifs); 

    ifs.close();            
    
    outpath = argv[2];
    
    printf("Starting to Sample\n");

    //chunker->outstream.open(argv[3],std::ios::app);

    chunker->outstream.open(argv[3]);

    size_t nIter = atoi(argv[4]);
    
    printf("SCHEDULE - %d iterations with smoothing disabled\n",nIter);
        
    chunker->resample(nIter);

    chunker->outstream.close();
    chunker->packResults(outpath.c_str());
    delete chunker;
    chunker = NULL;

    return 0;
}
