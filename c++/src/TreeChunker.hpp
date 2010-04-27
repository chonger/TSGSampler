#ifndef TSG_TREECHUNKER
#define TSG_TREECHUNKER 1

#include "ParseTree.hpp"
#include "SequentialSample.hpp"
#include <google/dense_hash_map>
#include <vector>
#include <math.h>
#include <ctime>

using namespace std;

typedef google::dense_hash_map<Segment,size_t,SegmentHash,SegmentEq> TreeHashMap;

class TreeChunker {
public:
    
    TreeChunker(size_t* rhsmap_, double* pcfg_,
                double* beta_,double* alpha_,
                ParseTree* trees_, size_t ntrees_, size_t nRHS_);
    
    ~TreeChunker() {
        delete[] pcfg;
        delete[] beta;
        delete[] alpha;
        delete[] trees;
        delete[] rhsMap;
        delete[] rhsCounts;
        delete nulltree1;
        delete nulltree2;
        delete[] rhsTotals;
        delete[] chart;
    }

    void resample(int iterations, double smoothS, double smoothF);

    void packResults(const char* filename);
    
private:
    
    void resample(double smooth);
    void resampleTrees();
    void resampleBeta();
    void sampleNode(ParseTree* tree, NodeOffset offset, double smooth);

    //sequential sampling
    void sampleTree(ParseTree* tree, double smooth);
    SeqSample sampleFrom(ParseTree& tree, NodeOffset node, double smooth);
    SeqSample sampleTop(ParseTree& tree, NodeOffset node, double smooth, std::vector<NodeOffset>& leaves);
    SampleDist* chart;
    static size_t samplesPerDist;

    /**
     * The probability of an elementary tree under the DP
     */ 
    double scoreDP(Segment& seg, double smooth);

    
    /**
     * The probability of an elementary tree under the base distribution
     */ 
    double score(Segment& seg);

    void shuffle();

    double logLikelihood();
    double segmentationP(ParseTree& tree);

    void resampleAlpha();
    std::pair<double,double> getLNMeanVar(double d, double variance);
    double evalGammaPosterior(double d, double gamma_a, double gamma_b, double k, double n);
    
    TreeHashMap treemap;

    size_t* rhsMap;
    size_t* rhsCounts;
    
    //for base prob dist
    double* pcfg;
    double* beta;

    double* alpha;
    ParseTree* trees;
    size_t ntrees;
    size_t nRHS;
    size_t* rhsTotals;
    
    vector<pair<ParseTree*,NodeOffset> > samples;

    ParseTree* nulltree1;
    ParseTree* nulltree2;


    size_t acceptCount;
    size_t acceptTotal;

    
};


#endif
