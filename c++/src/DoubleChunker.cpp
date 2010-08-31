#include "DoubleChunker.hpp"

using namespace std;

char DoubleChunker::sampleAspect(std::vector<double>& scores, double total) {
    float randval = (rand() / ((float) RAND_MAX + 1));
    double runTotal = 0.0;
    //printf("SAMPLE ASPECT - %f\n",total);
    for(size_t i=0;i<scores.size();++i) {
        //printf("PART - %f\n",scores[i]);
        runTotal += scores[i] / total;
        if(randval < runTotal)
            return i;
    }
    throw "!";
}

void DoubleChunker::sampleNode(ParseTree* tree, NodeOffset offset) {

    TreeNode& node = tree->nodelist[offset];
    NodeOffset headOffset = offset - node.head;
    TreeNode& head = tree->nodelist[headOffset];

    size_t headLHS = dblData->lhsMap[head.index];
    size_t nodeLHS = dblData->lhsMap[node.index];
    
    bool wasCut = tree->markers[offset];
    
    tree->markers[offset] = true; //pretend to split at the offset

    ///        GET TOP INFO
    Segment top = Segment(tree,headOffset);
    Segment bottom = Segment(tree,offset);

    /**
    size_t treeKronDel = top.equals(bottom) ? 1 : 0;
    size_t lhsKronDel = dblData->lhsMap[head.index] == dblData->lhsMap[node.index] ? 1 : 0;
    */
    
    ///         GET JOIN INFO
    tree->markers[offset] = false; //this will make the iterator find the join segment
    Segment join = Segment(tree,headOffset);

    if(wasCut) {
        //remove top and bottom
        hdp->removeFrom(head.aspect,top);
        hdp->removeFrom(node.aspect,bottom);
    } else {
        //remove join
        hdp->removeFrom(head.aspect,join);
    }

    std::vector<double> scoresJ;
    double jTotal = 0.0;
    std::vector<double> scoresT;
    double tTotal = 0.0;
    std::vector<double> scoresB;
    double bTotal = 0.0;

    //TODO !!! these dont consider the kroneker deltas!
    
    for(size_t i=0;i<hdp->numMixture;++i) {
        double j = hdp->score(join,i) * hdp->mixWeights[headLHS][i];
        jTotal += j; 
        scoresJ.push_back(j);
        double t = hdp->score(top,i) * hdp->mixWeights[headLHS][i];
        tTotal += t;
        scoresT.push_back(t);
        double b = hdp->score(bottom,i) * hdp->mixWeights[nodeLHS][i];
        bTotal += b;
        scoresB.push_back(b);
    }
    
    double combProb = tTotal * bTotal;    
    double cutoff = combProb / (jTotal + combProb);
    float randval = (rand() / ((float) RAND_MAX + 1));

    //Decide which one to insert into, based on a their probabilities in each DP
    //be carefull about things like the kronDeltas for identical assignments    
    
    if(randval > cutoff) { //join, dont cut
        
        //node is already marked as not cut from above in this method

        //sample the aspect for join
        char asp = sampleAspect(scoresJ,jTotal);

        hdp->insertTo(asp,join);

        /**
         * to update the head offset in a join,
         * everything in the bottom segment will now pointing to headOffset
         * this includes stub nodes at the leaves of the segment
         * so add in the difference
         */   
        for(Segment::iterator iter = bottom.begin();iter != bottom.end(); ++iter) {
            iter.n->head = iter.offset - headOffset;
        }
        
        head.aspect = asp;
        
    } else { //
        //the marker was set to false in this method, so set it true
        tree->markers[offset] = true;

        char aspT = sampleAspect(scoresT,tTotal);
        char aspB = sampleAspect(scoresB,bTotal);

        hdp->insertTo(aspT,top);
        hdp->insertTo(aspB,bottom);

        /**
         * To update the head offsets after a cut, everything but the first node
         * in the bottom segment should be set to bottom's offset
         *
         */ 
        NodeOffset newHead = offset;
        for(Segment::iterator iter = ++bottom.begin();iter != bottom.end(); ++iter) {
            iter.n->head = (iter.offset - newHead);
        }

        node.aspect = aspB;
        head.aspect = aspT;          
    }       
}


void DoubleChunker::packResults(const char* filename) {
    
    printf("Writing data to %s\n",filename);
    std::ofstream ofs(filename,std::ios::binary);
    
    dblData->write2Data(ofs);
    
    hdp->write(ofs);
    
    ofs.close();
}

void DoubleChunker::resample(size_t iterations) {
    
    for(size_t i=0;i<iterations;++i) {
        if(stop)
            return;

        clock_t start = clock();

        //shuffle
        for(size_t j=1;j<samples.size() - 1;++j) {
            size_t ind = rand() % (samples.size() - j);
            pair<ParseTree*,NodeOffset> tmp = samples[ind];
            samples[ind] = samples[samples.size() - j];
            samples[samples.size() - j] = tmp;
        }
        
        printf("Sampling, %d samples\n",samples.size());

        for(std::vector<std::pair<ParseTree*,NodeOffset> >::iterator iter = samples.begin();
            iter != samples.end(); ++iter) {
            sampleNode(iter->first,iter->second);
        }

        hdp->resampleParams();
        //specDP->resampleParams();
        //backDP->resampleParams();
        
        //double logl = dp->logLikelihood(tData->trees,tData->ntrees);

        clock_t finish = clock();
        printf("Iteration %d took %f seconds\n",i,(double(finish) - double(start))/CLOCKS_PER_SEC);
        //printf("Log Likelihood           : %f\n",logl);
        //printf("Number of changed labels : %d out of %d\n",numChanged,totalLabels);
        
        //outstream << logl << "\n";
    }
}
