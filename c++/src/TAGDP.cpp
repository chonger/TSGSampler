#include "TAGDP.hpp"

//all the samplers/chunkers do this...
void TAGSampler::resample(size_t iterations) {

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

        
        resampleParams();
        
        double logl = dp->logLikelihood(tData->trees,tData->ntrees);

        clock_t finish = clock();

        printf("Iteration %d took %f seconds\n",i,(double(finish) - double(start))/CLOCKS_PER_SEC);
        printf("Log Likelihood           : %f\n",logl);
        
        outstream << logl << "\n";
        
    }
    
}

//this one should be overridden by the individual sampler
void TAGSampler::sampleNode(ParseTree* tree, NodeOffset offset) {

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

    //wait on removals...
    /**
    if(wasCut) {
        //remove top and bottom
        removeSeg(top);
        removeSeg(bottom);
    } else {
        //remove join
        removeSeg(join);
    }
    */
    
    //the 6 options are join as a tag, join as a tsg, split with top and bottom in 4 ways

    //there must be some tsg rule up the tree and down the tree
    
    //this is easy...
    double joinTSG = tsgScore(join);

    //for this one, remove the supertop AND superbottom
    double joinTAG = tagScore(join);

    //easy again
    double topTSG = tsgScore(top);

    //remove supertop and bottom
    double topTAG = tagScore(top);

    
    //BIG TODO : when scoring the bottom, need to include krondeltas
    //easy
    double bottomTSG = tsgScore(bottom);
    double bottomTAG = tagScore(bottom);

    //also the TAG TAG combo is tricky...it should be the top sandwich plus the top plus whats after
    double tagtag = tagtag(top,bottom);

    std::vector<double> options;
    options.push_back(joinTSG);
    options.push_back(joinTAG);
    options.push_back(topTSG * bottomTAG);
    options.push_back(topTSG * bottomTSG);
    options.push_back(tagtag);
    options.push_back(topTAG * bottomTSG);
            
    double total = 0;
    for(size_t i=0;i<options.size();++i) {
        total += options[i];
    }
    float randval = (rand() / ((float) RAND_MAX + 1));
    double runTotal = 0;
    for(size_t i=0;i<options.size();++i) {
        if(options[i] == 0.0)
            continue;
        runTotal += options[i] / total;
        if(randval < total) {
            switch (i) {
                case 0:
                    insertTSG(join);
                    break;
                case 1:
                    insertTAG(join);
                    break;
                case 2:
                    insertTSG(top);
                    insertTAG(bottom);
                    break;
                case 3:
                    insertTSG(top);
                    insertTSG(bottom);
                    break;
                case 4:
                    insertTAG(top);
                    insertTAG(bottom);
                    break;
                case 5:
                    insertTAG(top);
                    insertTSG(bottom);
                    break;
                default:
                    throw "!";
            }
        }
    }
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

void TAGSampler::resampleParams() {

    //resample alphas for tag and tsg
    //resample both base distributions
    //resample tagmixture
    
}
