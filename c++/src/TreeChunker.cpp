#include "TreeChunker.hpp"
#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <fstream>

size_t TreeChunker::samplesPerDist = 10;

TreeChunker::TreeChunker(size_t* rhsmap_, double* pcfg_,
            double* beta_,double* alpha_,
            ParseTree* trees_, size_t ntrees_, size_t nRHS_) :
    rhsMap(rhsmap_), rhsCounts(NULL), pcfg(pcfg_), beta(beta_),
    alpha(alpha_), trees(trees_), ntrees(ntrees_), nRHS(nRHS_) {
    
    nulltree1 = new ParseTree(NULL,NULL,0);
    nulltree2 = new ParseTree(NULL,NULL,0);
    
    treemap.set_empty_key(Segment(nulltree1,(NodeOffset)0));
    treemap.set_deleted_key(Segment(nulltree2,(NodeOffset)0));
    
    rhsTotals = new size_t[nRHS];
    rhsCounts = new size_t[nRHS];
    for(size_t i=0;i<nRHS;++i) {
        rhsTotals[i] = 0;
        rhsCounts[i] = 0;
    }
    
    for(size_t i=0;i<ntrees;++i) {
        ParseTree& pt = trees[i];

        for(size_t j=0;j<pt.size;++j) {
            
            size_t rhsInd = rhsMap[pt.nodelist[j].index];
            rhsTotals[rhsInd] += 1;
            
            if(j > 0) //we never need to resample the root
                samples.push_back(make_pair(&(pt),j));
            if(pt.markers[j]) {
                rhsCounts[rhsInd] += 1;
                Segment seg(&pt,j);
                TreeHashMap::iterator findo = treemap.find(seg);
                if(findo == treemap.end())
                    treemap.insert(make_pair(seg,1));
                else
                    findo->second += 1;
            }
        }
    }
    
}

void TreeChunker::resample(int iterations, double smoothS, double smoothF) {
    double gap = smoothF - smoothS;
    for(int i=0;i<iterations;++i) {
        double smooth = smoothS + ((double) i / (double) iterations) * gap;
        
        clock_t start = clock();
        resample(smooth);
        resampleBeta();
        clock_t finish = clock();
        printf("Iteration %d took %f seconds\n",i,(double(finish) - double(start))/CLOCKS_PER_SEC);
        printf("Log Likelihood = %f\n",logLikelihood());
    }
}
    
void TreeChunker::resample(double smooth) {
    
    shuffle();
    
    //printf("TOTAL = %d\n",samples.size());
    //size_t i=0;

    
    for(vector<pair<ParseTree*,NodeOffset> >::iterator iter = samples.begin();
        iter != samples.end(); ++iter) {
        //if(++i % 1000 == 0)
        //    printf("%d\n",i);
        sampleNode(iter->first,iter->second,smooth);
    }



    
    /**
    for(size_t i=0;i<ntrees;++i) {
        sampleTree(trees[i],smooth);
    }
    */
}

void TreeChunker::resampleAlpha() {

    double ALPHA_SIGSQ = 20;
    double GAMMA_A = 1;
    double GAMMA_B = 5;
    
    gsl_rng* r = gsl_rng_alloc(gsl_rng_taus);
    size_t numClasses[nRHS];
    for(size_t i=0;i<nRHS;++i) { numClasses[i] = 0; }
    for(TreeHashMap::iterator iter = treemap.begin(); iter != treemap.end(); ++iter) {
        TreeNode& n = iter->first.ptree->nodelist[iter->first.headIndex];
        numClasses[rhsMap[n.index]] += 1;
    }
    for(size_t i=0;i<nRHS;++i) {
        double curAlpha = alpha[i];
        size_t numC = numClasses[i];
        size_t total = rhsTotals[i];

        std::pair<double,double> curMV = getLNMeanVar(curAlpha,ALPHA_SIGSQ);
        double nextAlpha = gsl_ran_lognormal(r,curMV.first,curMV.second);
        std::pair<double,double> nextMV = getLNMeanVar(nextAlpha,ALPHA_SIGSQ);
        double qFrac = gsl_ran_lognormal_pdf(curAlpha,nextMV.first,nextMV.second);
        qFrac /= gsl_ran_lognormal_pdf(nextAlpha,curMV.first,curMV.second);
        double pFrac = evalGammaPosterior(nextAlpha,GAMMA_A,GAMMA_B,numC,total);
        pFrac /= evalGammaPosterior(curAlpha,GAMMA_A,GAMMA_B,numC,total);

        double accept = qFrac * pFrac;
        if(accept >= 1.0)
            alpha[i] = nextAlpha;
        else {
            double rando = ((double)rand() / (double) RAND_MAX);
            if(rando <= accept)
                alpha[i] = nextAlpha;
            //if not, then leave it be
        }
    }
}

std::pair<double,double> TreeChunker::getLNMeanVar(double d, double variance) {
    double v = log (variance / pow(d,2.0) + 1);
    double m = log(d) - v/2;
    return std::make_pair(m,v);
}

double TreeChunker::evalGammaPosterior(double d, double gamma_a, double gamma_b, size_t k, size_t n) {
    double ret = gsl_ran_gamma_pdf(d,gamma_a,gamma_b);
    ret *= pow(d,k-1);
    ret *= (d + n);
    ret *= gsl_ran_beta_pdf(d,d+1,n);
    return ret;
}

void TreeChunker::resampleBeta() {
    gsl_rng* r = gsl_rng_alloc(gsl_rng_taus);
    for(size_t i=0;i<nRHS;++i) {
        size_t expCount = rhsCounts[i];
        size_t nExpCount = rhsTotals[i] - expCount;
        beta[i] = gsl_ran_beta(r,1+expCount, 1+nExpCount);
    }
}

SeqSample TreeChunker::sampleTop(ParseTree& tree, NodeOffset nodeoff,
                                 double smooth, std::vector<NodeOffset>& leaves) {

    SeqSample ret(&tree,nodeoff);
    ret.markerMask[nodeoff] = true;
    ret.markers[nodeoff] = true;
    Segment seg(&tree,nodeoff);
    Segment::iterator iter = seg.begin();
    ++seg.begin();

    while(iter != seg.end()) {
        TreeNode node = tree.nodelist[iter.offset];
        //decide if this node is a cut point
        bool cut = true;
        if(!node.isTerminal) { //automatically a cut point if its a terminal
            if(rand() > .5)
                cut = false;
        }
        if(cut) {
            iter.stub = true;
            ret.markers[iter.offset] = true;
            ret.markerMask[iter.offset] = true;
        }
        ++iter;
    }
    
    return ret;
    
}

SeqSample TreeChunker::sampleFrom(ParseTree& tree, NodeOffset nodeoff, double smooth) {
    
    if(chart[nodeoff].samples == NULL) {
        
        TreeNode node = tree.nodelist[nodeoff];

        if(node.isTerminal) {
            SeqSample sam(&tree,nodeoff);
            chart[nodeoff] = SampleDist(1,&sam);  
        } else {
            SeqSample* sams = new SeqSample[samplesPerDist];
            for(size_t i=0;i<samplesPerDist;++i) { //set samplesPerDist samples
                std::vector<NodeOffset> leaves;
                //sample a tree top
                SeqSample sam = sampleTop(tree,nodeoff,smooth,leaves);
                for(size_t k=0;k<leaves.size();++k) {
                    //sample the distribution at each leaf
                    SeqSample subSam = sampleFrom(tree,leaves[k],smooth);
                    //push the sampled subtree's markers into this sample
                    for(size_t j=0;j<tree.size;++j) {
                        if(subSam.markerMask[j]) {
                            sam.markerMask[j] = true;
                            sam.markers[j] = subSam.markers[j];
                        }
                    }
                    //multiply in the probability of the subtree
                    sam.prob *= subSam.prob;
                }
                sams[i] = sam;
            }
            chart[nodeoff] = SampleDist(samplesPerDist,sams);  
        }
    }
    return chart[nodeoff].sample();
}


void TreeChunker::sampleTree(ParseTree& tree, double smooth) {

    chart = new SampleDist[tree.size];

    //save the original cut markings in case of rejection
    bool* originalMarkers = new bool[tree.size];
    for(size_t i=0;i<tree.size;++i) {
        originalMarkers[i] = tree.markers[i];
    }
    
    double originalP = segmentationP(tree);

    double originalQ = 1.0; //TODO calculate q(orig)

    
    /**
     * sequential monte carlo sampler produces a sample from an approximate
     * distribution q(x)
     */
    SeqSample sam = sampleFrom(tree,0,smooth);

    for(size_t i=0;i<tree.size;++i) {
        tree.markers[i] = sam.markers[i];
    }
    double newP = segmentationP(tree);
    double newQ = sam.prob;
    
    /**
     * use metropolis-hastings to decide acceptance (record acceptance rate!)
     */ 
    double a1 = newP / originalP;
    double a2 = originalQ / newQ;
    double a = a1 * a2;
    bool accept = true;
    if(a < 1) {
        if(rand() > a)
            accept = false;
    }

    //if we accept, the markers are already set
    if(!accept) {
        //restore original markers
        for(size_t i=0;i<tree.size;++i) {
            tree.markers[i] = originalMarkers[i];
        }
    }
 
    
    delete[] chart;
    chart = NULL;

}


void TreeChunker::sampleNode(ParseTree* tree, NodeOffset offset, double smooth) {
        
    TreeNode& node = tree->nodelist[offset];
    NodeOffset headOffset = offset - node.head;
    TreeNode& head = tree->nodelist[headOffset];
    
    size_t headRHS = rhsMap[head.index];
    size_t myRHS = rhsMap[node.index];
    size_t headRHSTotal = rhsCounts[headRHS];
    size_t myRHSTotal = rhsCounts[myRHS];
    double headAlpha = alpha[headRHS];
    double myAlpha = alpha[myRHS];
    
    bool wasCut = tree->markers[offset];
    
    tree->markers[offset] = true;
    
    Segment top = Segment(tree,headOffset);
    
    TreeHashMap::iterator topIter = treemap.find(top);
    size_t topCount = 0;
    if(wasCut) { //top must be in the map

#ifdef TSGDEBUG
        if(topIter == treemap.end()) {
            printf("TOP SHOULD BE IN THE MAP\n");
            throw 1;
        }
#endif
        
        topCount = (topIter->second) - 1;
    } else {
        if(topIter != treemap.end())
            topCount = topIter->second;
    }
    double topScore = score(top);
    
    Segment bottom = Segment(tree,offset);
    TreeHashMap::iterator bottomIter = treemap.find(bottom);
    size_t bottomCount = 0;
    if(wasCut) {//bottom must be in the map
        
#ifdef TSGDEBUG
        if(bottomIter == treemap.end()) {
            printf("BOTTOM SHOULD BE IN THE MAP\n");
            throw 1;
        }
#endif
        
        bottomCount = bottomIter->second - 1;
    }else {
        if(bottomIter != treemap.end())
            bottomCount = bottomIter->second;
    }
    double bottomScore = score(bottom);
    
    size_t treeKronDel = top.equals(bottom) ? 1 : 0;
    size_t rhsKronDel = rhsMap[head.index] == rhsMap[node.index] ? 1 : 0;
    
    tree->markers[offset] = false;
    
    Segment join = Segment(tree,headOffset);

#ifdef TSGDEBUG
    if(join.equals(top) || join.equals(bottom)) {
        printf("%d CANT HAVE JOIN = TOP|BOT\n",tree->index);
        top.printMe();
        bottom.printMe();
        join.printMe();
        throw("!");
    }
#endif

    TreeHashMap::iterator joinIter = treemap.find(join);
    size_t joinCount = 0;
    if(!wasCut) { //join must be in the map

#ifdef TSGDEBUG
        if(joinIter == treemap.end()) {
            printf("%d JOIN SHOULD BE IN THE MAP\n",tree->index);
            
            throw 1;
        }
#endif

        joinCount = joinIter->second - 1;
    } else {
        if(joinIter != treemap.end())
            joinCount = joinIter->second;
    }
    double joinScore = score(join);
    

    double unSmoothedJ = (joinCount + headAlpha * joinScore) / (headAlpha + headRHSTotal);    
    double joinProb = pow(unSmoothedJ,smooth);
    double topProb = (topCount + headAlpha * topScore) / (headAlpha + headRHSTotal);
    double bottomProb = (treeKronDel + bottomCount + myAlpha * bottomScore) /
        (rhsKronDel + myRHSTotal + myAlpha);
    double combProb = pow(topProb * bottomProb,smooth);    
    double cutoff = combProb / (joinProb + combProb);
    float randval = (rand() / ((float) RAND_MAX + 1));

    //printf("CUTOFF at %f - join %f comb %f, randval %f\n",cutoff,joinProb,combProb,randval);
        
    if(randval > cutoff) { //join
                    
            //node is already marked as not cut from above in this method,             

        if(wasCut) {
            bool insertJoin = false;
            if(joinIter == treemap.end()) {
                insertJoin = true;
            } else {
                joinIter->second += 1;
            }
            
            //topIter and bottomIter must have been in, checked above
                            
            if(topIter->second == 1) {
                treemap.erase(topIter); //does not invalidate other iterators
            } else 
                topIter->second -= 1;

                
            if(bottomIter->second == 1) {
                treemap.erase(bottomIter); //does not invalidate other iterators
            } else 
                bottomIter->second -= 1;
            
            /**
             * to update the head offset in a join,
             * everything in the bottom segment will now pointing to headOffset
             * this includes stub nodes at the leaves of the segment
             * so add in the difference
             */   
            for(Segment::iterator iter = bottom.begin();iter != bottom.end(); ++iter) {
                iter.n->head = iter.offset - headOffset;
            }
                
            rhsCounts[myRHS] -= 1;
            if(insertJoin)
                treemap.insert(make_pair(join,1));


#ifdef TSGDEBUG

            /**
             * After a join, the counts of the top and bottom should have decreased
             * and join should have increased by 1.
             */
            
            TreeHashMap::iterator j = treemap.find(join);
            TreeHashMap::iterator t = treemap.find(top);
            TreeHashMap::iterator b = treemap.find(bottom);
            size_t cj = 0;
            if(j != treemap.end())
                cj = j->second;
            size_t ct = 0;
            if(t != treemap.end())
                ct = t->second;
            size_t cb = 0;
            if(b != treemap.end())
                cb = b->second;
            if(cj != joinCount + 1) {
                printf("JOIN ERROR : JOIN COUNT ERROR %d -> %d\n",joinCount,cj);
                top.printMe();
                bottom.printMe();
                join.printMe();
                throw "!";
            }
            if(ct != topCount) {
                printf("TOP COUNT ERROR\n");
                throw "!";
            }
            if(cb != bottomCount) {
                printf("BOTTOM COUNT ERROR\n");
                throw "!";
            }
#endif          
        } 
    } else {

        //the marker was set to false in this method, so set it true
        tree->markers[offset] = true;
        if(!wasCut) {
            bool insertTop = false;
            bool insertBot = false;
            if(topIter == treemap.end()) {
                insertTop = true;
            } else {
                topIter->second += 1;
            }
            if(bottomIter == treemap.end()) {
                insertBot = true;
            } else {
                bottomIter->second += 1;
            }
            
            //join must be in the map, checked at the beginning of this method
            
            if(joinIter->second == 1) {
                treemap.erase(joinIter); //does not invalidate other iterators
            } else 
                joinIter->second -= 1;
            
            rhsCounts[myRHS] += 1;
                
            /**
             * To update the head offsets after a cut, everything but the first node
             * in the bottom segment should be set to bottom's offset
             *
             */ 
            NodeOffset newHead = offset;
            for(Segment::iterator iter = ++bottom.begin();iter != bottom.end(); ++iter) {
                iter.n->head = (iter.offset - newHead);
            }
            
            if(insertTop) 
                treemap.insert(make_pair(top,1));
                
            if(insertBot) 
                treemap.insert(make_pair(bottom,1));

#ifdef TSGDEBUG
            TreeHashMap::iterator j = treemap.find(join);
            TreeHashMap::iterator t = treemap.find(top);
            TreeHashMap::iterator b = treemap.find(bottom);
            size_t cj = 0;
            if(j != treemap.end())
                cj = j->second;
            size_t ct = 0;
            if(t != treemap.end())
                ct = t->second;
            size_t cb = 0;
            if(b != treemap.end())
                cb = b->second;
            if(cj != joinCount) {
                printf("JOIN COUNT ERROR %d -> %d\n",joinCount,cj);
                throw "!";
            }
            if(ct != topCount + 1) {
                printf("TOP COUNT ERROR\n");
                throw "!";
            }
            if(cb != bottomCount + 1) {
                printf("BOTTOM COUNT ERROR\n");
                throw "!";
            }
#endif
            
        }
    }
}

double TreeChunker::score(Segment& seg) {
    double score = 1.0;
    for(Segment::iterator iter = seg.begin();iter != seg.end();++iter) {
        int index = iter.n->index;
        size_t rhs = rhsMap[index];
        if(iter.stub) {
            score *= (1.0 - beta[rhs]);
        } else {
            score *= pcfg[index]; //PCFG score
            score *= beta[rhs];
        }
    }
    return score;
}

void TreeChunker::shuffle() {
    for(size_t i=1;i<samples.size() - 1;++i) {
        size_t ind = rand() % (samples.size() - i);
        pair<ParseTree*,NodeOffset> tmp = samples[ind];
        samples[ind] = samples[samples.size() - i];
        samples[samples.size() - i] = tmp;
    }
}




namespace {

    void writeBEbytes(std::ofstream& ofs, char* data, size_t bytes) {
        for(int i = bytes - 1;i>=0;--i) {
            ofs.write(data + i,1);
        }
    }

};


void TreeChunker::packResults(const char* filename) {

    std::ofstream ofs(filename,std::ios::binary);

    printf("Writing data to %s\n",filename);

    writeBEbytes(ofs,reinterpret_cast<char*>(&ntrees),sizeof(size_t));

    for(size_t i=0;i<ntrees;++i) {

        ParseTree& tree = trees[i];

        size_t numNodes = tree.size;
        
        writeBEbytes(ofs,reinterpret_cast<char*>(&numNodes),sizeof(size_t));

        /**
        for(size_t j=0;j<numNodes;++j) {
            TreeNode& node = tree.nodelist[j];
            size_t index = node.index;
            char isTerm = node.isTerminal;
            size_t head = node.head;
            size_t parent = node.parent;
            size_t sibling = node.sibling;
            writeBEbytes(ofs,reinterpret_cast<char*>(&index),sizeof(size_t));
            writeBEbytes(ofs,reinterpret_cast<char*>(&isTerm),sizeof(char));
            writeBEbytes(ofs,reinterpret_cast<char*>(&head),sizeof(size_t));
            writeBEbytes(ofs,reinterpret_cast<char*>(&parent),sizeof(size_t));
            writeBEbytes(ofs,reinterpret_cast<char*>(&sibling),sizeof(size_t));
        }
        */

        for(size_t j=0;j<numNodes;++j) {
            char mark = tree.markers[j];
            writeBEbytes(ofs,reinterpret_cast<char*>(&mark),sizeof(char));
        }
    }

    writeBEbytes(ofs,reinterpret_cast<char*>(&nRHS),sizeof(size_t));
    for(size_t i=0;i<nRHS;++i) {
        writeBEbytes(ofs,reinterpret_cast<char*>(alpha + i),sizeof(double));
    }
    for(size_t i=0;i<nRHS;++i) {
        writeBEbytes(ofs,reinterpret_cast<char*>(beta + i),sizeof(double));
    }

    ofs.close();
    
}


double TreeChunker::logLikelihood() {
    double ll = 0.0;

    double avgSegs = 0.0;
    
    for(size_t i=0;i<ntrees;++i) {
        ParseTree& tree = trees[i];
        double sCount = 1.0;
        for(NodeOffset j=0;j<tree.size;++j) {
            if(tree.markers[j]) {
                sCount += 1;
                Segment seg = Segment(&tree,j);
                double bottomScore = score(seg);
                ll += log(bottomScore);
            }
        }
        sCount /= tree.size;
        avgSegs += sCount / ntrees;
    }
    printf("Average Segments Per Tree = %f\n",avgSegs);
    return ll;

}

double TreeChunker::segmentationP(ParseTree& tree) {
    double ret = 1.0;
    for(NodeOffset j=0;j<tree.size;++j) {
        if(tree.markers[j]) {
            Segment seg = Segment(&tree,j);
            double scr = score(seg);
            ret *= scr;
        }
    }
    return ret;
}
