#include "TreeChunker.hpp"
#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_sf_gamma.h>
#include <fstream>
#include <cmath>

size_t TreeChunker::samplesPerDist = 10;

TreeChunker::TreeChunker(size_t* rhsmap_, double* pcfg_,
                         double* beta_,double* alpha_,
                         ParseTree* trees_, size_t ntrees_, size_t nRHS_) : chart(NULL),
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

        /**
        TreeHashMap lastmap;
        lastmap.set_empty_key(Segment(nulltree1,(NodeOffset)0));
        lastmap.set_deleted_key(Segment(nulltree2,(NodeOffset)0));
        TreeHashMap::iterator it = treemap.begin();
        for(;it != treemap.end();++it) {
            lastmap.insert(make_pair(it->first,it->second));
        }
        */
        
        if(i % 10 == 9) {
            //if(false) {  
            resampleTrees();

            resampleBeta();
            resampleAlpha();

            printf("T%f\n",logLikelihood());
        } else {
            double smooth = smoothS + ((double) i / (double) iterations) * gap;
            //printf("DO ITER WITH SMOOTH = %f\n",smooth);
            //clock_t start = clock();
            resample(1/smooth);
            resampleBeta();
            resampleAlpha();
            //clock_t finish = clock();
            //printf("Iteration %d took %f seconds\n",i,(double(finish) - double(start))/CLOCKS_PER_SEC);
            printf("%f\n",logLikelihood());
            //printf("Log Likelihood = %f\n",logLikelihood());
        }

        /**
        double dist = 0.0;
        it = treemap.begin();
        for(;it != treemap.end();++it) {
            TreeHashMap::iterator f = lastmap.find(it->first);
            if(f == lastmap.end())
                dist += it->second;
            else
                dist += abs((double) it->second - f->second);
        }
        it = lastmap.begin();
        for(;it != lastmap.end();++it) {
            TreeHashMap::iterator f = treemap.find(it->first);
            if(f == treemap.end())
                dist += it->second;
        }
        */
        //printf("DISTANCE = %f\n",dist);
        
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
    


    
    
    
}

void TreeChunker::resampleTrees() {
    //printf("SAMPLE JOINT TREES\n");
    //for(size_t i=0;i<ntrees;++i) {
    for(size_t i=0;i<1;++i) {
        //        if(i % 1000 == 0)
        //    printf("SAMPLE TREE %d\n",i);
        sampleTree(&trees[i],1.0);
    }
    
}



void TreeChunker::resampleAlpha() {

    //printf("RESAMPLE ALPHA\n");
    
    double ALPHA_SIGSQ = 20;
    double GAMMA_A = 1;
    double GAMMA_B = 50;
    
    gsl_rng* r = gsl_rng_alloc(gsl_rng_taus);
    size_t numClasses[nRHS];
    for(size_t i=0;i<nRHS;++i) { numClasses[i] = 0; }

    for(TreeHashMap::iterator iter = treemap.begin(); iter != treemap.end(); ++iter) {
        TreeNode& n = iter->first.ptree->nodelist[iter->first.headIndex];
        numClasses[rhsMap[n.index]] += 1;
        //printf("INC %d\n",rhsMap[n.index]);
    }
    
    for(size_t i=0;i<nRHS;++i) {
        //printf("ALPHA NUMBER %d\n",i);
        double curAlpha = alpha[i];
        double numC = numClasses[i];
        double total = rhsTotals[i];

        std::pair<double,double> curMV = getLNMeanVar(curAlpha,ALPHA_SIGSQ);
        double nextAlpha = gsl_ran_lognormal(r,curMV.first,curMV.second);
        std::pair<double,double> nextMV = getLNMeanVar(nextAlpha,ALPHA_SIGSQ);
        double qFrac = gsl_ran_lognormal_pdf(curAlpha,nextMV.first,nextMV.second);
        //printf("Q1 = %f\n",qFrac);
        //printf("Q2 = %f\n",gsl_ran_lognormal_pdf(nextAlpha,curMV.first,curMV.second));
        qFrac /= gsl_ran_lognormal_pdf(nextAlpha,curMV.first,curMV.second);

        //printf("%f %f %f %f\n",GAMMA_A,GAMMA_B,numC,total);
        double pFrac = evalGammaPosterior(nextAlpha,GAMMA_A,GAMMA_B,numC,total);
        //printf("P1 = %f\n",pFrac);
        //printf("P2 = %f\n",evalGammaPosterior(curAlpha,GAMMA_A,GAMMA_B,numC,total));
        pFrac /= evalGammaPosterior(curAlpha,GAMMA_A,GAMMA_B,numC,total);

        //printf("P Q = %f %f\n",qFrac,pFrac);
        
        double accept = qFrac * pFrac;
        //printf("ACC=%f\n",accept);
        if(nextAlpha == 0)
            accept = 0.0;
        if(accept >= 1.0) 
            alpha[i] = nextAlpha;
        else {
            double rando = ((double)rand() / (double) RAND_MAX);
            if(rando <= accept) {
                alpha[i] = nextAlpha;
            }
            
                //printf("REJECT %f from %f\n",nextAlpha,alpha[i]);
            //if not, then leave it be
        }

        //printf("AL %f\n",alpha[i]);
    }

    gsl_rng_free(r);
    
}

std::pair<double,double> TreeChunker::getLNMeanVar(double d, double variance) {
    double v = log (variance / pow(d,2.0) + 1);
    double m = log(d) - v/2;
    return std::make_pair(m,v);
}

double TreeChunker::evalGammaPosterior(double d, double gamma_a, double gamma_b, double k, double n) {

    //printf("EVAL %f %f %f %f %f\n",d,gamma_a,gamma_b,k,n);
    double ret = gsl_ran_gamma_pdf(d,gamma_a,gamma_b);
    //printf("!%f\n",ret);
    ret *= pow(d,k-1);
    //printf("!%f\n",ret);
    ret *= (d + n);
    //printf("!%f\n",ret);
    double m = gsl_sf_beta(d+1,n);
    ret *= m;
    //printf("!%f\n",ret);
    return ret;
}

void TreeChunker::resampleBeta() {
    gsl_rng* r = gsl_rng_alloc(gsl_rng_taus);
    for(size_t i=0;i<nRHS;++i) {
        size_t expCount = rhsCounts[i];
        size_t nExpCount = rhsTotals[i] - expCount;
        double newVal = gsl_ran_beta(r,1+expCount, 1+nExpCount);
        if(newVal == 1.0)
            newVal = .9999;
        if(newVal == 0.0)
            newVal = .0001;
        beta[i] = newVal;
    }
    gsl_rng_free(r);
    
}

SeqSample TreeChunker::sampleTop(ParseTree& tree, NodeOffset nodeoff,
                                 double smooth, std::vector<NodeOffset>& leaves) {

    //printf("Sample Top, size = %d\n",tree.size);
    
    //this is the sample of the treetop, initally with all markers masked
    SeqSample ret(&tree,nodeoff);

    //unmask the marker at the head and set its marker to true (cut)
    ret.markerMask[nodeoff] = true;
    ret.markers[nodeoff] = true;

    //use a segment to traverse the tree
    tree.clearMarkers(); //this might be overkill, if the markers are cleared initially (before this is called)
    Segment seg(&tree,nodeoff);

    Segment::iterator iter = seg.begin();
    ++iter;

    double prob = 1.0;
    
    while(iter != seg.end()) {
        TreeNode* node = iter.n;
        int index = node->index;
        size_t lhs = rhsMap[index];

        //iter should not be a stub at this point.
        if(iter.stub)
            throw "Iter should not be a stub here";
        
        //decide if this node is a cut point
        bool cut = true;

        //a high beta means that this lhs likes to not be cut
        double cutProb = beta[lhs];
        //printf("CutProb = %f\n",cutProb);
        double rDub = (double) rand() / ((double)RAND_MAX + 1.0);
        //printf("RAND = %f\n",rDub);
        if(rDub > cutProb)
            cut = false;

        
        if(cut) {
            //printf("CUT!\n");
            prob *= (1-cutProb);
            iter.stub = true;  //this makes the upcoming ++ work correctly
            leaves.push_back(iter.offset);
            ret.markers[iter.offset] = true;
            ret.markerMask[iter.offset] = true;
        } else {
            prob *= cutProb;
            ret.markers[iter.offset] = false;
            ret.markerMask[iter.offset] = true;
        }
        ++iter;
    }

    ret.prob = prob;
    return ret;
    
}

SeqSample TreeChunker::sampleFrom(ParseTree& tree, NodeOffset nodeoff, double smooth) {
    //printf("SAMPLING AT %d\n",nodeoff);
    //if the distribution has not been determined for this node yet, we must calculate it
    if(chart[nodeoff].samples == NULL) {
        //printf("Filling in %d\n",nodeoff);
        TreeNode node = tree.nodelist[nodeoff];

        if(node.isTerminal) {
            SeqSample sam(&tree,nodeoff);
            sam.markerMask[nodeoff] = true;
            sam.markers[nodeoff] = true;
            //SampleDist makes a copy of sam in its constructor
            chart[nodeoff] = SampleDist(1,&sam,&tree);
        } else {
            //printf("Not a terminal\n");
            SeqSample sams[samplesPerDist];
            for(size_t i=0;i<samplesPerDist;++i) { //set samplesPerDist samples
                //printf("get sample %d\n",i);
                std::vector<NodeOffset> leaves;
                //sample a tree top
                SeqSample sam = sampleTop(tree,nodeoff,smooth,leaves);
                if(sam.prob == 0) {

                    printf("TOP!!!Why does this sample have p = 0?\n");
                    throw "Why does this sample have p = 0?";
                }
                //right now sam.prob is the sequential proposal (based on betas)

                //printf("this sample has %d leaves\n",leaves.size());
                for(size_t k=0;k<leaves.size();++k) {

                    //sample the distribution at each leaf
                    SeqSample subSam = sampleFrom(tree,leaves[k],smooth);
                    //push the sampled subtree's markers into this sample
                    for(size_t j=0;j<tree.size;++j) {
                        if(subSam.markerMask[j]) {

                            //remove me later
                            if(sam.markerMask[j] && (sam.markers[j] != subSam.markers[j]))
                                throw "The subsample disagrees";

                            
                            sam.markerMask[j] = true;
                            sam.markers[j] = subSam.markers[j];

                            
                        }
                    }
                    //multiply in the probability of the subtree
                    sam.prob *= subSam.prob;
                }

                //right now sam.prob represents how often we would produce this sample
                //we need to reweight this by the actual probability
                Segment seg(&tree,nodeoff);
                //copy in the markers
                for(size_t j=0;j<tree.size;++j) {
                    if(sam.markerMask[j]) {
                        seg.markers[j] = sam.markers[j];
                    }
                }
                double realP = scoreDP(seg,1.0);

                if(sam.prob == 0) {
                    printf("Why does this sample have p = 0?\n");
                    throw "Why does this sample have p = 0?";
                }
                
                sam.prob = realP / sam.prob; //this might be backwards...
                
                sams[i] = sam;
            }
            chart[nodeoff] = SampleDist(samplesPerDist,sams,&tree);  
        }
    }
    
    return chart[nodeoff].sample();
}


void TreeChunker::sampleTree(ParseTree* tree, double smooth) {

    if(chart != NULL)
        delete[] chart;
    chart = new SampleDist[tree->size];

    double originalP = segmentationP(*tree);

    //remove these segments from the map
    for(size_t i=0;i<tree->size;++i) {
        if(tree->markers[i]) {
            Segment seg(tree,i);
            TreeHashMap::iterator iter = treemap.find(seg);
            if(iter == treemap.end())
                throw "THEY SHOULD BE IN THE MAP";
            
            if(iter->second == 1) {
                treemap.erase(iter); //does not invalidate other iterators
            } else 
                iter->second -= 1;
            
            size_t lhs = tree->nodelist[seg.headIndex].index;
            rhsMap[lhs] -= 1;
        }   
    }
    
    //store the original segmentation for computation of Q later
    SeqSample origSam(tree,0);
    for(size_t i=0;i<tree->size;++i) {
        origSam.markerMask[i] = true;
        origSam.markers[i] = tree->markers[i];
    }
    
    /**
     * sequential monte carlo sampler produces a sample from an approximate
     * distribution q(x)
     */
    SeqSample sam = sampleFrom(*tree,0,smooth);
    double newQ = sam.prob;
    
    double originalQ = chart[0].score(origSam.markers,tree->size);
    
    for(size_t i=0;i<tree->size;++i) {
        if(i == 0){
            if(!sam.markerMask[i] || !sam.markers[i])
                throw "The root should be marked";
        }
        if(!sam.markerMask[i])
            throw "All nodes should be unmasked here";
        tree->markers[i] = sam.markers[i];
    }
    
    double newP = segmentationP(*tree);
    
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
        for(size_t i=0;i<tree->size;++i) {
            tree->markers[i] = origSam.markers[i];
        }
    }

    for(size_t i=0;i<tree->size;++i) {
        if(tree->markers[i]) {
            Segment seg = Segment(tree,i);
            TreeHashMap::iterator iter = treemap.find(seg);
            if(iter == treemap.end())
                treemap.insert(make_pair(seg,1));
            else
                iter->second += 1;
            size_t lhs = tree->nodelist[seg.headIndex].index;
            rhsMap[lhs] += 1;
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

double TreeChunker::scoreDP(Segment& seg, double smooth) {
    TreeNode& node = seg.ptree->nodelist[seg.headIndex];
    
    size_t rhs = rhsMap[node.index];
    size_t total = rhsCounts[rhs];
    double al = alpha[rhs];

    TreeHashMap::iterator iter = treemap.find(seg);
    size_t count = 0;
    if(iter != treemap.end())
        count = iter->second;

    double baseScore = score(seg);
    
    double unsmoothed = (count + al * baseScore) / (al + total);
    return pow(unsmoothed,smooth);

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

    printf("Writing data to %s\n",filename);
    std::ofstream ofs(filename,std::ios::binary);



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
                double bottomScore = scoreDP(seg,1.0);
                if(bottomScore == 0) {
                    printf("index = %d\n",j);
                    TreeNode& node = seg.ptree->nodelist[seg.headIndex];
    
                    size_t rhs = rhsMap[node.index];
                    size_t total = rhsCounts[rhs];
                    double al = alpha[rhs];
                    
                    TreeHashMap::iterator iter = treemap.find(seg);
                    size_t count = 0;
                    if(iter != treemap.end())
                        count = iter->second;
                    double baseScore = score(seg);

                    printf("RHS %d\n",rhs);
                    printf("total %d\n",total);
                    printf("alpha %f\n",al);
                    printf("baseScore %f\n",baseScore);
                    printf("count %d\n",count);
                    
                    throw "Got a zero score!";
                }
                ll += log(bottomScore);
            }
        }
        //sCount /= tree.size;
        avgSegs += sCount / ntrees;
    }
    //printf("Average Segments Per Tree = %f\n",avgSegs);
    return ll;

}

double TreeChunker::segmentationP(ParseTree& tree) {
    double ret = 1.0;
    for(NodeOffset j=0;j<tree.size;++j) {
        if(tree.markers[j]) {
            Segment seg = Segment(&tree,j);
            double scr = scoreDP(seg,1.0);
            ret *= scr;
        }
    }
    return ret;
}
