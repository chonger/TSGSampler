#include "TreeChunker.hpp"
#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_sf_gamma.h>
#include <fstream>
#include <cmath>
#include <curses.h>

size_t TreeChunker::samplesPerDist = 100;

TreeChunker::TreeChunker(size_t* lhsmap_, double* pcfg_,size_t numRules_,
                         double* beta_,double* alpha_,
                         ParseTree* trees_, size_t ntrees_, size_t nLHS_) :
    chart(NULL),
    
    lhsMap(lhsmap_), lhsCounts(NULL), pcfg(pcfg_), numRules(numRules_), beta(beta_),
    alpha(alpha_), trees(trees_), ntrees(ntrees_), nLHS(nLHS_),
    resampleA(true), resampleB(true) {

    stop = false;
    
    nulltree1 = new ParseTree(NULL,NULL,0);
    nulltree2 = new ParseTree(NULL,NULL,0);    

    treemap.set_empty_key(Segment(nulltree1,(NodeOffset)0));
    treemap.set_deleted_key(Segment(nulltree2,(NodeOffset)0));
    
    lhsTotals = new size_t[nLHS];
    lhsCounts = new size_t[nLHS];
    for(size_t i=0;i<nLHS;++i) {
        lhsTotals[i] = 0;
        lhsCounts[i] = 0;
    }
    
    for(size_t i=0;i<ntrees;++i) {
        ParseTree& pt = trees[i];

        for(size_t j=0;j<pt.size;++j) {
            
            size_t lhsInd = lhsMap[pt.nodelist[j].index];
            lhsTotals[lhsInd] += 1;
            
            if(j > 0) //we never need to resample the root
                samples.push_back(make_pair(&(pt),j));
            
            if(pt.markers[j]) {
                lhsCounts[lhsInd] += 1;
                Segment seg(&pt,j);
                TreeHashMap::iterator findo = treemap.find(seg);
                if(findo == treemap.end())
                    treemap.insert(make_pair(seg,1));
                else
                    findo->second += 1;
            }
        }
    }

    totalLabels = samples.size();
    /**
    for(size_t i=0;i<nLHS;++i) {
        printf("%d - COUNT %d - TOTAL %d\n",i,lhsCounts[i],lhsTotals[i]);

    }
    */
}

void TreeChunker::resample(int iterations, double smoothS, double smoothF, size_t distSize, size_t jointFreq) {
    double gap = smoothF - smoothS;
    
    
    for(int i=0;i<iterations;++i) {
        if(stop)
            return;
        /**
        TreeHashMap lastmap;
        lastmap.set_empty_key(Segment(nulltree1,(NodeOffset)0));
        lastmap.set_deleted_key(Segment(nulltree2,(NodeOffset)0));
        TreeHashMap::iterator it = treemap.begin();
        for(;it != treemap.end();++it) {
            lastmap.insert(make_pair(it->first,it->second));
        }
        */
        numChanged = 0;
        clock_t start = clock();
        if(jointFreq > 0 && i % jointFreq == jointFreq - 1) {
            //if(i % 2 == 1) {
            //if(false) {
        //if(true) {
            samplesPerDist = distSize;
            resampleTrees();
        } else {
            double smooth = smoothS + ((double) i / (double) iterations) * gap;
            //printf("DO ITER WITH SMOOTH = %f\n",smooth);
            
            resample(1/smooth);
                    

            //printf("Log Likelihood = %f\n",logLikelihood());
        }
        if(resampleB)
            resampleBeta();
        if(resampleA)
            resampleAlpha();

        double logl = logLikelihood();
        //if(i % 100 == 0) {

            clock_t finish = clock();
            printf("Iteration %d took %f seconds\n",i,(double(finish) - double(start))/CLOCKS_PER_SEC);
            printf("Log Likelihood           : %f\n",logl);
            printf("Number of changed labels : %d out of %d\n",numChanged,totalLabels);
            /**
            for(size_t k=0;k<nLHS;++k) {
                printf("ALPHA %d  - %E\n",k,alpha[k]);
            }
            */
            
            //}
        outstream << logl << "\n";
        //treemap.resize(0);

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

/**
 * Do one full Gibbs iteration with a given smooth factor
 */ 
void TreeChunker::resample(double smooth) {
    shuffle();
    
    for(vector<pair<ParseTree*,NodeOffset> >::iterator iter = samples.begin();
        iter != samples.end(); ++iter) {
        sampleNode(iter->first,iter->second,smooth);
    }
}

void TreeChunker::resampleTrees() {
    //printf("SAMPLE JOINT TREES\n");
    //for(size_t i=0;i<ntrees;++i) {

    acceptCount = 0;
    acceptTotal = 0;
    avgAcc = 0.0;
    avgP = 0.0;
    pTotal = 0.0;
    for(size_t i=0;i<ntrees;++i) {
        //        if(i % 1000 == 0)
        //    printf("SAMPLE TREE %d\n",i);
        sampleTree(&trees[i],1.0);
    }
    /**
    for(size_t i=0;i<ntrees;++i) {
        ParseTree* tree = &trees[i];
        for(size_t i=0;i<tree->size;++i) {
            if(tree->markers[i]) {
                Segment seg = Segment(tree,i);
                TreeHashMap::iterator iter = treemap.find(seg);
            }   
        } 

    }
    */
    
    printf("Accept Rate = %f\n",(double)acceptCount/(double)ntrees);
    printf("Average Accept = %f\n",avgAcc/(double)ntrees);
    printf("Average P = %f\n",avgP/pTotal);
    
}


void TreeChunker::resampleAlphaMH() {
    
    double ALPHA_SIGSQ = 30;
    double GAMMA_A = .001;
    double GAMMA_B = 1000;

    gsl_rng* r = gsl_rng_alloc(gsl_rng_taus);
    size_t numClasses[nLHS];
    for(size_t i=0;i<nLHS;++i) { numClasses[i] = 0; }

    for(TreeHashMap::iterator iter = treemap.begin(); iter != treemap.end(); ++iter) {
        TreeNode& n = iter->first.ptree->nodelist[iter->first.headIndex];
        numClasses[lhsMap[n.index]] += 1;
        //printf("INC %d\n",lhsMap[n.index]);
    }
    
    for(size_t i=0;i<nLHS;++i) {
        //printf("ALPHA NUMBER %d\n",i);
        double curAlpha = alpha[i];
        double numC = numClasses[i];
        double total = lhsTotals[i];
        
        
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
            continue;
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
        
        
        //    printf("AL %f\n",alpha[i]);

    }

    gsl_rng_free(r);

}

void TreeChunker::resampleAlphaEW() {
    
    double GAMMA_A = .001;
    double GAMMA_B = 1000;
    
    gsl_rng* r = gsl_rng_alloc(gsl_rng_taus);
    size_t numClasses[nLHS];
    for(size_t i=0;i<nLHS;++i) { numClasses[i] = 0; }

    for(TreeHashMap::iterator iter = treemap.begin(); iter != treemap.end(); ++iter) {
        TreeNode& n = iter->first.ptree->nodelist[iter->first.headIndex];
        numClasses[lhsMap[n.index]] += 1;
        //printf("INC %d\n",lhsMap[n.index]);
    }
    
    for(size_t i=0;i<nLHS;++i) {
        //printf("ALPHA NUMBER %d\n",i);
        double curAlpha = alpha[i];
        double numC = numClasses[i];
        double total = lhsTotals[i];

        double eta = gsl_ran_beta(r,curAlpha + 1, total);

        double bMinLog = GAMMA_B - log(eta);
        
        double pifrac = (GAMMA_A + numC - 1) / (total * bMinLog);

        double binTheta = pifrac / (1 + pifrac);

        double rDub = (double) rand() / ((double)RAND_MAX + 1.0);

        double gA = GAMMA_A + numC;
        
        if(rDub > binTheta)
            gA -= 1;

        alpha[i] = gsl_ran_gamma(r,gA,bMinLog);

        //printf("AL %f\n",alpha[i]);

    }

    gsl_rng_free(r);
    
}


void TreeChunker::resampleAlpha() {

    //resampleAlphaEW();
    resampleAlphaMH();
    
}

std::pair<double,double> TreeChunker::getLNMeanVar(double d, double variance) {
    double v = log (variance / pow(d,2.0) + 1);
    double m = log(d) - v/2;
    return std::make_pair(m,v);
}

double TreeChunker::evalGammaPosterior(double d, double gamma_a, double gamma_b, double k, double n) {

    //printf("EVAL %f %f %f %f %f\n",d,gamma_a,gamma_b,k,n);
    double ret = log(gsl_ran_gamma_pdf(d,gamma_a,gamma_b));
    //printf("!%f\n",ret);
    ret += log(d) * (k-1);
    //printf("!%f\n",ret);
    ret += log(d + n);
    //printf("!%f\n",ret);
    ret += gsl_sf_lnbeta(d+1,n);
    //printf("!%f\n",ret);
    ret = exp(ret);
    //printf("!%f\n",ret);
    return ret;
}

void TreeChunker::resampleBeta() {
    double prior = 100;
    
    gsl_rng* r = gsl_rng_alloc(gsl_rng_taus);
    for(size_t i=0;i<nLHS;++i) {
        size_t expCount = lhsCounts[i];
        size_t nExpCount = lhsTotals[i] - expCount;
        double newVal = gsl_ran_beta(r,prior+nExpCount,prior+expCount);

        //avoid zeroing segment probabilities
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
        size_t lhs = lhsMap[index];

        //iter should not be a stub at this point.
        /**
        if(iter.stub)
            throw "Iter should not be a stub here";
        */
        
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
            prob *= (cutProb);
            iter.stub = true;  //this makes the upcoming ++ work correctly
            leaves.push_back(iter.offset);
            ret.markers[iter.offset] = true;
            ret.markerMask[iter.offset] = true;
        } else {
            prob *= (1-cutProb);
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

                /**
                if(sam.prob == 0) {

                    printf("TOP!!!Why does this sample have p = 0?\n");
                    throw "Why does this sample have p = 0?";
                }
                */
                //right now sam.prob is the sequential proposal (based on betas)

                //printf("this sample has %d leaves\n",leaves.size());
                for(size_t k=0;k<leaves.size();++k) {

                    //sample the distribution at each leaf
                    SeqSample subSam = sampleFrom(tree,leaves[k],smooth);
                    //push the sampled subtree's markers into this sample
                    for(size_t j=0;j<tree.size;++j) {
                        if(subSam.markerMask[j]) {

                            /**
                            if(sam.markerMask[j] && (sam.markers[j] != subSam.markers[j]))
                                throw "The subsample disagrees";
                            */
                            
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

                /**
                if(sam.prob == 0) {
                    printf("Why does this sample have p = 0?\n");
                    throw "Why does this sample have p = 0?";
                }
                */
                
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

    //remove these segments from the map
    for(size_t i=0;i<tree->size;++i) {
        if(tree->markers[i]) {
            Segment seg(tree,i);

            //look up the segment
            TreeHashMap::iterator iter = treemap.find(seg);

            /**
            if(iter == treemap.end()) {
                printf("I = %d\n",i);

                throw "THEY SHOULD BE IN THE MAP";

            }
            */

            //decrement it
            if(iter->second == 1) {
                treemap.erase(iter); //does not invalidate other iterators
            } else 
                iter->second -= 1;

            //decrement the total
            size_t lhs = tree->nodelist[seg.headIndex].index;
            lhsCounts[lhsMap[lhs]] -= 1;
        }   
    }

    //now that the segments are removed, we can calculate the segmentation Prob
    double originalP = segmentationP(tree);
    if(originalP > 1.0)
        throw "BAD!";

    
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
    
    bool same = true;
    for(size_t i=0;i<tree->size;++i) {
        if(origSam.markers[i] != sam.markers[i]) {
            same = false;
            break;
        }
    }
    
    double newQ = sam.prob + chart[0].leftOut;
    
    double originalQ = chart[0].score(origSam.markers,tree->size);
    /**
    printf("NEW SAMPLE\n");
    for(size_t i=0;i<tree->size;++i) {
        printf("%d",sam.markers[i]);
    }
    printf("\n");

    printf("OLD SAMPLE\n");
    for(size_t i=0;i<tree->size;++i) {
        printf("%d",origSam.markers[i]);
    }
    printf("\n");
    */
    for(size_t i=0;i<tree->size;++i) {

        /**
        if(i == 0){
            if(!sam.markerMask[i] || !sam.markers[i])
                throw "The root should be marked";
        }
        */
        
        if(!sam.markerMask[i])
            throw "All nodes should be unmasked here";
        
        bool mark = sam.markers[i];

        //        if(mark != 0) {
        //  mark = true;
        //}
                
        tree->markers[i] = mark;
    
    }

    double newP = segmentationP(tree);
    if(newP > 1.0)
        throw "BAD!";

    /**
    if(same) {
        printf("SAME!\n");
    } else {
        printf("NOTSAME!\n");
    }
    printf("newP %E oldP %E newQ %E oldQ %E\n",newP,originalP,newQ,originalQ);
    */
    
    /**
     * use metropolis-hastings to decide acceptance
     *
     * right now the tree has already been accepted.  If acceptance rate is low this will be a time sink
     */ 
    double a1 = newP / originalP;
    avgP += a1;
    pTotal += 1;
    double a2 = originalQ / newQ;
    double a = a1 * a2;
    bool accept = true;
    avgAcc += a;
    //printf("A - %E\n",a);
    if(a < 1) {
        double rDub = (double) rand() / ((double)RAND_MAX + 1.0);
        if(rDub > a)
            accept = false; //keep the old tree
    }

    //uncommet to always accept
    //accept = true;

    
    acceptTotal += 1;

    
    //if we accept, the markers are already set
    if(!accept) {
        //restore original markers
        for(size_t i=0;i<tree->size;++i) {
            tree->markers[i] = origSam.markers[i];
        }
    } else {
        acceptCount += 1;
        //if(!same)
        //    printf("ACCEPT\n");
    }


    //accept is decided by now

    for(size_t i=0;i<tree->size;++i) {
        if(tree->markers[i]) {
            Segment seg = Segment(tree,i);
            TreeHashMap::iterator iter = treemap.find(seg);
            if(iter == treemap.end())
                treemap.insert(make_pair(seg,1));
            else
                iter->second += 1;
            size_t lhs = tree->nodelist[seg.headIndex].index;
            lhsCounts[lhsMap[lhs]] += 1;

            if(accept) {
                Segment::iterator sitr = seg.begin();
                ++sitr;
                while(sitr != seg.end()) {
                    NodeOffset newOff = sitr.offset - i;
                    if(!accept) {
                        if(newOff != sitr.n->head) {
                            printf("OLD/NEW - %d %d\n",sitr.n->head,newOff);
                            throw "!!!";
                        }
                        
                    }
                    sitr.n->head = newOff;
                    ++sitr;
                }
            }
        }   
    }
    delete[] chart;
    chart = NULL;

}


void TreeChunker::sampleNode(ParseTree* tree, NodeOffset offset, double smooth) {
    
    TreeNode& node = tree->nodelist[offset];
    NodeOffset headOffset = offset - node.head;
    TreeNode& head = tree->nodelist[headOffset];
    
    size_t headLHS = lhsMap[head.index];
    size_t myLHS = lhsMap[node.index];
    size_t headLHSTotal = lhsCounts[headLHS];
    size_t myLHSTotal = lhsCounts[myLHS];
    double headAlpha = alpha[headLHS];
    double myAlpha = alpha[myLHS];

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
    size_t lhsKronDel = lhsMap[head.index] == lhsMap[node.index] ? 1 : 0;
    
    tree->markers[offset] = false;
    
    Segment join = Segment(tree,headOffset);

#ifdef TSGDEBUG
    if(join.equals(top) || join.equals(bottom)) {
        printf("CANT HAVE JOIN = TOP|BOT\n");
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
            printf("JOIN SHOULD BE IN THE MAP\n");
            
            throw 1;
        }
#endif

        joinCount = joinIter->second - 1;
    } else {

        if(joinIter != treemap.end())
            joinCount = joinIter->second;
        //else it stays at zero
    }

    double joinScore = score(join);
    

    double unSmoothedJ = (joinCount + headAlpha * joinScore) / (headAlpha + headLHSTotal);    
    double joinProb = pow(unSmoothedJ,smooth);
    double topProb = (topCount + headAlpha * topScore) / (headAlpha + headLHSTotal);
    double bottomProb = (treeKronDel + bottomCount + myAlpha * bottomScore) /
        (lhsKronDel + myLHSTotal + myAlpha);
    double combProb = pow(topProb * bottomProb,smooth);    
    double cutoff = combProb / (joinProb + combProb);
    float randval = (rand() / ((float) RAND_MAX + 1));

    //printf("CUTOFF at %f - join %f comb %f, randval %f\n",cutoff,joinProb,combProb,randval);


    //TOFDO _ REMOVE THESE DEBUG VARIABLES
    bool doCut = false;
    bool inserted = false;
    bool erTop = false;
    bool erBot = false;

    
    if(randval > cutoff) { //join
        doCut = false;
        //node is already marked as not cut from above in this method,             

        if(wasCut) {
            numChanged++;
            bool insertJoin = false;
            if(joinIter == treemap.end()) {
                insertJoin = true;
            } else {
                joinIter->second += 1;

            }
            
            //topIter and bottomIter must have been in, checked above
                            
            if(topIter->second == 1) {
                erTop = true;
                treemap.erase(topIter); //does not invalidate other iterators
            } else 
                topIter->second -= 1;

                
            if(bottomIter->second == 1) {
                erBot = true;
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
                
            lhsCounts[myLHS] -= 1;
            if(insertJoin) {
                treemap.insert(make_pair(join,1));    //INSERT INVALIDATES
                inserted = true;
            }

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
                printf("j %d b %d t %d\n",join.headIndex,bottom.headIndex,top.headIndex);
                printf("Wascut %s %s- Iscut - %d\n",(wasCut)?"T":"F",(!wasCut)?"F":"T",doCut);
                printf("insJ %d - erTop %d - erBot %d\n",inserted,erTop,erBot);
                top.printMe();
                bottom.printMe();
                join.printMe();
                throw "!";
            }
            size_t minus = 0;
            if(top.equals(bottom))
                minus = 1;
            if(ct != topCount - minus) {
                printf("TOP COUNT ERROR\n");
                top.printMe();
                bottom.printMe();
                join.printMe();
                throw "!";
            }
            if(cb != bottomCount - minus) {
                printf("BOTTOM COUNT ERROR\n");
                top.printMe();
                bottom.printMe();
                join.printMe();
                throw "!";
            }
#endif          
        } 
    } else {
        doCut = true;
        //the marker was set to false in this method, so set it true
        tree->markers[offset] = true;
        if(!wasCut) {
            numChanged++;
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
            
            lhsCounts[myLHS] += 1;
                
            /**
             * To update the head offsets after a cut, everything but the first node
             * in the bottom segment should be set to bottom's offset
             *
             */ 
            NodeOffset newHead = offset;
            for(Segment::iterator iter = ++bottom.begin();iter != bottom.end(); ++iter) {
                iter.n->head = (iter.offset - newHead);
            }

            bool dblInsert = false;
            if(insertTop) {
                if(top.equals(bottom)) {
                    dblInsert = true;
                    treemap.insert(make_pair(top,2)); //INSERT INVALIDATES
                    insertBot = false;
                } else 
                    treemap.insert(make_pair(top,1)); //INSERT INVALIDATES
            }
                
            if(insertBot) {
                treemap.insert(make_pair(bottom,1)); //INSERT INVALIDATES
            }
#ifdef TSGDEBUG

            //check that counts updated right for this split
            
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
                top.printMe();
                bottom.printMe();
                join.printMe();
                throw "!";
            }
            size_t add = 1;
            if(top.equals(bottom))
                add = 2;
            if(ct != topCount + add) {
                printf("TOP COUNT ERROR %d %d\n",ct,topCount+add);
                printf("BOTTOM COUNT %d %d\n",cb,bottomCount+add);
                printf("JOIN COUNT %d -> %d\n",joinCount,cj);
                printf("DBLADD? %d\n",dblInsert);
                top.printMe();
                bottom.printMe();
                join.printMe();
                throw "!";
            }
            if(cb != bottomCount + add) {
                printf("BOTTOM COUNT ERROR %d %d\n",cb,bottomCount+add);
                top.printMe();
                bottom.printMe();
                join.printMe();
                throw "!";
            }
#endif
            
        }
    }
}

double TreeChunker::scoreDP(Segment& seg, double smooth) {
    TreeNode& node = seg.ptree->nodelist[seg.headIndex];
    
    size_t lhs = lhsMap[node.index];
    size_t total = lhsCounts[lhs];
    double al = alpha[lhs];

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
    bool first = true;
    for(Segment::iterator iter = seg.begin();iter != seg.end();++iter) {
        
        int index = iter.n->index;
        size_t lhs = lhsMap[index];
        if(iter.stub) {
            //triggers if a node is not a terminal but is a leaf
            //so it must have children, maybe a parent. ROOT -> S is the exception
            //the "good" case is one where the leaf and its parent have different heads
            //if leaf and parent have the same head, we must be cutting a head chain
            //b/c one of the leaf's children has the same head (by def)

            TreeNode* leaf = iter.n;
            //par will be the same node at the root
            TreeNode& par = seg.ptree->nodelist[iter.offset - leaf->parent];
            if(leaf->lexHead == par.lexHead) {
                score *= 1.0;
            }

            score *= (1.0 - beta[lhs]);
        } else {
            score *= pcfg[index]; //PCFG score
            if(!first)
                score *= beta[lhs];
            else
                first = false;
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


    writeBEbytes(ofs,reinterpret_cast<char*>(&numRules),sizeof(size_t));

    //write pcfg probs
    for(size_t i=0;i<numRules;++i) {
        writeBEbytes(ofs,reinterpret_cast<char*>(pcfg + i),sizeof(double));
    }

    //write head indexes
    for(size_t i=0;i<numRules;++i) {
        writeBEbytes(ofs,reinterpret_cast<char*>(lhsMap + i),sizeof(size_t));
    }

    writeBEbytes(ofs,reinterpret_cast<char*>(&nLHS),sizeof(size_t));
    
    //write betas
    for(size_t i=0;i<nLHS;++i) {
        writeBEbytes(ofs,reinterpret_cast<char*>(beta + i),sizeof(double));
    }

    //write alphas
    for(size_t i=0;i<nLHS;++i) {
        writeBEbytes(ofs,reinterpret_cast<char*>(alpha + i),sizeof(double));
    }

    writeBEbytes(ofs,reinterpret_cast<char*>(&ntrees),sizeof(size_t));
    
    for(size_t i=0;i<ntrees;++i) {
        size_t numNodes = trees[i].size;
        writeBEbytes(ofs,reinterpret_cast<char*>(&numNodes),sizeof(size_t));
        
        for(size_t j=0;j<numNodes;++j) {

            TreeNode n = trees[i].nodelist[j];
            
            writeBEbytes(ofs,reinterpret_cast<char*>(&(n.index)),sizeof(size_t));
            writeBEbytes(ofs,reinterpret_cast<char*>(&(n.isTerminal)),sizeof(char));
            writeBEbytes(ofs,reinterpret_cast<char*>(&(n.head)),sizeof(size_t));
            writeBEbytes(ofs,reinterpret_cast<char*>(&(n.parent)),sizeof(size_t));
            writeBEbytes(ofs,reinterpret_cast<char*>(&(n.sibling)),sizeof(size_t));
            writeBEbytes(ofs,reinterpret_cast<char*>(&(n.lexHead)),sizeof(size_t));
        }
        
        for(size_t j=0;j<numNodes;++j) {
            char mark = trees[i].markers[j];
            writeBEbytes(ofs,reinterpret_cast<char*>(&mark),sizeof(char));
        }
    }

    ofs.close();
    
}


double TreeChunker::logLikelihood() {
    double ll = 0.0;
    
    for(size_t i=0;i<ntrees;++i) {
        ParseTree& tree = trees[i];
        for(NodeOffset j=0;j<tree.size;++j) {
            if(tree.markers[j]) {
                Segment seg = Segment(&tree,j);
                double segScore = scoreDP(seg,1.0);
                ll += log(segScore);
            }
        }
    }
    
    return ll;
}

double TreeChunker::segmentationP(ParseTree* tree) {
    double ret = 1.0;
    for(NodeOffset j=0;j<tree->size;++j) {
        if(tree->markers[j]) {
            Segment seg = Segment(tree,j);
            double scr = scoreDP(seg,1.0);
            ret *= scr;
        }
    }
    return ret;
}
