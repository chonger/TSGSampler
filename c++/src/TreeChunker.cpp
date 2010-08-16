#include "TreeChunker.hpp"
#include <fstream>
#include <cmath>
#include <curses.h>

using namespace std;
//using namespace enbuske;

TreeChunker::TreeChunker(TreeData* tData_, std::ifstream& ifs, size_t nThreads_) :
    tData(tData_), nThreads(nThreads_) {

    stop = false;
    
    samples.resize(nThreads_);
    
    //JUST CHECKS

    totalLabels = samples.size();

    /**
    for(size_t i=0;i<tData->ntrees;++i) {
        ParseTree& pt = tData->trees[i];
        bool hasTag = false;
        for(size_t j=0;j<pt.size;++j) {

            if(pt.nodelist[j].type == 1)
                hasTag = true;
     
        }
        if(!hasTag) {
            printf("NO TAGS IN SENTENCE %d\n",i);
            throw "!";
        }
    }
    */

}

void TreeChunker::shuffleSamples() {

    //assume no more than 255 threads
    char indShuff[tData->ntrees];
    size_t factor = tData->ntrees / nThreads;
    for(size_t i=0;i<tData->ntrees;++i) {
        char ind = i/factor;
        if(ind >= (char)nThreads)
            ind = nThreads - 1;
        indShuff[i] = ind;
    }

    //shuffle indexes
    for(size_t j=1;j<tData->ntrees - 1;++j) {
        size_t ind = rand() % (tData->ntrees - j);
        size_t tmp = indShuff[ind];
        indShuff[ind] = indShuff[tData->ntrees - j];
        indShuff[tData->ntrees - j] = tmp;
    }

    for(size_t i=0;i<nThreads;++i) {
        samples[i].clear();
    }
    
    //build sample lists
    for(size_t i=0;i<tData->ntrees;++i) {
        ParseTree& pt = tData->trees[i];
            
        for(size_t j=0;j<pt.size;++j) {
            if(j > 0) //we never need to resample the root
                samples[indShuff[i]].push_back(make_pair(&(pt),j));            
        }
    }

    //shuffle each section
    for(size_t i=0;i<nThreads;++i) {
        std::vector<std::pair<ParseTree*,NodeOffset> >& curS = samples[i];

        for(size_t j=1;j<curS.size() - 1;++j) {
            size_t ind = rand() % (curS.size() - j);
            pair<ParseTree*,NodeOffset> tmp = curS[ind];
            curS[ind] = curS[curS.size() - j];
            curS[curS.size() - j] = tmp;
        }
        
    }

    for(size_t i=0;i<nThreads;++i) {
        threads[i]->samples = samples[i];
    }

    printf("SHUFFLED\n");
}

void TreeChunker::resample(size_t iterations) {

    dp->fromTreeData();
    
    for(size_t i=0;i<iterations;++i) {
        if(stop)
            return;

        clock_t start = clock();

        shuffleSamples();

        if(nThreads > 1) {
        
            //send off the threads
            for(size_t j=0;j<nThreads;++j) {
                threads[j]->go();
            }
            
            for(size_t j=0;j<nThreads;++j) {
                threads[j]->m_thread->join();
            }
            
            //reduce dps
            dp->fromTreeData();
            for(size_t j=0;j<nThreads;++j) {
                threads[j]->dp->treemap.clear();
            }
            for(TreeHashMap::iterator iter = dp->treemap.begin();
                iter != dp->treemap.end(); ++iter) {
                
                for(size_t j=0;j<nThreads;++j) {
                    threads[j]->dp->treemap.insert(std::make_pair(iter->first,iter->second));
                }
            }
        } else {

            printf("Sampling without threads, %d samples\n",samples[0].size());
            for(std::vector<std::pair<ParseTree*,NodeOffset> >::iterator iter = samples[0].begin();
                iter != samples[0].end(); ++iter) {
                threads[0]->sampleNode(iter->first,iter->second);
            }
            printf("DONEISH\n");
            dp->fromTreeData();
        }
        
        dp->resampleParams();
        
        double logl = dp->logLikelihood(tData->trees,tData->ntrees);

        clock_t finish = clock();
        printf("Iteration %d took %f seconds\n",i,(double(finish) - double(start))/CLOCKS_PER_SEC);
        printf("Log Likelihood           : %f\n",logl);
        //printf("Number of changed labels : %d out of %d\n",numChanged,totalLabels);
        
        outstream << logl << "\n";

        //documentation says that this "increases bucket count to at least 0
        //not really sure why we should use it though
        dp->treemap.resize(0);
    }
}

void ChunkThread::sampleNode(ParseTree* tree, NodeOffset offset) {
    
    TreeNode& node = tree->nodelist[offset];
    NodeOffset headOffset = offset - node.head;
    TreeNode& head = tree->nodelist[headOffset];
    
    size_t headLHS = tData->lhsMap[head.index];
    size_t myLHS = tData->lhsMap[node.index];
    size_t headLHSTotal = dp->lhsCounts[headLHS];
    size_t myLHSTotal = dp->lhsCounts[myLHS];
    double headAlpha = dp->alpha[headLHS];
    double myAlpha = dp->alpha[myLHS];
    
    bool wasCut = tree->markers[offset];
        
    tree->markers[offset] = true;
    
    Segment top = Segment(tree,headOffset);
    TreeHashMap::iterator topIter = dp->treemap.find(top);
    size_t topCount = 0;
    if(wasCut) { //top must be in the map
        topCount = topIter->second - 1;
    } else {

        if(topIter != dp->treemap.end())
            topCount = topIter->second;
        //else it stays at zero
    }
    std::pair<double,double> topScore = dp->bScore(top);
    
    Segment bottom = Segment(tree,offset);
    TreeHashMap::iterator bottomIter = dp->treemap.find(bottom);
    size_t bottomCount = 0;
    if(wasCut) { //bottom must be in the map
        bottomCount = bottomIter->second - 1;
    } else {

        if(bottomIter != dp->treemap.end())
            bottomCount = bottomIter->second;
        //else it stays at zero
    }

    
    std::pair<double,double> bottomScore = dp->bScore(bottom);
    
    size_t treeKronDel = top.equals(bottom) ? 1 : 0;
    size_t lhsKronDel = tData->lhsMap[head.index] == tData->lhsMap[node.index] ? 1 : 0;
    
    tree->markers[offset] = false;
    
    Segment join = Segment(tree,headOffset);

    TreeHashMap::iterator joinIter = dp->treemap.find(join);
    size_t joinCount = 0;
    if(!wasCut) { //join must be in the map
        joinCount = joinIter->second - 1;
    } else {

        if(joinIter != dp->treemap.end())
            joinCount = joinIter->second;
        //else it stays at zero
    }

    std::pair<double,double> joinScore = dp->bScore(join);
    
    //printf("smooth %E\n",smooth);
    double joinProb = (joinCount * joinScore.second + headAlpha * joinScore.first) / (headAlpha + headLHSTotal);    

    double topProb = (topCount * topScore.second + headAlpha * topScore.first) / (headAlpha + headLHSTotal);
    double bottomProb = (bottomScore.second * (treeKronDel + bottomCount) + myAlpha * bottomScore.first) /
        (lhsKronDel + myLHSTotal + myAlpha);

    //TODO : check if this goes to zero ever
    double scFac = topScore.second * bottomScore.second / joinScore.second;
    
    double combProb = topProb * bottomProb;    
    double cutoff = combProb / (joinProb * scFac + combProb);
    float randval = (rand() / ((float) RAND_MAX + 1));

    if(combProb == 0 && joinProb == 0) {

        //dont do anything...
        printf("ALL PROBS ARE ZERO\n");
        return;

        
        top.printMe();
        bottom.printMe();
        join.printMe();
        for(NodeOffset k=0;k<tree->size;++k) {
            printf("%d",tree->markers[k]);
        }
        printf("\n");
        for(NodeOffset k=0;k<tree->size;++k) {
            printf("%d",tree->nodelist[k].type);
        }
        printf("\n");
        printf("at index %d\n",offset);
        
        throw "!";
    }
    /**
    if(wasCut) {
        printf("JOIN %E TOP %E BOT %E\n",joinScore,topScore,bottomScore);
    }
    */
    
    //printf("CUTOFF at %f - join %E comb %E, randval %f\n",cutoff,joinProb,combProb,randval);

    //TOFDO _ REMOVE THESE DEBUG VARIABLES
    bool doCut = false;
    bool inserted = false;
    bool erTop = false;
    bool erBot = false;

    
    if(randval > cutoff) { //join
        doCut = false;
        //node is already marked as not cut from above in this method,             

        if(wasCut) {

            bool insertJoin = false;
            if(joinIter == dp->treemap.end()) {
                insertJoin = true;
            } else {
                joinIter->second += 1;

            }
            
            //topIter and bottomIter must have been in, checked above
                            
            if(topIter->second == 1) {
                erTop = true;
                dp->treemap.erase(topIter); //does not invalidate other iterators
            } else 
                topIter->second -= 1;

                
            if(bottomIter->second == 1) {
                erBot = true;
                dp->treemap.erase(bottomIter); //does not invalidate other iterators
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
                
            dp->lhsCounts[myLHS] -= 1;
            if(insertJoin) {
                dp->treemap.insert(make_pair(join,1));    //INSERT INVALIDATES
                inserted = true;
            }
        } 
    } else { //
        doCut = true;
        //the marker was set to false in this method, so set it true
        tree->markers[offset] = true;
        if(!wasCut) {
            /**
            bool ok = false;
            for(Segment::iterator iter = top.begin();iter != top.end();++iter) {
                if(iter.n->type != 0)
                    ok = true;
            }
            if(!ok) {
                printf("no tags in top\n");
                throw "!";
            }
            ok = false;
            for(Segment::iterator iter = bottom.begin();iter != bottom.end();++iter) {
                if(iter.n->type != 0)
                    ok = true;
            }
            if(!ok) {
                printf("no tags in bottom\n");
                printf("%E %E %E\n",topScore.first,bottomScore.first,joinScore.first);
                printf("%E %E %E\n",topScore.second,bottomScore.second,joinScore.second);
                printf("%E %E %E\n",randval,cutoff,scFac);

                top.printMe();
                bottom.printMe();
                join.printMe();
                throw "!";
            }
            */

            bool insertTop = false;
            bool insertBot = false;
            if(topIter == dp->treemap.end()) {
                insertTop = true;
            } else {
                topIter->second += 1;
            }
            if(bottomIter == dp->treemap.end()) {
                insertBot = true;
            } else {
                bottomIter->second += 1;
            }
            
            //join must be in the map, checked at the beginning of this method
            
            if(joinIter->second == 1) {
                dp->treemap.erase(joinIter); //does not invalidate other iterators
            } else 
                joinIter->second -= 1;
            
            dp->lhsCounts[myLHS] += 1;
                
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
                    dp->treemap.insert(make_pair(top,2)); //INSERT INVALIDATES
                    insertBot = false;
                } else 
                    dp->treemap.insert(make_pair(top,1)); //INSERT INVALIDATES
            }
                
            if(insertBot) {
                dp->treemap.insert(make_pair(bottom,1)); //INSERT INVALIDATES
            }
        }
    }    
}


void TreeChunker::packResults(const char* filename) {

    printf("Writing data to %s\n",filename);
    std::ofstream ofs(filename,std::ios::binary);
    
    tData->writeData(ofs);
    
    dp->write(ofs);    
    
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
        
        specDP->resampleParams();
        //backDP->resampleParams();
        
        //double logl = dp->logLikelihood(tData->trees,tData->ntrees);

        clock_t finish = clock();
        printf("Iteration %d took %f seconds\n",i,(double(finish) - double(start))/CLOCKS_PER_SEC);
        //printf("Log Likelihood           : %f\n",logl);
        //printf("Number of changed labels : %d out of %d\n",numChanged,totalLabels);
        
        //outstream << logl << "\n";
    }
}


size_t DoubleChunker::getCount(TreeHashMap::iterator& iter, DP* dp, bool inMap, bool minus) {
    size_t ret = 0;
    if(inMap) { //must be in the map just use iter
        ret = iter->second;
    } else {
        //could be in the map from some other tree
        if(iter != dp->treemap.end()) {
            ret = iter->second;
        }
        //else it stays at zero
    }
    if(minus)
        ret -= 1;
    return ret;
}

char DoubleChunker::sampleAspect(double p1, double p2) {
    double cutoff = p1 / (p1 + p2);
    float randval = (rand() / ((float) RAND_MAX + 1));
    char asp = 0;
    if(randval > cutoff)
        asp = 1;
    return asp;
}

void DoubleChunker::minusFromMap(TreeHashMap::iterator& iter, DP* dp, size_t head) {
    if(iter->second == 1) {
        dp->treemap.erase(iter); //does not invalidate other iterators
    } else 
        iter->second -= 1;
    dp->lhsCounts[head] -= 1;
}

bool DoubleChunker::addToMap(TreeHashMap::iterator& iter, DP* dp, size_t head) {
    bool ret = false;
    if(iter == dp->treemap.end()) {
        ret = true;
    } else {
        iter->second += 1;
    }
    dp->lhsCounts[head] += 1;
    return ret;
}

void DoubleChunker::sampleNode(ParseTree* tree, NodeOffset offset) {

    TreeNode& node = tree->nodelist[offset];
    NodeOffset headOffset = offset - node.head;
    TreeNode& head = tree->nodelist[headOffset];
    
    size_t headLHS = dblData->lhsMap[head.index];
    size_t curLHS = dblData->lhsMap[node.index];
    
    size_t backCurTotal = backDP->lhsCounts[curLHS];
    size_t specCurTotal = specDP->lhsCounts[curLHS];
    size_t backHeadTotal = backDP->lhsCounts[headLHS];
    size_t specHeadTotal = specDP->lhsCounts[headLHS];
    double backCurAlpha = backDP->alpha[curLHS];
    double specCurAlpha = specDP->alpha[curLHS];
    double backHeadAlpha = backDP->alpha[headLHS];
    double specHeadAlpha = specDP->alpha[headLHS];
    
    bool wasCut = tree->markers[offset];
    
    tree->markers[offset] = true; //pretend to split at the offset

    ///        GET TOP INFO
    Segment top = Segment(tree,headOffset);
    
    //get the iter, count, and P in the background for top
    TreeHashMap::iterator topBIter = backDP->treemap.find(top);
    size_t topBCount = getCount(topBIter,backDP,wasCut,(head.aspect == 0));
    std::pair<double,double> topBScore = backDP->bScore(top);
    
    //get the iter, count, and P in the specific for top
    TreeHashMap::iterator topSIter = specDP->treemap.find(top);
    size_t topSCount = getCount(topSIter,specDP,wasCut,(head.aspect == 1));
    std::pair<double,double> topSScore = specDP->bScore(top);


    ///        GET BOTTOM INFO
    Segment bottom = Segment(tree,offset);
    
    //get the iter, count, and P in the backG for bottom
    TreeHashMap::iterator bottomBIter = backDP->treemap.find(bottom);
    size_t bottomBCount = getCount(bottomBIter,backDP,wasCut,(node.aspect == 0));
    std::pair<double,double> bottomBScore = backDP->bScore(bottom);
        
    //get the iter, count, and P in the specific for bottom
    TreeHashMap::iterator bottomSIter = specDP->treemap.find(bottom);
    size_t bottomSCount = getCount(bottomSIter,specDP,wasCut,(node.aspect == 1));
    std::pair<double,double> bottomSScore = specDP->bScore(bottom);
    
    size_t treeKronDel = top.equals(bottom) ? 1 : 0;
    size_t lhsKronDel = dblData->lhsMap[head.index] == dblData->lhsMap[node.index] ? 1 : 0;
    
    tree->markers[offset] = false;

    ///         GET JOIN INFO
    Segment join = Segment(tree,headOffset);

    //get the iter, count, and P in the specific for join
    TreeHashMap::iterator joinBIter = backDP->treemap.find(join);
    size_t joinBCount = getCount(joinBIter,backDP,!wasCut,(head.aspect == 0));
    std::pair<double,double> joinBScore = backDP->bScore(join);
    
    //get the iter, count, and P in the specific for join
    TreeHashMap::iterator joinSIter = specDP->treemap.find(join);
    size_t joinSCount = getCount(joinSIter,specDP,!wasCut,(head.aspect == 1));
    std::pair<double,double> joinSScore = specDP->bScore(join);

    //we only need the count and score to get the total score and scale

    //we only need those scores and scales to choose the cut variable

    ///       GET DP PROBABILITIES FOR ALL TREES

    
    double backJ = (joinBCount * joinBScore.second + backHeadAlpha * joinBScore.first) / (backHeadAlpha + backHeadTotal);
    double specJ = (joinSCount * joinSScore.second + specHeadAlpha * joinSScore.first) / (specHeadAlpha + specHeadTotal);    

    double backT = (topBCount * topBScore.second + backHeadAlpha * topBScore.first) / (backHeadAlpha + backHeadTotal);
    double specT = (topSCount * topSScore.second + specHeadAlpha * topSScore.first) / (specHeadAlpha + specHeadTotal);
    
    double backB = (bottomBScore.second * (treeKronDel + bottomBCount) + backCurAlpha * bottomBScore.first) /
        (lhsKronDel + backCurTotal + backCurAlpha);
    double specB = (bottomSScore.second * (treeKronDel + bottomSCount) + specCurAlpha * bottomSScore.first) /
        (lhsKronDel + specCurTotal + specCurAlpha);

    //make all the scales the same
    backT *= topSScore.second / topBScore.second;
    backB *= bottomSScore.second / bottomBScore.second;
    backJ *= joinSScore.second / joinBScore.second;

    double scFac = topSScore.second * bottomSScore.second / joinSScore.second;

    double specProb = specInd[curLHS];

    double tProb = specProb * specT + (1 - specProb) * backT;
    double bProb = specProb * specB + (1 - specProb) * backB;
    double jProb = specProb * specJ + (1 - specProb) * backJ;

    double combProb = tProb * bProb;    
    double cutoff = combProb / (jProb * scFac + combProb);
    float randval = (rand() / ((float) RAND_MAX + 1));


    //Decide which one to insert into, based on a their probabilities in each DP
    //be carefull about things like the kronDeltas for identical assignments    
    
    if(randval > cutoff) { //join, dont cut
        
        //node is already marked as not cut from above in this method

        //sample the aspect for join
        char asp = sampleAspect(backJ,specJ);

        if(!wasCut && (asp == head.aspect))
            return; //we've stayed in the same state
        else if(!wasCut) { //we're switching aspects only
            //no need to remove top and bottom
            //remove from one aspect, insert in the other
            
            TreeHashMap::iterator& remIter = joinBIter;
            TreeHashMap::iterator& insIter = joinSIter;
            DP* remdp = backDP;
            DP* insdp = specDP;
            if(asp == 0) {
                remIter = joinSIter;
                insIter = joinBIter;
                remdp = specDP;
                insdp = backDP;
            }

            //which ever iter we're removing should be in the map
            minusFromMap(remIter,remdp,headLHS);
            bool insert = addToMap(insIter,insdp,headLHS);
            if(insert) {
                insdp->treemap.insert(make_pair(join,1));  
            }

            head.aspect = asp;
            
        } else {

            //now its because we're joining something that was already cut,
            //so remove the top and bottom and insert join

            //assume top and bottom and join are in bg to start
            TreeHashMap::iterator& remTIter = topBIter;
            TreeHashMap::iterator& remBIter = bottomBIter;
            TreeHashMap::iterator& insJIter = joinBIter;
            DP* remTdp = backDP;
            DP* remBdp = backDP;
            DP* insJdp = backDP;
            
            if(head.aspect == 1) { //top was in spec
                remTIter = topSIter;
                remTdp = specDP;
            }
            if(node.aspect == 1) { //bottom was in spec
                remBIter = bottomSIter;
                remBdp = specDP;
            }
            if(asp == 1) {
                insJIter = joinSIter;
                insJdp = specDP;
            }

            //remove top and bottom
            minusFromMap(remTIter,remTdp,headLHS);
            minusFromMap(remBIter,remBdp,curLHS);

            //insert join
            bool insert = addToMap(insJIter,insJdp,headLHS);
            if(insert)
                insJdp->treemap.insert(make_pair(join,1));  

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
        }
    } else { //
        //the marker was set to false in this method, so set it true
        tree->markers[offset] = true;

        char aspT = sampleAspect(backT,specT);
        char aspB = sampleAspect(backB,specB);

        if(wasCut) { //change tops aspect
            if(aspT != head.aspect) {
                TreeHashMap::iterator& remIter = topBIter;
                TreeHashMap::iterator& insIter = topSIter;
                DP* remdp = backDP;
                DP* insdp = specDP;
                if(aspT == 0) {
                    remIter = topSIter;
                    insIter = topBIter;
                    remdp = specDP;
                    insdp = backDP;
                }
                
                minusFromMap(remIter,remdp,headLHS);
                bool insert = addToMap(insIter,insdp,headLHS);
                if(insert)
                    insdp->treemap.insert(std::make_pair(top,1));
                head.aspect = aspT;
            }
            if(aspB != node.aspect) {
                TreeHashMap::iterator& remIter = bottomBIter;
                TreeHashMap::iterator& insIter = bottomSIter;
                DP* remdp = backDP;
                DP* insdp = specDP;
                if(aspB == 0) {
                    remIter = bottomSIter;
                    insIter = bottomBIter;
                    remdp = specDP;
                    insdp = backDP;
                }
            
                minusFromMap(remIter,remdp,curLHS);
                bool insert = addToMap(insIter,insdp,curLHS);
                if(insert)
                    insdp->treemap.insert(std::make_pair(top,1));
            
                node.aspect = aspB;
            }
        } else {
            //definitely removin join
            //also definitely inserting both top and bottom
            TreeHashMap::iterator& insTIter = topBIter;
            TreeHashMap::iterator& insBIter = bottomBIter;
            TreeHashMap::iterator& remJIter = joinBIter;
            DP* insTdp = backDP;
            DP* insBdp = backDP;
            DP* remJdp = backDP;
            
            if(aspT == 1) { 
                insTIter = topSIter;
                insTdp = specDP;
            }
            if(aspB == 1) { 
                insBIter = bottomSIter;
                insBdp = specDP;
            }
            if(head.aspect == 1) {
                remJIter = joinSIter;
                remJdp = specDP;
            }

            minusFromMap(remJIter,remJdp,headLHS);
            bool insertT = addToMap(insTIter,insTdp,headLHS);
            bool insertB = addToMap(insBIter,insBdp,curLHS);
            if(insertT) {
                if(insertB && top.equals(bottom) && aspT == aspB) {
                    insTdp->treemap.insert(std::make_pair(top,2));
                    insertB = false;
                } else
                    insTdp->treemap.insert(std::make_pair(top,1));
            }
            if(insertB)
                insBdp->treemap.insert(std::make_pair(bottom,1));

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
}


void DoubleChunker::packResults(const char* filename) {

    printf("Writing data to %s\n",filename);
    std::ofstream ofs(filename,std::ios::binary);
    
    dblData->write2Data(ofs);
    
    specDP->write(ofs);
    backDP->write(ofs);

    for(size_t i=0;i<dblData->nLHS;++i) {
        writeBEbytes(ofs,reinterpret_cast<char*>(specInd + i),sizeof(double));
    }
    
    ofs.close();
}
