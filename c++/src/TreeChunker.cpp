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
    if(wasCut) { //join must be in the map
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
    if(wasCut) { //join must be in the map
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

    writeBEbytes(ofs,reinterpret_cast<char*>(&(tData->numRules)),sizeof(size_t));

    //write pcfg probs
    for(size_t i=0;i<tData->numRules;++i) {
        writeBEbytes(ofs,reinterpret_cast<char*>(tData->pcfg + i),sizeof(double));
    }

    //write head indexes
    for(size_t i=0;i<tData->numRules;++i) {
        writeBEbytes(ofs,reinterpret_cast<char*>(tData->lhsMap + i),sizeof(size_t));
    }

    writeBEbytes(ofs,reinterpret_cast<char*>(&(tData->nLHS)),sizeof(size_t));



    
    writeBEbytes(ofs,reinterpret_cast<char*>(&(tData->ntrees)),sizeof(size_t));
    
    for(size_t i=0;i<tData->ntrees;++i) {
        size_t numNodes = tData->trees[i].size;
        writeBEbytes(ofs,reinterpret_cast<char*>(&numNodes),sizeof(size_t));
        
        for(size_t j=0;j<numNodes;++j) {

            TreeNode n = tData->trees[i].nodelist[j];
            
            writeBEbytes(ofs,reinterpret_cast<char*>(&(n.index)),sizeof(size_t));
            writeBEbytes(ofs,reinterpret_cast<char*>(&(n.isTerminal)),sizeof(char));
            writeBEbytes(ofs,reinterpret_cast<char*>(&(n.head)),sizeof(size_t));
            writeBEbytes(ofs,reinterpret_cast<char*>(&(n.parent)),sizeof(size_t));
            writeBEbytes(ofs,reinterpret_cast<char*>(&(n.sibling)),sizeof(size_t));
            writeBEbytes(ofs,reinterpret_cast<char*>(&(n.lexHead)),sizeof(size_t));
            writeBEbytes(ofs,reinterpret_cast<char*>(&(n.type)),sizeof(size_t));
        }
        
        for(size_t j=0;j<numNodes;++j) {
            char mark = tData->trees[i].markers[j];
            writeBEbytes(ofs,reinterpret_cast<char*>(&mark),sizeof(char));
        }
    }

    dp->write(ofs);    
    
    ofs.close();
}
